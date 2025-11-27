//! Procedure contract inference.
//!
//! This module provides automatic inference of procedure contracts from their
//! implementations, enabling workspace-wide inter-procedural taint analysis.

use std::collections::HashMap;
use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, Spanned};
use tower_lsp::lsp_types::{Position, Range};

use crate::symbol_path::SymbolPath;

// ═══════════════════════════════════════════════════════════════════════════
// Contract Types
// ═══════════════════════════════════════════════════════════════════════════

/// Describes whether a procedure validates its inputs.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum ValidationBehavior {
    /// Procedure does not validate inputs
    #[default]
    None,
    /// Procedure validates inputs (has u32assert* at start)
    ValidatesU32,
}

/// Stack effect of a procedure - how it changes the stack depth.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum StackEffect {
    /// Stack effect is unknown (contains control flow or calls to unknown procedures)
    #[default]
    Unknown,
    /// Known stack effect: (inputs_consumed, outputs_produced)
    /// Net change = outputs_produced - inputs_consumed
    Known { inputs: usize, outputs: usize },
}

/// Contract describing expected behavior of a procedure.
#[derive(Clone, Debug)]
pub struct ProcContract {
    /// Full path of the procedure
    pub path: SymbolPath,
    /// Whether the procedure validates its inputs
    pub validates: ValidationBehavior,
    /// Whether the procedure uses u32 operations (implies u32 input requirements)
    pub uses_u32_ops: bool,
    /// Whether the procedure reads from advice stack
    pub reads_advice: bool,
    /// Whether the procedure performs Merkle operations
    pub uses_merkle_ops: bool,
    /// Stack effect of this procedure
    pub stack_effect: StackEffect,
    /// Location of the procedure definition
    pub definition_range: Option<Range>,
}

impl ProcContract {
    /// Returns true if this procedure validates its inputs.
    pub fn validates_inputs(&self) -> bool {
        matches!(self.validates, ValidationBehavior::ValidatesU32)
    }

    /// Returns true if this procedure requires u32 inputs based on its operations.
    pub fn requires_u32_inputs(&self) -> bool {
        self.uses_u32_ops && !self.validates_inputs()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Contract Store
// ═══════════════════════════════════════════════════════════════════════════

/// Storage for procedure contracts across the workspace.
#[derive(Clone, Debug, Default)]
pub struct ContractStore {
    /// Inferred contracts by procedure path
    contracts: HashMap<SymbolPath, ProcContract>,
    /// Index by short name for fast lookups
    by_name: HashMap<String, Vec<SymbolPath>>,
}

impl ContractStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Update contracts for a document.
    pub fn update_document(&mut self, contracts: Vec<ProcContract>) {
        for contract in contracts {
            let path = contract.path.clone();
            let name = path.name().to_string();

            // Update name index - remove old entry first
            self.by_name
                .entry(name.clone())
                .or_default()
                .retain(|p| p != &path);
            self.by_name
                .entry(name)
                .or_default()
                .push(path.clone());

            self.contracts.insert(path, contract);
        }
    }

    /// Remove contracts for a document.
    pub fn remove_document(&mut self, paths: &[SymbolPath]) {
        for path in paths {
            self.contracts.remove(path);
            let name = path.name().to_string();
            if let Some(entries) = self.by_name.get_mut(&name) {
                entries.retain(|p| p != path);
            }
        }
    }

    /// Get contract by exact path.
    pub fn get(&self, path: &SymbolPath) -> Option<&ProcContract> {
        self.contracts.get(path)
    }

    /// Get contract by short name (last segment).
    pub fn get_by_name(&self, name: &str) -> Option<&ProcContract> {
        self.by_name
            .get(name)?
            .first()
            .and_then(|path| self.contracts.get(path))
    }

    /// Get contract by path suffix.
    pub fn get_by_suffix(&self, suffix: &str) -> Option<&ProcContract> {
        let name = suffix.rsplit("::").next()?;
        self.by_name
            .get(name)?
            .iter()
            .find(|p| p.ends_with(suffix))
            .and_then(|path| self.contracts.get(path))
    }

    /// Get all contracts (for iteration).
    pub fn iter(&self) -> impl Iterator<Item = (&SymbolPath, &ProcContract)> {
        self.contracts.iter()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Contract Inference
// ═══════════════════════════════════════════════════════════════════════════

/// Infer contracts for all procedures in a module.
///
/// This function performs iterative refinement: it first infers contracts without
/// transitive knowledge, then re-infers using the contracts from the first pass.
/// This allows procedures that call other procedures in the same module to get
/// accurate stack effects when those callees have known effects.
pub fn infer_module_contracts(
    module: &Module,
    source_manager: &DefaultSourceManager,
) -> Vec<ProcContract> {
    infer_module_contracts_with_store(module, source_manager, None)
}

/// Infer contracts for all procedures in a module, with access to existing contracts.
///
/// This is used during workspace-wide analysis where we may already have contracts
/// for procedures in other modules.
pub fn infer_module_contracts_with_store(
    module: &Module,
    source_manager: &DefaultSourceManager,
    existing_contracts: Option<&ContractStore>,
) -> Vec<ProcContract> {
    // First pass: collect procedure info and infer contracts
    // Using existing_contracts if provided
    let mut collector = ContractCollector {
        module,
        source_manager,
        contracts: existing_contracts,
        inferred: Vec::new(),
    };
    let _ = visit::visit_module(&mut collector, module);
    let first_pass = collector.inferred;

    // Build a temporary store with first-pass results
    let mut temp_store = existing_contracts.cloned().unwrap_or_default();
    temp_store.update_document(first_pass.clone());

    // Second pass: re-infer with knowledge of all contracts (including same-module)
    // This allows procedures calling other procedures to get transitive effects
    let mut second_pass = Vec::new();
    for contract in &first_pass {
        // Only re-infer if first pass had Unknown effect
        if matches!(contract.stack_effect, StackEffect::Unknown) {
            // Find the procedure in the module
            if let Some(proc) = find_procedure_by_path(module, &contract.path) {
                let re_inferred = infer_procedure_contract_with_store(
                    proc,
                    contract.path.clone(),
                    contract.definition_range.clone(),
                    Some(&temp_store),
                );
                second_pass.push(re_inferred);
            } else {
                second_pass.push(contract.clone());
            }
        } else {
            second_pass.push(contract.clone());
        }
    }

    second_pass
}

/// Find a procedure in a module by its path.
fn find_procedure_by_path<'a>(module: &'a Module, path: &SymbolPath) -> Option<&'a Procedure> {
    let name = path.name();
    module.procedures().find(|p| p.name().as_str() == name)
}

/// Visitor that collects contracts from all procedures in a module.
struct ContractCollector<'a> {
    module: &'a Module,
    source_manager: &'a DefaultSourceManager,
    /// Optional existing contracts for transitive effect inference
    contracts: Option<&'a ContractStore>,
    /// Contracts inferred during this pass
    inferred: Vec<ProcContract>,
}

impl<'a> Visit for ContractCollector<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> ControlFlow<()> {
        let path = SymbolPath::from_module_and_name(self.module, proc.name().as_str());
        let definition_range = span_to_range(self.source_manager, proc.name().span());

        let contract =
            infer_procedure_contract_with_store(proc, path, definition_range, self.contracts);
        self.inferred.push(contract);

        // Don't descend into procedure body for contract collection
        ControlFlow::Continue(())
    }
}

/// Infer a contract with access to existing contracts for transitive effects.
fn infer_procedure_contract_with_store(
    proc: &Procedure,
    path: SymbolPath,
    definition_range: Option<Range>,
    contracts: Option<&ContractStore>,
) -> ProcContract {
    let mut analyzer = ProcedureAnalyzer::new(contracts);
    let _ = visit::visit_procedure(&mut analyzer, proc);

    ProcContract {
        path,
        validates: analyzer.validation_behavior(),
        uses_u32_ops: analyzer.uses_u32_ops,
        reads_advice: analyzer.reads_advice,
        uses_merkle_ops: analyzer.uses_merkle_ops,
        stack_effect: analyzer.stack_effect(),
        definition_range,
    }
}

/// Analyzer that walks a procedure to infer its contract.
struct ProcedureAnalyzer<'a> {
    /// Optional contract store for looking up transitive effects
    contracts: Option<&'a ContractStore>,
    /// Whether we've seen validation instructions at the start
    early_validation: bool,
    /// Number of validation instructions at the start
    validation_count: usize,
    /// Whether we've seen any non-validation instruction
    seen_non_validation: bool,
    /// Whether the procedure uses u32 operations
    uses_u32_ops: bool,
    /// Whether the procedure reads from advice
    reads_advice: bool,
    /// Whether the procedure uses Merkle operations
    uses_merkle_ops: bool,
    /// Current stack depth relative to start (can be negative)
    stack_depth: i32,
    /// Minimum stack depth reached (negative = inputs consumed)
    min_depth: i32,
    /// Whether stack effect is unknown (due to control flow or unknown calls)
    unknown_effect: bool,
}

impl<'a> ProcedureAnalyzer<'a> {
    fn new(contracts: Option<&'a ContractStore>) -> Self {
        Self {
            contracts,
            early_validation: false,
            validation_count: 0,
            seen_non_validation: false,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_depth: 0,
            min_depth: 0,
            unknown_effect: false,
        }
    }
}

impl<'a> ProcedureAnalyzer<'a> {
    fn validation_behavior(&self) -> ValidationBehavior {
        if self.early_validation && self.validation_count > 0 {
            ValidationBehavior::ValidatesU32
        } else {
            ValidationBehavior::None
        }
    }

    fn stack_effect(&self) -> StackEffect {
        if self.unknown_effect {
            StackEffect::Unknown
        } else {
            // inputs = how far below zero we went (min_depth is negative or zero)
            let inputs = (-self.min_depth).max(0) as usize;
            // outputs = final depth + inputs consumed
            let outputs = (self.stack_depth - self.min_depth) as usize;
            StackEffect::Known { inputs, outputs }
        }
    }

    /// Apply a stack depth change: negative = pop, positive = push
    fn apply_stack_change(&mut self, change: i32) {
        self.stack_depth += change;
        if self.stack_depth < self.min_depth {
            self.min_depth = self.stack_depth;
        }
    }

    /// Apply a pop then push operation (e.g., add pops 2, pushes 1).
    /// This correctly tracks min_depth for operations that consume stack values.
    fn apply_pop_push(&mut self, pops: i32, pushes: i32) {
        // First apply the pops to track min_depth
        self.stack_depth -= pops;
        if self.stack_depth < self.min_depth {
            self.min_depth = self.stack_depth;
        }
        // Then apply the pushes
        self.stack_depth += pushes;
    }

    /// Mark that the operation needs N values on the stack but doesn't change depth.
    /// This is used for swap-like operations.
    fn require_stack_depth(&mut self, needed: i32) {
        // Temporarily decrease depth to mark the requirement
        let current = self.stack_depth;
        self.stack_depth -= needed;
        if self.stack_depth < self.min_depth {
            self.min_depth = self.stack_depth;
        }
        // Restore depth (operation doesn't change net depth)
        self.stack_depth = current;
    }

    /// Compute stack effect of a block without modifying this analyzer's state.
    fn compute_block_effect(&self, block: &Block) -> Option<(i32, i32)> {
        let mut sub_analyzer = ProcedureAnalyzer::new(self.contracts);
        let _ = visit::visit_block(&mut sub_analyzer, block);

        if sub_analyzer.unknown_effect {
            None
        } else {
            // Return (inputs_consumed, outputs_produced)
            let inputs = (-sub_analyzer.min_depth).max(0);
            let outputs = sub_analyzer.stack_depth - sub_analyzer.min_depth;
            Some((inputs, outputs))
        }
    }

    /// Handle procedure call - apply transitive stack effect if known.
    fn handle_procedure_call(&mut self, target: &InvocationTarget) {
        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str(),
            InvocationTarget::Path(path) => path.inner().as_str(),
            InvocationTarget::MastRoot(_) => {
                // MAST root calls have unknown effect
                self.unknown_effect = true;
                return;
            }
        };

        // Look up the contract for this procedure
        let stack_effect = self.contracts.and_then(|store| {
            store
                .get_by_suffix(target_name)
                .or_else(|| store.get_by_name(target_name))
                .map(|c| &c.stack_effect)
        });

        match stack_effect {
            Some(StackEffect::Known { inputs, outputs }) => {
                // Known effect: apply it
                self.apply_stack_change(-(*inputs as i32));
                self.apply_stack_change(*outputs as i32);
            }
            Some(StackEffect::Unknown) | None => {
                // Unknown effect - mark as unknown
                self.unknown_effect = true;
            }
        }
    }

    fn analyze_instruction(&mut self, inst: &Instruction) {
        use miden_assembly_syntax::ast::Immediate;

        let inst_str = inst.to_string();

        // Check for validation instructions
        let is_validation = inst_str.starts_with("u32assert");

        if is_validation && !self.seen_non_validation {
            self.early_validation = true;
            self.validation_count += 1;
        } else if !is_validation {
            self.seen_non_validation = true;
        }

        // Check for u32 operations (excluding assertions and tests)
        if inst_str.starts_with("u32")
            && !inst_str.starts_with("u32assert")
            && !inst_str.starts_with("u32test")
        {
            self.uses_u32_ops = true;
        }

        // Check for advice operations
        match inst {
            Instruction::AdvPush(_) | Instruction::AdvLoadW | Instruction::AdvPipe => {
                self.reads_advice = true;
            }
            _ => {}
        }

        // Check for Merkle operations
        match inst {
            Instruction::MTreeGet
            | Instruction::MTreeSet
            | Instruction::MTreeMerge
            | Instruction::MTreeVerify
            | Instruction::MTreeVerifyWithError(_) => {
                self.uses_merkle_ops = true;
            }
            _ => {}
        }

        // Track stack effects
        match inst {
            // Procedure calls - handled via handle_procedure_call with transitive effects
            Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
                self.handle_procedure_call(target);
            }

            // Push operations
            Instruction::Push(_) => self.apply_stack_change(1),
            Instruction::PushFeltList(values) => self.apply_stack_change(values.len() as i32),
            Instruction::AdvPush(n) => {
                let count = match n {
                    Immediate::Value(v) => v.into_inner() as i32,
                    _ => 1,
                };
                self.apply_stack_change(count);
            }
            Instruction::AdvLoadW => self.apply_stack_change(0), // pops 4, pushes 4
            Instruction::AdvPipe => self.apply_stack_change(0),   // pops 8, pushes 8

            // Pop/drop operations
            Instruction::Drop => self.apply_stack_change(-1),
            Instruction::DropW => self.apply_stack_change(-4),

            // Dup operations (push 1)
            Instruction::Dup0 | Instruction::Dup1 | Instruction::Dup2 | Instruction::Dup3
            | Instruction::Dup4 | Instruction::Dup5 | Instruction::Dup6 | Instruction::Dup7
            | Instruction::Dup8 | Instruction::Dup9 | Instruction::Dup10 | Instruction::Dup11
            | Instruction::Dup12 | Instruction::Dup13 | Instruction::Dup14 | Instruction::Dup15 => {
                self.apply_stack_change(1);
            }
            Instruction::DupW0 | Instruction::DupW1 | Instruction::DupW2 | Instruction::DupW3 => {
                self.apply_stack_change(4);
            }

            // Swap/move operations (no net change, but need elements on stack)
            Instruction::Swap1 => self.require_stack_depth(2),
            Instruction::Swap2 | Instruction::Swap3 | Instruction::Swap4
            | Instruction::Swap5 | Instruction::Swap6 | Instruction::Swap7 | Instruction::Swap8
            | Instruction::Swap9 | Instruction::Swap10 | Instruction::Swap11 | Instruction::Swap12
            | Instruction::Swap13 | Instruction::Swap14 | Instruction::Swap15 => {
                // SwapN needs N+1 elements on stack
                let inst_str = inst.to_string();
                if let Some(n) = inst_str.strip_prefix("swap.") {
                    if let Ok(n) = n.parse::<i32>() {
                        self.require_stack_depth(n + 1);
                    }
                }
            }

            Instruction::MovUp2 => self.require_stack_depth(3),
            Instruction::MovUp3 => self.require_stack_depth(4),
            Instruction::MovUp4 => self.require_stack_depth(5),
            Instruction::MovUp5 => self.require_stack_depth(6),
            Instruction::MovUp6 => self.require_stack_depth(7),
            Instruction::MovUp7 => self.require_stack_depth(8),
            Instruction::MovUp8 => self.require_stack_depth(9),
            Instruction::MovUp9 => self.require_stack_depth(10),
            Instruction::MovUp10 => self.require_stack_depth(11),
            Instruction::MovUp11 => self.require_stack_depth(12),
            Instruction::MovUp12 => self.require_stack_depth(13),
            Instruction::MovUp13 => self.require_stack_depth(14),
            Instruction::MovUp14 => self.require_stack_depth(15),
            Instruction::MovUp15 => self.require_stack_depth(16),

            Instruction::MovDn2 => self.require_stack_depth(3),
            Instruction::MovDn3 => self.require_stack_depth(4),
            Instruction::MovDn4 => self.require_stack_depth(5),
            Instruction::MovDn5 => self.require_stack_depth(6),
            Instruction::MovDn6 => self.require_stack_depth(7),
            Instruction::MovDn7 => self.require_stack_depth(8),
            Instruction::MovDn8 => self.require_stack_depth(9),
            Instruction::MovDn9 => self.require_stack_depth(10),
            Instruction::MovDn10 => self.require_stack_depth(11),
            Instruction::MovDn11 => self.require_stack_depth(12),
            Instruction::MovDn12 => self.require_stack_depth(13),
            Instruction::MovDn13 => self.require_stack_depth(14),
            Instruction::MovDn14 => self.require_stack_depth(15),
            Instruction::MovDn15 => self.require_stack_depth(16),

            // Pad operations
            Instruction::PadW => self.apply_stack_change(4),

            // Binary arithmetic: pop 2, push 1
            Instruction::Add | Instruction::Sub
            | Instruction::Mul | Instruction::Div
            | Instruction::Eq | Instruction::Neq
            | Instruction::Lt | Instruction::Lte | Instruction::Gt | Instruction::Gte
            | Instruction::And | Instruction::Or | Instruction::Xor => {
                self.apply_pop_push(2, 1);
            }

            // Immediate arithmetic: pop 1, push 1 (operand from immediate)
            Instruction::AddImm(_) | Instruction::SubImm(_)
            | Instruction::MulImm(_) | Instruction::DivImm(_)
            | Instruction::EqImm(_) | Instruction::NeqImm(_) => {
                self.apply_pop_push(1, 1);
            }

            // Unary operations: pop 1, push 1 (no change but need 1 input)
            Instruction::Neg | Instruction::Inv | Instruction::Not | Instruction::IsOdd => {
                self.require_stack_depth(1);
            }

            // Memory operations
            Instruction::MemLoad | Instruction::MemLoadImm(_) => {
                // pop addr, push value: no net change
            }
            Instruction::MemStore | Instruction::MemStoreImm(_) => {
                self.apply_stack_change(-2); // pop addr + value
            }

            // Merkle operations - complex, mark as unknown for safety
            Instruction::MTreeGet | Instruction::MTreeSet | Instruction::MTreeMerge
            | Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
                // These have complex stack effects, mark unknown
                self.unknown_effect = true;
            }

            // u32 operations: most are binary (pop 2, push 1)
            _ if inst_str.starts_with("u32") && !inst_str.starts_with("u32assert") => {
                self.apply_stack_change(-1);
            }

            // Default: no change (conservative for unknown instructions)
            _ => {}
        }
    }
}

impl<'a> Visit for ProcedureAnalyzer<'a> {
    fn visit_op(&mut self, op: &Op) -> ControlFlow<()> {
        match op {
            Op::Inst(inst) => {
                self.analyze_instruction(inst.inner());
            }
            Op::If {
                then_blk, else_blk, ..
            } => {
                // Compute effects of both branches
                let then_effect = self.compute_block_effect(then_blk);
                let else_effect = self.compute_block_effect(else_blk);

                match (then_effect, else_effect) {
                    (Some((then_in, then_out)), Some((else_in, else_out))) => {
                        // Both branches have known effects
                        let then_net = then_out - then_in;
                        let else_net = else_out - else_in;

                        if then_net == else_net {
                            // Same net effect - we can compute the combined effect
                            // The inputs required is the max of both branches
                            // (since we don't know which branch will execute)
                            let max_inputs = then_in.max(else_in);
                            // Adjust outputs based on max inputs
                            let outputs = max_inputs + then_net;
                            self.apply_stack_change(-max_inputs);
                            self.apply_stack_change(outputs);
                        } else {
                            // Different net effects - unknown
                            self.unknown_effect = true;
                        }
                    }
                    _ => {
                        // At least one branch has unknown effect
                        self.unknown_effect = true;
                    }
                }
                // Don't recurse - we've already computed the effects
                return ControlFlow::Continue(());
            }
            Op::While { body, .. } => {
                // While loops have known effect only if the body has zero net change
                if let Some((body_in, body_out)) = self.compute_block_effect(body) {
                    let net_change = body_out - body_in;
                    if net_change == 0 {
                        // Zero net change per iteration - the loop needs body_in inputs
                        // to start, and produces body_out outputs when it terminates.
                        // Since net is 0, inputs = outputs.
                        self.apply_pop_push(body_in, body_out);
                    } else {
                        // Non-zero net change with unknown iteration count
                        self.unknown_effect = true;
                    }
                } else {
                    self.unknown_effect = true;
                }
                // Don't recurse - we've already computed the effects
                return ControlFlow::Continue(());
            }
            Op::Repeat { count, body, .. } => {
                // Repeat has known count, so we can compute exact effect
                if let Some((body_in, body_out)) = self.compute_block_effect(body) {
                    let count = *count as i32;
                    // Each iteration: consumes body_in, produces body_out
                    // Net change per iteration: body_out - body_in
                    // Total net change: count * (body_out - body_in)
                    let net_per_iter = body_out - body_in;
                    let total_net = count * net_per_iter;

                    // For inputs: first iteration needs body_in inputs
                    // Subsequent iterations use previous outputs
                    // Track conservatively: total inputs = body_in (first iter consumes from original stack)
                    self.apply_stack_change(-body_in);
                    self.apply_stack_change(body_in + total_net);
                } else {
                    self.unknown_effect = true;
                }
                // Don't recurse - we've already computed the effects
                return ControlFlow::Continue(());
            }
        }
        // Default traversal for Op::Inst
        visit::visit_op(self, op)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════

fn span_to_range(
    source_manager: &DefaultSourceManager,
    span: miden_debug_types::SourceSpan,
) -> Option<Range> {
    let byte_range = span.into_range();
    let start = source_manager
        .file_line_col(miden_debug_types::SourceSpan::at(
            span.source_id(),
            byte_range.start,
        ))
        .ok()?;
    let end = source_manager
        .file_line_col(miden_debug_types::SourceSpan::at(
            span.source_id(),
            byte_range.end,
        ))
        .ok()?;

    Some(Range::new(
        Position::new(
            start.line.to_usize().saturating_sub(1) as u32,
            start.column.to_usize().saturating_sub(1) as u32,
        ),
        Position::new(
            end.line.to_usize().saturating_sub(1) as u32,
            end.column.to_usize().saturating_sub(1) as u32,
        ),
    ))
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contract_store_basic() {
        let mut store = ContractStore::new();
        let path = SymbolPath::new("::test::my_proc");
        let contract = ProcContract {
            path: path.clone(),
            validates: ValidationBehavior::ValidatesU32,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        store.update_document(vec![contract]);

        assert!(store.get(&path).is_some());
        assert!(store.get_by_name("my_proc").is_some());
    }

    #[test]
    fn test_contract_store_by_suffix() {
        let mut store = ContractStore::new();
        let path = SymbolPath::new("::std::math::u64::add");
        let contract = ProcContract {
            path: path.clone(),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        store.update_document(vec![contract]);

        assert!(store.get_by_suffix("u64::add").is_some());
        assert!(store.get_by_suffix("add").is_some());
        assert!(store.get_by_suffix("math::u64::add").is_some());
    }

    #[test]
    fn test_validation_behavior() {
        let contract = ProcContract {
            path: SymbolPath::new("::test::validated"),
            validates: ValidationBehavior::ValidatesU32,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        assert!(contract.validates_inputs());
        assert!(!contract.requires_u32_inputs()); // Validates, so doesn't "require" external validation

        let contract2 = ProcContract {
            path: SymbolPath::new("::test::unvalidated"),
            validates: ValidationBehavior::None,
            uses_u32_ops: true,
            reads_advice: false,
            uses_merkle_ops: false,
            stack_effect: StackEffect::Unknown,
            definition_range: None,
        };

        assert!(!contract2.validates_inputs());
        assert!(contract2.requires_u32_inputs()); // Uses u32 but doesn't validate
    }

    // Helper to parse MASM and infer contracts
    fn parse_and_infer(source: &str) -> Vec<ProcContract> {
        use miden_assembly_syntax::ast::ModuleKind;
        use miden_assembly_syntax::{Parse, ParseOptions};
        use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};

        let source_manager = DefaultSourceManager::default();
        let uri = miden_debug_types::Uri::from("test://test.masm");
        source_manager.load(SourceLanguage::Masm, uri.clone(), source.to_string());

        let source_file = source_manager.get_by_uri(&uri).expect("Failed to load source");
        let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
        module_path.push("test");
        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: Some(module_path.into()),
            ..Default::default()
        };
        let module = source_file
            .parse_with_options(&source_manager, opts)
            .expect("Failed to parse MASM");
        infer_module_contracts(&module, &source_manager)
    }

    #[test]
    fn test_simple_proc_known_stack_effect() {
        // A procedure that pushes 3 values: net effect is 0 inputs, 3 outputs
        let contracts = parse_and_infer(
            "proc push_three
    push.1 push.2 push.3
end",
        );

        assert_eq!(contracts.len(), 1);
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 3
            }
        );
    }

    #[test]
    fn test_proc_with_inputs_known_stack_effect() {
        // A procedure that adds two values (pops 2, pushes 1): net effect is 2 inputs, 1 output
        let contracts = parse_and_infer(
            "proc add_values
    add
end",
        );

        assert_eq!(contracts.len(), 1);
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 2,
                outputs: 1
            }
        );
    }

    #[test]
    fn test_repeat_known_count_stack_effect() {
        // repeat.3 with body that pushes 1 value
        // Each iteration: 0 inputs, 1 output (net +1)
        // Total: 0 inputs, 3 outputs
        let contracts = parse_and_infer(
            "proc repeat_push
    repeat.3
        push.1
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 3
            }
        );
    }

    #[test]
    fn test_repeat_zero_net_effect() {
        // repeat.5 with body that pops and pushes (swap): net 0
        // Total: still 0 net change, but needs 2 inputs
        let contracts = parse_and_infer(
            "proc repeat_swap
    repeat.5
        swap
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // Swap needs 2 inputs, produces 2 outputs, repeated 5 times = still 2 in, 2 out
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 2,
                outputs: 2
            }
        );
    }

    #[test]
    fn test_if_else_matching_effects() {
        // if-else where both branches have same net effect (push 1)
        let contracts = parse_and_infer(
            "proc if_push
    if.true
        push.1
    else
        push.2
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // Both branches push 1, so net effect is 0 inputs, 1 output
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 1
            }
        );
    }

    #[test]
    fn test_if_else_different_effects_unknown() {
        // if-else where branches have different net effects
        let contracts = parse_and_infer(
            "proc if_unbalanced
    if.true
        push.1
    else
        push.1 push.2
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // Different net effects: then=+1, else=+2 -> unknown
        assert_eq!(contracts[0].stack_effect, StackEffect::Unknown);
    }

    #[test]
    fn test_while_zero_net_effect() {
        // while loop with zero net effect (swap each iteration)
        let contracts = parse_and_infer(
            "proc while_swap
    while.true
        swap
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // While loop with zero net body effect = known zero overall effect
        // But it still needs the initial 2 inputs for swap
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 2,
                outputs: 2
            }
        );
    }

    #[test]
    fn test_while_nonzero_net_effect_unknown() {
        // while loop with non-zero net effect = unknown
        let contracts = parse_and_infer(
            "proc while_push
    while.true
        push.1
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // Non-zero net effect in while loop = unknown
        assert_eq!(contracts[0].stack_effect, StackEffect::Unknown);
    }

    #[test]
    fn test_transitive_proc_call() {
        // Two procedures: helper has known effect, caller uses it
        let contracts = parse_and_infer(
            "proc helper
    push.1 push.2
end

proc caller
    exec.helper
    add
end",
        );

        assert_eq!(contracts.len(), 2);

        // Helper: 0 inputs, 2 outputs
        let helper = contracts.iter().find(|c| c.path.name() == "helper").unwrap();
        assert_eq!(
            helper.stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 2
            }
        );

        // Caller: exec.helper pushes 2, add pops 2 pushes 1 = 0 inputs, 1 output
        let caller = contracts.iter().find(|c| c.path.name() == "caller").unwrap();
        assert_eq!(
            caller.stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 1
            }
        );
    }
}
