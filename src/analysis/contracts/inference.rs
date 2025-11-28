//! Contract inference from procedure implementations.
//!
//! This module provides automatic inference of procedure contracts by analyzing
//! procedure bodies. It detects validation patterns, u32 operations, advice reads,
//! Merkle operations, and computes stack effects.

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, Spanned};
use tower_lsp::lsp_types::{Position, Range};

use crate::symbol_path::SymbolPath;

use super::store::ContractStore;
use super::types::{ProcContract, StackEffect, ValidationBehavior};
use crate::analysis::stack_ops::static_effect;
use crate::analysis::types::Bounds;
use crate::analysis::utils::push_imm_to_u64;
use crate::analysis::while_loops::infer_while_bound;

// ═══════════════════════════════════════════════════════════════════════════
// Module-Level Inference
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
        // Re-infer if first pass had incomplete effect (Unknown or KnownInputs)
        // These may now have fully known effects after looking up their callees
        let needs_reinference = matches!(
            contract.stack_effect,
            StackEffect::Unknown | StackEffect::KnownInputs { .. }
        );
        if needs_reinference {
            // Find the procedure in the module
            if let Some(proc) = find_procedure_by_path(module, &contract.path) {
                let re_inferred = infer_procedure_contract_with_store(
                    proc,
                    contract.path.clone(),
                    contract.definition_range,
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

// ═══════════════════════════════════════════════════════════════════════════
// Contract Collector Visitor
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
// Procedure Analyzer
// ═══════════════════════════════════════════════════════════════════════════

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
    /// Bounds tracking for top stack elements (for while loop analysis)
    bounds_stack: Vec<Bounds>,
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
            bounds_stack: Vec::new(),
        }
    }

    fn validation_behavior(&self) -> ValidationBehavior {
        if self.early_validation && self.validation_count > 0 {
            ValidationBehavior::ValidatesU32
        } else {
            ValidationBehavior::None
        }
    }

    fn stack_effect(&self) -> StackEffect {
        // inputs = how far below zero we went (min_depth is negative or zero)
        let inputs = (-self.min_depth).max(0) as usize;

        if self.unknown_effect {
            // Even when outputs are unknown, we still know the minimum inputs required
            StackEffect::KnownInputs { inputs }
        } else {
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

    // ═══════════════════════════════════════════════════════════════════════
    // Bounds stack helpers
    // ═══════════════════════════════════════════════════════════════════════

    /// Push a bounds value onto the bounds stack.
    fn push_bounds(&mut self, bounds: Bounds) {
        self.bounds_stack.push(bounds);
    }

    /// Pop a bounds value from the bounds stack.
    fn pop_bounds(&mut self) -> Bounds {
        self.bounds_stack.pop().unwrap_or(Bounds::Field)
    }

    /// Get the initial counter value if the top of bounds stack is a constant.
    fn get_top_const(&self) -> Option<u64> {
        self.bounds_stack.last().and_then(|b| b.as_const())
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
            Some(StackEffect::KnownInputs { inputs }) => {
                // We know inputs but not outputs - apply inputs, mark outputs unknown
                self.apply_stack_change(-(*inputs as i32));
                self.unknown_effect = true;
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

        // Track stack effects - handle special cases first, then use static_effect()
        match inst {
            // Procedure calls - handled via handle_procedure_call with transitive effects
            Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
                self.handle_procedure_call(target);
                return;
            }

            // Dynamic procedure calls - mark as unknown
            Instruction::DynExec | Instruction::DynCall => {
                self.apply_pop_push(4, 0);
                self.unknown_effect = true;
                return;
            }

            // STARK/complex operations - mark as unknown
            Instruction::FriExt2Fold4 | Instruction::HornerBase | Instruction::HornerExt |
            Instruction::EvalCircuit | Instruction::LogPrecompile | Instruction::SysEvent(_) => {
                self.unknown_effect = true;
                return;
            }

            // Push operations - need to track bounds for while loop analysis
            Instruction::Push(imm) => {
                self.apply_stack_change(1);
                let bounds = push_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.push_bounds(bounds);
                return;
            }
            Instruction::PushFeltList(values) => {
                self.apply_stack_change(values.len() as i32);
                for _ in values {
                    self.push_bounds(Bounds::Field);
                }
                return;
            }
            Instruction::AdvPush(n) => {
                let count = match n {
                    Immediate::Value(v) => v.into_inner() as i32,
                    _ => 1,
                };
                self.apply_stack_change(count);
                for _ in 0..count {
                    self.push_bounds(Bounds::Field);
                }
                return;
            }
            Instruction::AdvLoadW => {
                // pops 4, pushes 4 - track bounds
                for _ in 0..4 {
                    self.pop_bounds();
                }
                for _ in 0..4 {
                    self.push_bounds(Bounds::Field);
                }
                // Fall through to use static_effect for stack tracking
            }
            Instruction::AdvPipe => {
                // pops 8, pushes 8 - track bounds
                for _ in 0..8 {
                    self.pop_bounds();
                }
                for _ in 0..8 {
                    self.push_bounds(Bounds::Field);
                }
                // Fall through to use static_effect for stack tracking
            }

            // Drop operations - need to pop bounds
            Instruction::Drop => {
                self.apply_stack_change(-1);
                self.pop_bounds();
                return;
            }
            Instruction::DropW => {
                self.apply_stack_change(-4);
                for _ in 0..4 {
                    self.pop_bounds();
                }
                return;
            }

            // All other instructions - use static_effect()
            _ => {}
        }

        // Use static_effect() for all remaining instructions
        if let Some(effect) = static_effect(inst) {
            let pops = effect.pops as i32;
            let pushes = effect.pushes as i32;

            if pops == 0 && pushes == 0 {
                // No net effect, but might need stack depth for operations like swap
                // Calculate required depth from instruction type
                let required = self.get_required_depth(inst);
                if required > 0 {
                    self.require_stack_depth(required);
                }
            } else {
                self.apply_pop_push(pops, pushes);
            }
        }
        // If static_effect returns None, it's a dynamic instruction we should have handled above
    }

    /// Get the required stack depth for swap/move operations that don't change stack size.
    fn get_required_depth(&self, inst: &Instruction) -> i32 {
        match inst {
            Instruction::Swap1 => 2,
            Instruction::Swap2 => 3,
            Instruction::Swap3 => 4,
            Instruction::Swap4 => 5,
            Instruction::Swap5 => 6,
            Instruction::Swap6 => 7,
            Instruction::Swap7 => 8,
            Instruction::Swap8 => 9,
            Instruction::Swap9 => 10,
            Instruction::Swap10 => 11,
            Instruction::Swap11 => 12,
            Instruction::Swap12 => 13,
            Instruction::Swap13 => 14,
            Instruction::Swap14 => 15,
            Instruction::Swap15 => 16,
            Instruction::MovUp2 | Instruction::MovDn2 => 3,
            Instruction::MovUp3 | Instruction::MovDn3 => 4,
            Instruction::MovUp4 | Instruction::MovDn4 => 5,
            Instruction::MovUp5 | Instruction::MovDn5 => 6,
            Instruction::MovUp6 | Instruction::MovDn6 => 7,
            Instruction::MovUp7 | Instruction::MovDn7 => 8,
            Instruction::MovUp8 | Instruction::MovDn8 => 9,
            Instruction::MovUp9 | Instruction::MovDn9 => 10,
            Instruction::MovUp10 | Instruction::MovDn10 => 11,
            Instruction::MovUp11 | Instruction::MovDn11 => 12,
            Instruction::MovUp12 | Instruction::MovDn12 => 13,
            Instruction::MovUp13 | Instruction::MovDn13 => 14,
            Instruction::MovUp14 | Instruction::MovDn14 => 15,
            Instruction::MovUp15 | Instruction::MovDn15 => 16,
            Instruction::SwapW1 => 8,
            Instruction::SwapW2 | Instruction::MovUpW2 | Instruction::MovDnW2 => 12,
            Instruction::SwapW3 | Instruction::SwapDw | Instruction::MovUpW3 | Instruction::MovDnW3 => 16,
            Instruction::Reversew => 4,
            Instruction::Reversedw => 8,
            _ => 0,
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
                // if.true consumes 1 element (the condition) from the stack
                self.apply_pop_push(1, 0);

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
                            // Different net effects - unknown outputs
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
                // While loops have known effect if:
                // 1. Body has zero net change, OR
                // 2. We can infer the iteration count from a counter pattern
                if let Some((body_in, body_out)) = self.compute_block_effect(body) {
                    let net_change = body_out - body_in;
                    if net_change == 0 {
                        // Zero net change per iteration - the loop needs body_in inputs
                        // to start, and produces body_out outputs when it terminates.
                        // Since net is 0, inputs = outputs.
                        self.apply_pop_push(body_in, body_out);
                    } else {
                        // Non-zero net change - try to infer iteration count
                        // The counter should be on top of the bounds stack
                        let initial_counter = self.get_top_const();
                        let loop_bound = infer_while_bound(body, initial_counter);

                        if let Some(iterations) = loop_bound.count() {
                            // Known iteration count - compute total effect
                            let total_net = (iterations as i32) * net_change;
                            // First iteration consumes body_in from original stack
                            // Total effect: body_in inputs consumed, (body_in + total_net) outputs
                            self.apply_stack_change(-body_in);
                            self.apply_stack_change(body_in + total_net);
                        } else {
                            // Unknown iteration count
                            self.unknown_effect = true;
                        }
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
        // if.true consumes 1 (condition), both branches push 1
        // Net: 1 input (condition), 1 output
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 1,
                outputs: 1
            }
        );
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
}
