//! Procedure contract inference.
//!
//! This module provides automatic inference of procedure contracts from their
//! implementations, enabling workspace-wide inter-procedural taint analysis.

use std::collections::HashMap;
use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Immediate, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, Spanned};
use tower_lsp::lsp_types::{Position, Range};

use crate::symbol_path::SymbolPath;

use super::types::Bounds;
use super::while_loops::infer_while_bound;

// ═══════════════════════════════════════════════════════════════════════════
// Helper for extracting values from immediates
// ═══════════════════════════════════════════════════════════════════════════

/// Try to extract a u64 value from a push immediate.
fn push_imm_to_u64(imm: &Immediate<miden_assembly_syntax::parser::PushValue>) -> Option<u64> {
    use miden_assembly_syntax::parser::{IntValue, PushValue};

    match imm {
        Immediate::Value(span) => match span.inner() {
            PushValue::Int(int_val) => {
                let val = match int_val {
                    IntValue::U8(v) => *v as u64,
                    IntValue::U16(v) => *v as u64,
                    IntValue::U32(v) => *v as u64,
                    IntValue::Felt(f) => f.as_int(),
                };
                Some(val)
            }
            PushValue::Word(_) => None, // Word pushes multiple values, can't extract single u64
        },
        _ => None,
    }
}


// ═══════════════════════════════════════════════════════════════════════════
// Signature Parsing
// ═══════════════════════════════════════════════════════════════════════════

/// Parsed stack effect from an explicit procedure signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedSignature {
    /// Number of input stack elements
    pub inputs: usize,
    /// Number of output stack elements
    pub outputs: usize,
}

/// Get the number of stack elements for a type name.
///
/// Returns the number of field elements that the type occupies on the stack:
/// - `u64` = 2 elements
/// - `u128` = 4 elements
/// - `u256` = 8 elements
/// - Unknown types default to 1 element
fn type_to_element_count(type_name: &str) -> usize {
    match type_name.trim() {
        "u64" => 2,
        "u128" => 4,
        "u256" => 8,
        _ => 1, // Default: single field element
    }
}

/// Parse a procedure signature to extract input and output element counts.
///
/// Supports signatures like:
/// - `proc foo(a: u256, b: u64) -> u128`
/// - `pub proc bar(x: u256) -> u256`
/// - `export.baz(a: u64, b: u64)`
///
/// Returns `None` if no signature with types is found.
pub fn parse_procedure_signature(signature_line: &str) -> Option<ParsedSignature> {
    // Find the parameter list: everything between ( and )
    let params_start = signature_line.find('(')?;
    let params_end = signature_line.find(')')?;
    if params_start >= params_end {
        return None;
    }

    let params_str = &signature_line[params_start + 1..params_end];

    // Parse input parameters
    let mut inputs = 0;
    if !params_str.trim().is_empty() {
        for param in params_str.split(',') {
            let param = param.trim();
            if param.is_empty() {
                continue;
            }
            // Look for type annotation: "name: type"
            if let Some(colon_pos) = param.find(':') {
                let type_name = param[colon_pos + 1..].trim();
                inputs += type_to_element_count(type_name);
            } else {
                // No type annotation, assume single element
                inputs += 1;
            }
        }
    }

    // Parse return type(s): everything after "->"
    let mut outputs = 0;
    if let Some(arrow_pos) = signature_line.find("->") {
        let return_str = &signature_line[arrow_pos + 2..];
        // Return can be a single type or comma-separated types
        for ret_type in return_str.split(',') {
            let ret_type = ret_type.trim();
            if ret_type.is_empty() {
                continue;
            }
            outputs += type_to_element_count(ret_type);
        }
    }

    // Only return if we found typed parameters (otherwise fall back to inference)
    if inputs > 0 || outputs > 0 {
        Some(ParsedSignature { inputs, outputs })
    } else {
        None
    }
}

/// Extract and parse a procedure signature from source text at a given line.
///
/// Returns the parsed signature if the procedure has explicit type annotations.
pub fn extract_and_parse_signature(source: &str, def_line: usize) -> Option<ParsedSignature> {
    let line = source.lines().nth(def_line)?;
    parse_procedure_signature(line)
}

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
    /// Stack effect is completely unknown
    #[default]
    Unknown,
    /// Inputs are known but outputs are unknown (e.g., due to while loops with unknown iteration count)
    KnownInputs { inputs: usize },
    /// Known stack effect: (inputs_consumed, outputs_produced)
    /// Net change = outputs_produced - inputs_consumed
    Known { inputs: usize, outputs: usize },
}

impl StackEffect {
    /// Get the number of inputs if known.
    pub fn inputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown => None,
            StackEffect::KnownInputs { inputs } => Some(*inputs),
            StackEffect::Known { inputs, .. } => Some(*inputs),
        }
    }

    /// Get the number of outputs if known.
    pub fn outputs(&self) -> Option<usize> {
        match self {
            StackEffect::Unknown | StackEffect::KnownInputs { .. } => None,
            StackEffect::Known { outputs, .. } => Some(*outputs),
        }
    }
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

        // Track stack effects
        match inst {
            // Procedure calls - handled via handle_procedure_call with transitive effects
            Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
                self.handle_procedure_call(target);
            }

            // Push operations - also track bounds for while loop analysis
            Instruction::Push(imm) => {
                self.apply_stack_change(1);
                let bounds = push_imm_to_u64(imm)
                    .map(Bounds::Const)
                    .unwrap_or(Bounds::Field);
                self.push_bounds(bounds);
            }
            Instruction::PushFeltList(values) => {
                self.apply_stack_change(values.len() as i32);
                for _ in values {
                    self.push_bounds(Bounds::Field);
                }
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
            }
            Instruction::AdvLoadW => {
                self.apply_stack_change(0); // pops 4, pushes 4
                for _ in 0..4 {
                    self.pop_bounds();
                }
                for _ in 0..4 {
                    self.push_bounds(Bounds::Field);
                }
            }
            Instruction::AdvPipe => {
                self.apply_stack_change(0); // pops 8, pushes 8
                for _ in 0..8 {
                    self.pop_bounds();
                }
                for _ in 0..8 {
                    self.push_bounds(Bounds::Field);
                }
            }

            // Pop/drop operations
            Instruction::Drop => {
                self.apply_stack_change(-1);
                self.pop_bounds();
            }
            Instruction::DropW => {
                self.apply_stack_change(-4);
                for _ in 0..4 {
                    self.pop_bounds();
                }
            }

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

            // Unary operations: pop 1, push 1
            Instruction::Neg | Instruction::Inv | Instruction::Not | Instruction::IsOdd | Instruction::Incr => {
                self.apply_pop_push(1, 1);
            }

            // Other math operations
            Instruction::Exp => {
                self.apply_pop_push(2, 1); // pop base + exp, push result
            }
            Instruction::ExpImm(_) | Instruction::ExpBitLength(_) => {
                self.apply_pop_push(1, 1); // pop base, push result
            }
            Instruction::ILog2 | Instruction::Pow2 => {
                self.apply_pop_push(1, 1);
            }

            // ─────────────────────────────────────────────────────────────────────
            // Memory operations - single element
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MemLoad => {
                self.apply_pop_push(1, 1); // pop addr, push value
            }
            Instruction::MemLoadImm(_) => {
                self.apply_stack_change(1); // push value (addr from immediate)
            }
            Instruction::MemStore => {
                self.apply_pop_push(2, 0); // pop addr + value
            }
            Instruction::MemStoreImm(_) => {
                self.apply_pop_push(1, 0); // pop value (addr from immediate)
            }

            // ─────────────────────────────────────────────────────────────────────
            // Memory operations - word level (4 elements)
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
                self.apply_pop_push(1, 4); // pop addr, push 4 values
            }
            Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
                self.apply_stack_change(4); // push 4 values (addr from immediate)
            }
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
                self.apply_pop_push(5, 0); // pop addr + 4 values
            }
            Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
                self.apply_pop_push(4, 0); // pop 4 values (addr from immediate)
            }

            // ─────────────────────────────────────────────────────────────────────
            // Local memory operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::LocLoad(_) => {
                self.apply_stack_change(1); // push 1 value
            }
            Instruction::LocLoadWBe(_) | Instruction::LocLoadWLe(_) => {
                self.apply_stack_change(4); // push 4 values
            }
            Instruction::LocStore(_) => {
                self.apply_pop_push(1, 0); // pop 1 value
            }
            Instruction::LocStoreWBe(_) | Instruction::LocStoreWLe(_) => {
                self.apply_pop_push(4, 0); // pop 4 values
            }
            Instruction::Locaddr(_) => {
                self.apply_stack_change(1); // push address
            }

            // ─────────────────────────────────────────────────────────────────────
            // Memory streaming
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MemStream => {
                self.apply_pop_push(12, 12); // pops 12, pushes 12
            }

            // ─────────────────────────────────────────────────────────────────────
            // Merkle operations - known stack effects
            // ─────────────────────────────────────────────────────────────────────
            Instruction::MTreeGet => {
                // Stack: [d, i, R, ...] -> [V, R, ...]
                // Pops depth + index (2), root word stays, pushes value word
                // Net: pops 2, pushes 4 (value word replaces nothing, root stays)
                // Actually: [d, i, R0, R1, R2, R3] -> [V0, V1, V2, V3, R0, R1, R2, R3]
                // That's pop 2, push 4
                self.apply_pop_push(2, 4);
            }
            Instruction::MTreeSet => {
                // Stack: [d, i, R, V, ...] -> [R', V, ...]
                // Pops d, i (2), keeps R (4), keeps V (4), pushes new R' (4)
                // Net: pops 2, pushes 4 (new root)
                self.apply_pop_push(2, 4);
            }
            Instruction::MTreeMerge => {
                // Stack: [R, L, ...] -> [M, ...]
                // Pops 8 (two words), pushes 4 (merged hash)
                self.apply_pop_push(8, 4);
            }
            Instruction::MTreeVerify | Instruction::MTreeVerifyWithError(_) => {
                // Stack: [d, i, R, V] -> [d, i, R, V] (unchanged, just verifies)
                // No stack change
                self.require_stack_depth(10); // needs d, i, R(4), V(4)
            }

            // ─────────────────────────────────────────────────────────────────────
            // Cryptographic operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Hash => {
                self.apply_pop_push(4, 4); // pop word, push hash
            }
            Instruction::HMerge => {
                self.apply_pop_push(8, 4); // pop 2 words, push merged hash
            }
            Instruction::HPerm => {
                self.apply_pop_push(12, 12); // permutation on 12 elements
            }

            // ─────────────────────────────────────────────────────────────────────
            // Extension field operations (ext2 - 2 field elements per value)
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Ext2Add | Instruction::Ext2Sub | Instruction::Ext2Mul | Instruction::Ext2Div => {
                self.apply_pop_push(4, 2); // pop 2 ext2 values (4 felts), push 1 ext2 (2 felts)
            }
            Instruction::Ext2Neg | Instruction::Ext2Inv => {
                self.apply_pop_push(2, 2); // pop 1 ext2, push 1 ext2
            }

            // ─────────────────────────────────────────────────────────────────────
            // Conditional operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::CSwap => {
                self.apply_pop_push(3, 2); // pop cond + 2 values, push 2 values
            }
            Instruction::CSwapW => {
                self.apply_pop_push(9, 8); // pop cond + 2 words, push 2 words
            }
            Instruction::CDrop => {
                self.apply_pop_push(3, 1); // pop cond + 2 values, push 1 value
            }
            Instruction::CDropW => {
                self.apply_pop_push(9, 4); // pop cond + 2 words, push 1 word
            }

            // ─────────────────────────────────────────────────────────────────────
            // Assertion operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Assert | Instruction::AssertWithError(_) => {
                self.apply_pop_push(1, 0); // pop condition
            }
            Instruction::AssertEq | Instruction::AssertEqWithError(_) => {
                self.apply_pop_push(2, 0); // pop 2 values to compare
            }
            Instruction::AssertEqw | Instruction::AssertEqwWithError(_) => {
                self.apply_pop_push(8, 0); // pop 2 words to compare
            }
            Instruction::Assertz | Instruction::AssertzWithError(_) => {
                self.apply_pop_push(1, 0); // pop value to check
            }
            Instruction::Eqw => {
                self.apply_pop_push(8, 1); // pop 2 words, push bool
            }

            // ─────────────────────────────────────────────────────────────────────
            // Input/environment operations
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Sdepth => {
                self.apply_stack_change(1); // push stack depth
            }
            Instruction::Caller => {
                self.apply_stack_change(4); // push caller hash (4 elements)
            }
            Instruction::Clk => {
                self.apply_stack_change(1); // push clock cycle
            }

            // ─────────────────────────────────────────────────────────────────────
            // Word-level stack manipulation
            // ─────────────────────────────────────────────────────────────────────
            Instruction::SwapW1 => self.require_stack_depth(8),
            Instruction::SwapW2 => self.require_stack_depth(12),
            Instruction::SwapW3 => self.require_stack_depth(16),
            Instruction::SwapDw => self.require_stack_depth(16),

            Instruction::MovUpW2 => self.require_stack_depth(12),
            Instruction::MovUpW3 => self.require_stack_depth(16),
            Instruction::MovDnW2 => self.require_stack_depth(12),
            Instruction::MovDnW3 => self.require_stack_depth(16),

            Instruction::Reversew => self.require_stack_depth(4),
            Instruction::Reversedw => self.require_stack_depth(8),

            // ─────────────────────────────────────────────────────────────────────
            // Dynamic procedure calls
            // ─────────────────────────────────────────────────────────────────────
            Instruction::DynExec | Instruction::DynCall => {
                // Pop target hash (4 elements), unknown effect after
                self.apply_pop_push(4, 0);
                self.unknown_effect = true;
            }
            Instruction::ProcRef(_) => {
                self.apply_stack_change(4); // push procedure hash
            }

            // ─────────────────────────────────────────────────────────────────────
            // u32 operations - explicit handling for each variant
            // ─────────────────────────────────────────────────────────────────────

            // u32 assertions - no stack change
            Instruction::U32Assert | Instruction::U32AssertWithError(_) |
            Instruction::U32Assert2 | Instruction::U32Assert2WithError(_) |
            Instruction::U32AssertW | Instruction::U32AssertWWithError(_) => {
                // No stack change, just validation
            }

            // u32 test - pushes bool without popping
            Instruction::U32Test => {
                self.apply_stack_change(1);
            }
            Instruction::U32TestW => {
                self.apply_stack_change(1);
            }

            // u32 split/cast
            Instruction::U32Split => {
                self.apply_pop_push(1, 2); // pop 1 felt, push hi + lo
            }
            Instruction::U32Cast => {
                self.apply_pop_push(1, 1); // pop 1, push truncated
            }

            // u32 wrapping binary ops: pop 2, push 1
            Instruction::U32WrappingAdd | Instruction::U32WrappingSub | Instruction::U32WrappingMul => {
                self.apply_pop_push(2, 1);
            }
            // u32 wrapping with immediate: pop 1, push 1
            Instruction::U32WrappingAddImm(_) | Instruction::U32WrappingSubImm(_) | Instruction::U32WrappingMulImm(_) => {
                self.apply_pop_push(1, 1);
            }

            // u32 overflowing binary ops: pop 2, push 2 (result + overflow flag)
            Instruction::U32OverflowingAdd | Instruction::U32OverflowingSub | Instruction::U32OverflowingMul => {
                self.apply_pop_push(2, 2);
            }
            // u32 overflowing with immediate: pop 1, push 2
            Instruction::U32OverflowingAddImm(_) | Instruction::U32OverflowingSubImm(_) | Instruction::U32OverflowingMulImm(_) => {
                self.apply_pop_push(1, 2);
            }

            // u32 ternary ops: pop 3
            Instruction::U32OverflowingAdd3 => {
                self.apply_pop_push(3, 2); // pop 3, push result + carry
            }
            Instruction::U32WrappingAdd3 => {
                self.apply_pop_push(3, 1); // pop 3, push wrapped result
            }
            Instruction::U32OverflowingMadd => {
                self.apply_pop_push(3, 2); // pop 3, push result + overflow
            }
            Instruction::U32WrappingMadd => {
                self.apply_pop_push(3, 1); // pop 3, push wrapped result
            }

            // u32 division: pop 2, push 1
            Instruction::U32Div | Instruction::U32Mod => {
                self.apply_pop_push(2, 1);
            }
            // u32 division with immediate: pop 1, push 1
            Instruction::U32DivImm(_) | Instruction::U32ModImm(_) => {
                self.apply_pop_push(1, 1);
            }
            // u32 divmod: pop 2, push 2 (quotient + remainder)
            Instruction::U32DivMod => {
                self.apply_pop_push(2, 2);
            }
            Instruction::U32DivModImm(_) => {
                self.apply_pop_push(1, 2);
            }

            // u32 bitwise binary: pop 2, push 1
            Instruction::U32And | Instruction::U32Or | Instruction::U32Xor => {
                self.apply_pop_push(2, 1);
            }
            // u32 bitwise not: pop 1, push 1
            Instruction::U32Not => {
                self.apply_pop_push(1, 1);
            }

            // u32 shift/rotate: pop 2, push 1
            Instruction::U32Shl | Instruction::U32Shr | Instruction::U32Rotl | Instruction::U32Rotr => {
                self.apply_pop_push(2, 1);
            }
            // u32 shift/rotate with immediate: pop 1, push 1
            Instruction::U32ShlImm(_) | Instruction::U32ShrImm(_) |
            Instruction::U32RotlImm(_) | Instruction::U32RotrImm(_) => {
                self.apply_pop_push(1, 1);
            }

            // u32 bit counting: pop 1, push 1
            Instruction::U32Popcnt | Instruction::U32Clz | Instruction::U32Ctz |
            Instruction::U32Clo | Instruction::U32Cto => {
                self.apply_pop_push(1, 1);
            }

            // u32 comparison: pop 2, push 1 (bool)
            Instruction::U32Lt | Instruction::U32Lte | Instruction::U32Gt | Instruction::U32Gte => {
                self.apply_pop_push(2, 1);
            }

            // u32 min/max: pop 2, push 1
            Instruction::U32Min | Instruction::U32Max => {
                self.apply_pop_push(2, 1);
            }

            // ─────────────────────────────────────────────────────────────────────
            // Push slice
            // ─────────────────────────────────────────────────────────────────────
            Instruction::PushSlice(_, range) => {
                self.apply_stack_change(range.len() as i32);
            }

            // ─────────────────────────────────────────────────────────────────────
            // Debug/trace/nop - no stack effect
            // ─────────────────────────────────────────────────────────────────────
            Instruction::Nop | Instruction::Breakpoint | Instruction::Debug(_) |
            Instruction::Emit | Instruction::EmitImm(_) | Instruction::Trace(_) => {
                // No stack effect
            }

            // ─────────────────────────────────────────────────────────────────────
            // STARK/complex operations - mark as unknown
            // ─────────────────────────────────────────────────────────────────────
            Instruction::FriExt2Fold4 | Instruction::HornerBase | Instruction::HornerExt |
            Instruction::EvalCircuit | Instruction::LogPrecompile | Instruction::SysEvent(_) => {
                self.unknown_effect = true;
            }
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
    fn test_if_else_different_effects_known_inputs() {
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
        // Different net effects: then=+1, else=+2 -> outputs unknown, but inputs (1 for condition) known
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::KnownInputs { inputs: 1 }
        );
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
    fn test_while_nonzero_net_effect_known_inputs() {
        // while loop with non-zero net effect = inputs known, outputs unknown
        // This test: while body just pushes, needs 1 input for condition check
        let contracts = parse_and_infer(
            "proc while_push
    swap
    while.true
        push.1
    end
end",
        );

        assert_eq!(contracts.len(), 1);
        // swap needs 2 inputs, while loop body has non-zero net effect
        // Inputs are preserved even though outputs are unknown
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::KnownInputs { inputs: 2 }
        );
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

    #[test]
    fn test_while_countdown_known_bound() {
        // While loop with known countdown pattern:
        // - push.5 initializes counter to 5
        // - body has net effect of +2 (computed as inputs:1, outputs:3 by body analysis)
        // - 5 iterations × net +2 = 10 from loop, minus the input borrowed = 10
        //
        // Note: The body analysis computes effect as if starting with empty stack,
        // which leads to a conservative estimate. The actual runtime behavior
        // would be different, but for safety analysis this is acceptable.
        let contracts = parse_and_infer(
            "proc countdown_loop
    push.5
    while.true
        push.1
        swap
        sub.1
        dup.0
        neq.0
    end
    drop
end",
        );

        assert_eq!(contracts.len(), 1);
        // Body effect: inputs=1, outputs=3, net=+2
        // Loop effect: -1 input + (1 + 5*2) = 10 outputs from while
        // Total: push.5(+1) + while(10) + drop(-1) = 10 outputs
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 10
            }
        );
    }

    #[test]
    fn test_while_simple_countdown_pattern() {
        // Simpler countdown pattern:
        // - push.5 initializes counter
        // - body: sub.1 (needs 1, produces 1), dup.0 (+1), neq.0 (needs 1, produces 1)
        // - body analysis: inputs=1, outputs=2, net=+1 per iteration
        // - 5 iterations × net +1 = 5 from loop
        let contracts = parse_and_infer(
            "proc simple_countdown
    push.5
    while.true
        sub.1
        dup.0
        neq.0
    end
    drop
end",
        );

        assert_eq!(contracts.len(), 1);
        // push.5(+1) + while loop(-1 input + 1 + 5*1 = 5) + drop(-1) = 5 outputs
        assert_eq!(
            contracts[0].stack_effect,
            StackEffect::Known {
                inputs: 0,
                outputs: 5
            }
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Signature Parsing Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn test_parse_signature_u256_params() {
        let sig = parse_procedure_signature("pub proc xor(rhs: u256, lhs: u256) -> u256");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u256 = 8 elements each, so 2 params = 16 inputs
        assert_eq!(sig.inputs, 16);
        // Return u256 = 8 outputs
        assert_eq!(sig.outputs, 8);
    }

    #[test]
    fn test_parse_signature_u64_params() {
        let sig = parse_procedure_signature("proc add_u64(a: u64, b: u64) -> u64");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u64 = 2 elements each, so 2 params = 4 inputs
        assert_eq!(sig.inputs, 4);
        // Return u64 = 2 outputs
        assert_eq!(sig.outputs, 2);
    }

    #[test]
    fn test_parse_signature_u128_params() {
        let sig = parse_procedure_signature("proc transform(x: u128) -> u128");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u128 = 4 elements
        assert_eq!(sig.inputs, 4);
        assert_eq!(sig.outputs, 4);
    }

    #[test]
    fn test_parse_signature_mixed_types() {
        let sig = parse_procedure_signature("proc mixed(a: u64, b: u128, c: u256) -> u64");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        // u64=2 + u128=4 + u256=8 = 14 inputs
        assert_eq!(sig.inputs, 14);
        // Return u64 = 2 outputs
        assert_eq!(sig.outputs, 2);
    }

    #[test]
    fn test_parse_signature_no_types() {
        // No type annotations - should return None
        let sig = parse_procedure_signature("proc simple");
        assert!(sig.is_none());
    }

    #[test]
    fn test_parse_signature_no_params_with_return() {
        let sig = parse_procedure_signature("proc get_value() -> u256");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        assert_eq!(sig.inputs, 0);
        assert_eq!(sig.outputs, 8);
    }

    #[test]
    fn test_parse_signature_unknown_type_defaults() {
        // Unknown types default to 1 element
        let sig = parse_procedure_signature("proc foo(x: felt) -> felt");
        assert!(sig.is_some());
        let sig = sig.unwrap();
        assert_eq!(sig.inputs, 1);
        assert_eq!(sig.outputs, 1);
    }

    #[test]
    fn test_type_to_element_count() {
        assert_eq!(type_to_element_count("u64"), 2);
        assert_eq!(type_to_element_count("u128"), 4);
        assert_eq!(type_to_element_count("u256"), 8);
        assert_eq!(type_to_element_count("felt"), 1);
        assert_eq!(type_to_element_count("unknown"), 1);
    }
}
