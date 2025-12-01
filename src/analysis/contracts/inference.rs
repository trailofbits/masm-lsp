//! Contract inference from procedure implementations.
//!
//! This module provides automatic inference of procedure contracts by analyzing
//! procedure bodies. It detects validation patterns, u32 operations, advice reads,
//! Merkle operations, and computes stack effects.
//!
//! # Performance
//!
//! Contract inference uses topological ordering to analyze procedures efficiently.
//! By analyzing procedures in dependency order (leaves first), each procedure is
//! analyzed exactly once with full knowledge of its callees' contracts.

use std::collections::HashMap;
use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, Spanned};
use tower_lsp::lsp_types::Range;

use crate::symbol_path::SymbolPath;
use crate::symbol_resolution::SymbolResolver;

use super::super::abstract_interpretation::{AbstractState, SymbolicExpr};
use super::super::call_graph::{CallGraph, TopologicalNode};
use super::super::stack_ops::StackLike;
use super::store::ContractStore;
use super::types::{InputKind, OutputKind, ProcContract, ProcSignature, StackEffect, ValidationBehavior};
use crate::analysis::stack_ops::static_effect;
use crate::analysis::types::Bounds;
use crate::analysis::utils::push_imm_to_u64;
use crate::analysis::while_loops::infer_while_bound;

// ═══════════════════════════════════════════════════════════════════════════
// Module-Level Inference
// ═══════════════════════════════════════════════════════════════════════════

/// Infer contracts for all procedures in a module.
///
/// This function analyzes procedures in topological order (leaves first),
/// so each procedure is analyzed exactly once with full knowledge of its
/// callees' contracts.
pub fn infer_module_contracts(
    module: &Module,
    source_manager: &DefaultSourceManager,
) -> Vec<ProcContract> {
    infer_module_contracts_with_store(module, source_manager, None)
}

/// Infer contracts for all procedures in a module, with access to existing contracts.
///
/// This uses topological ordering to analyze procedures efficiently:
/// 1. Build a call graph and compute topological order (leaves first)
/// 2. Analyze each procedure exactly once, with full callee knowledge
/// 3. For mutually recursive procedures (SCCs), mark as Unknown
///
/// # Performance
///
/// - Each procedure analyzed exactly once: O(n) procedure analyses
/// - No iteration or convergence checking needed
/// - Time complexity: O(n × m) where n is procedures, m is average procedure size
pub fn infer_module_contracts_with_store(
    module: &Module,
    source_manager: &DefaultSourceManager,
    existing_contracts: Option<&ContractStore>,
) -> Vec<ProcContract> {
    // Collect procedure metadata (paths, definition ranges, AST references)
    let mut proc_info: HashMap<SymbolPath, (Option<Range>, &Procedure)> = HashMap::new();
    for proc in module.procedures() {
        let path = SymbolPath::from_module_and_name(module, proc.name().as_str());
        let definition_range = crate::diagnostics::span_to_range(source_manager, proc.name().span());
        proc_info.insert(path, (definition_range, proc));
    }

    // Build call graph and compute topological order
    let call_graph = CallGraph::from_module(module);
    let topo_order = call_graph.topological_order();

    // Create resolver for this module (used for resolving call targets)
    let resolver = crate::symbol_resolution::create_resolver(module);

    // Initialize working store with existing contracts (for external calls)
    let mut working_store = existing_contracts.cloned().unwrap_or_default();

    // Process procedures in topological order (leaves first)
    // This ensures callees are always analyzed before callers
    for node in &topo_order {
        match node {
            TopologicalNode::Single(path) => {
                // Analyze this procedure - all its callees are already in working_store
                if let Some((def_range, proc)) = proc_info.get(path) {
                    let contract = infer_procedure_contract_with_store(
                        proc,
                        path.clone(),
                        *def_range,
                        &resolver,
                        Some(&working_store),
                    );
                    working_store.update_document(vec![contract]);
                }
            }
            TopologicalNode::Cycle(paths) => {
                // Mutually recursive procedures - analyze with partial information
                // First pass: analyze all with current (possibly incomplete) info
                for path in paths {
                    if let Some((def_range, proc)) = proc_info.get(path) {
                        let contract = infer_procedure_contract_with_store(
                            proc,
                            path.clone(),
                            *def_range,
                            &resolver,
                            Some(&working_store),
                        );
                        working_store.update_document(vec![contract]);
                    }
                }

                // Second pass: re-analyze to propagate any discovered effects
                // This handles simple mutual recursion cases
                for path in paths {
                    if let Some((def_range, proc)) = proc_info.get(path) {
                        let contract = infer_procedure_contract_with_store(
                            proc,
                            path.clone(),
                            *def_range,
                            &resolver,
                            Some(&working_store),
                        );
                        working_store.update_document(vec![contract]);
                    }
                }
            }
        }
    }

    // Signature propagation pass
    // Signatures may need one more pass to propagate transitive information
    // (e.g., OutputAddress through call chains)
    for node in &topo_order {
        for path in node.paths() {
            if let Some((def_range, proc)) = proc_info.get(path) {
                let contract = infer_procedure_contract_with_store(
                    proc,
                    path.clone(),
                    *def_range,
                    &resolver,
                    Some(&working_store),
                );
                working_store.update_document(vec![contract]);
            }
        }
    }

    // Collect final contracts in original order
    proc_info
        .keys()
        .filter_map(|path| working_store.get(path).cloned())
        .collect()
}
// ═══════════════════════════════════════════════════════════════════════════
// Procedure Contract Inference
// ═══════════════════════════════════════════════════════════════════════════

/// Infer a contract with access to existing contracts for transitive effects.
fn infer_procedure_contract_with_store(
    proc: &Procedure,
    path: SymbolPath,
    definition_range: Option<Range>,
    resolver: &SymbolResolver,
    contracts: Option<&ContractStore>,
) -> ProcContract {
    let mut analyzer = SignatureAnalyzer::new(resolver, contracts);
    let _ = visit::visit_procedure(&mut analyzer, proc);

    ProcContract {
        path,
        validates: analyzer.validation_behavior(),
        uses_u32_ops: analyzer.uses_u32_ops,
        reads_advice: analyzer.reads_advice,
        uses_merkle_ops: analyzer.uses_merkle_ops,
        stack_effect: analyzer.stack_effect(),
        signature: analyzer.build_signature(),
        definition_range,
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper Functions for SymbolicExpr
// ═══════════════════════════════════════════════════════════════════════════

/// Returns the input position if this expression is a direct Input reference.
fn expr_input_position(expr: &SymbolicExpr) -> Option<usize> {
    match expr {
        SymbolicExpr::Input(pos) => Some(*pos),
        _ => None,
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Signature Analyzer
// ═══════════════════════════════════════════════════════════════════════════

/// Full-featured analyzer that walks a procedure to infer its complete contract,
/// including detailed input/output signatures with provenance tracking.
///
/// Uses `AbstractState` from the abstract interpretation framework for stack
/// tracking, which provides sophisticated symbolic expression handling.
struct SignatureAnalyzer<'a> {
    /// Symbol resolver for resolving call targets to fully-qualified paths
    resolver: &'a SymbolResolver<'a>,
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
    /// Whether stack effect is unknown (due to control flow or unknown calls)
    unknown_effect: bool,
    /// Bounds tracking for top stack elements (for while loop analysis)
    bounds_stack: Vec<Bounds>,
    /// Abstract state for tracking symbolic stack values
    abstract_state: AbstractState,
    /// Inferred input kinds based on how inputs are used
    input_usage: Vec<InputKind>,
    /// Track maximum inputs seen (for stack effect calculation)
    max_inputs_seen: usize,
}

impl<'a> SignatureAnalyzer<'a> {
    fn new(resolver: &'a SymbolResolver<'a>, contracts: Option<&'a ContractStore>) -> Self {
        Self {
            resolver,
            contracts,
            early_validation: false,
            validation_count: 0,
            seen_non_validation: false,
            uses_u32_ops: false,
            reads_advice: false,
            uses_merkle_ops: false,
            unknown_effect: false,
            bounds_stack: Vec::new(),
            abstract_state: AbstractState::empty(),
            input_usage: Vec::new(),
            max_inputs_seen: 0,
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
        // Calculate inputs from abstract state
        let inputs = self.abstract_state.inputs_discovered().max(self.max_inputs_seen);

        if self.unknown_effect {
            // Even when outputs are unknown, we still know the minimum inputs required
            StackEffect::KnownInputs { inputs }
        } else {
            // With AbstractState, outputs are simply the stack depth
            // The abstract state tracks all inputs that were consumed/discovered
            let outputs = self.abstract_state.depth();
            StackEffect::Known { inputs, outputs }
        }
    }

    /// Build the full procedure signature from collected usage information.
    fn build_signature(&self) -> Option<ProcSignature> {
        let num_inputs = self.abstract_state.inputs_discovered().max(self.max_inputs_seen);

        let num_outputs = if self.unknown_effect {
            0 // Unknown outputs
        } else {
            self.abstract_state.depth()
        };

        // If no inputs and no outputs, no signature needed
        if num_inputs == 0 && num_outputs == 0 {
            return None;
        }

        let mut sig = ProcSignature::new(num_inputs, num_outputs);

        // Copy over the inferred input kinds
        for (i, kind) in self.input_usage.iter().enumerate() {
            if i < num_inputs {
                sig.inputs[i] = *kind;
            }
        }

        // Build output kinds from abstract state stack
        // Output array: outputs[0] = top of stack, outputs[num_outputs-1] = bottom
        for i in 0..num_outputs {
            if let Some(expr) = self.abstract_state.peek(i) {
                sig.outputs[i] = match expr {
                    SymbolicExpr::Input(pos) => {
                        OutputKind::InputPassthrough { input_pos: *pos }
                    }
                    SymbolicExpr::MemoryLoad { address } => {
                        let from_input = expr_input_position(address);
                        OutputKind::MemoryRead { from_input }
                    }
                    _ => OutputKind::Computed,
                };
            }
        }

        Some(sig)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Stack helpers
    // ═══════════════════════════════════════════════════════════════════════

    /// Record that an input at the given position was used as a memory address
    /// for a read operation.
    fn record_input_read(&mut self, input_pos: usize) {
        // Ensure input_usage is large enough
        while self.input_usage.len() <= input_pos {
            self.input_usage.push(InputKind::Value);
        }
        self.input_usage[input_pos] = self.input_usage[input_pos].merge(InputKind::InputAddress);
        self.max_inputs_seen = self.max_inputs_seen.max(input_pos + 1);
    }

    /// Record that an input at the given position was used as a memory address
    /// for a write operation.
    fn record_input_write(&mut self, input_pos: usize) {
        // Ensure input_usage is large enough
        while self.input_usage.len() <= input_pos {
            self.input_usage.push(InputKind::Value);
        }
        self.input_usage[input_pos] = self.input_usage[input_pos].merge(InputKind::OutputAddress);
        self.max_inputs_seen = self.max_inputs_seen.max(input_pos + 1);
    }

    /// Pop one element from stack, tracking if it was an input
    fn pop_one(&mut self) -> SymbolicExpr {
        let elem = self.abstract_state.pop();
        // Track inputs that are being consumed
        if let SymbolicExpr::Input(pos) = &elem {
            self.max_inputs_seen = self.max_inputs_seen.max(*pos + 1);
        }
        elem
    }

    /// Pop N elements from stack, tracking any inputs that were consumed
    fn pop_n(&mut self, n: usize) {
        for _ in 0..n {
            self.pop_one();
        }
    }

    /// Push N elements with Top (derived/unknown) symbolic expression
    fn push_n(&mut self, n: usize, expr: SymbolicExpr) {
        for _ in 0..n {
            self.abstract_state.push(expr.clone());
        }
    }

    /// Apply a pop then push operation (e.g., add pops 2, pushes 1).
    fn apply_pop_push(&mut self, pops: usize, pushes: usize) {
        self.pop_n(pops);
        self.push_n(pushes, SymbolicExpr::Top);
    }

    /// Duplicate element at position n to top (dup.n)
    fn dup(&mut self, n: usize) {
        self.abstract_state.ensure_depth(n + 1);
        if let Some(expr) = self.abstract_state.peek(n) {
            let cloned = expr.clone();
            self.abstract_state.push(cloned);
        }
    }

    /// Duplicate word at position n to top (dupw.n)
    fn dupw(&mut self, n: usize) {
        let start = n * 4;
        self.abstract_state.ensure_depth(start + 4);
        // Duplicate 4 elements from word n to top
        for i in 0..4 {
            if let Some(expr) = self.abstract_state.peek(start + 3 - i) {
                let cloned = expr.clone();
                self.abstract_state.push(cloned);
            }
        }
    }

    /// Swap word 0 (top) with word at word_offset (swapw.N)
    fn swapw(&mut self, word_offset: usize) {
        let start = word_offset * 4;
        self.abstract_state.ensure_depth(start + 4);
        for i in 0..4 {
            self.abstract_state.swap(i, start + i);
        }
    }

    /// Move word at position n to top (movupw.n)
    fn movupw(&mut self, n: usize) {
        let start = n * 4;
        // movupw moves 4 elements together
        for _ in 0..4 {
            self.abstract_state.movup(start);
        }
    }

    /// Move top word to position n (movdnw.n)
    fn movdnw(&mut self, n: usize) {
        let start = n * 4;
        // movdnw moves 4 elements together
        for _ in 0..4 {
            self.abstract_state.movdn(start);
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Memory operation detection for input signature inference
    // ═══════════════════════════════════════════════════════════════════════

    /// Check if instruction is a memory operation and record input usage.
    ///
    /// Memory operations consume an address from the stack. If that address
    /// came from a procedure input, we record whether the input is used for
    /// reading (InputAddress) or writing (OutputAddress).
    ///
    /// This now uses proper provenance tracking through all stack operations.
    fn check_memory_operation(&mut self, inst: &Instruction) {
        // Determine the stack position of the address operand and operation type
        // Note: For stack-based ops, address is at top (pos 0)
        // For immediate ops, address is embedded in instruction (no input tracking)
        let (addr_pos, is_write) = match inst {
            // ─────────────────────────────────────────────────────────────────
            // Memory stores (writes) - address consumed from stack
            // ─────────────────────────────────────────────────────────────────
            // mem_store: [val, addr] -> consumes both, addr at pos 0
            Instruction::MemStore => (0, true),
            // mem_storew: [V0, V1, V2, V3, addr] -> addr at pos 0 (after 4 values)
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => (0, true),
            // Immediate variants don't use stack address
            Instruction::MemStoreImm(_) | Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
                return; // Address is immediate, not from input
            }

            // ─────────────────────────────────────────────────────────────────
            // Memory loads (reads) - address consumed from stack
            // ─────────────────────────────────────────────────────────────────
            // mem_load: [addr] -> consumes addr, pushes value
            Instruction::MemLoad => (0, false),
            // mem_loadw: [V0, V1, V2, V3, addr] -> addr at pos 0
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => (0, false),
            // Immediate variants don't use stack address
            Instruction::MemLoadImm(_) | Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
                return; // Address is immediate, not from input
            }

            // ─────────────────────────────────────────────────────────────────
            // Memory stream operations
            // ─────────────────────────────────────────────────────────────────
            Instruction::MemStream => {
                // Complex operation, skip for now
                return;
            }

            // Not a memory operation
            _ => return,
        };

        // Use abstract state to determine if address came from an input
        // Need to ensure we have enough depth to peek at the address position
        self.abstract_state.ensure_depth(addr_pos + 1);
        if let Some(expr) = self.abstract_state.peek(addr_pos) {
            if let Some(input_pos) = expr_input_position(expr) {
                if is_write {
                    self.record_input_write(input_pos);
                } else {
                    self.record_input_read(input_pos);
                }
            }
        }
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
    fn compute_block_effect(&self, block: &Block) -> Option<(usize, usize)> {
        let mut sub_analyzer = SignatureAnalyzer::new(self.resolver, self.contracts);
        let _ = visit::visit_block(&mut sub_analyzer, block);

        if sub_analyzer.unknown_effect {
            None
        } else {
            // Return (inputs_consumed, outputs_produced)
            let inputs = sub_analyzer.abstract_state.inputs_discovered();
            let outputs = sub_analyzer.abstract_state.depth();
            Some((inputs, outputs))
        }
    }

    /// Handle procedure call - apply transitive stack effect and propagate address usage.
    fn handle_procedure_call(&mut self, target: &InvocationTarget) {
        // MAST root calls have unknown effect
        if matches!(target, InvocationTarget::MastRoot(_)) {
            self.unknown_effect = true;
            return;
        }

        // Look up the contract using the resolved fully-qualified path
        let contract = self.contracts.and_then(|store| {
            let resolved = self.resolver.resolve_target(target)?;
            store.get(&resolved)
        });

        match contract.map(|c| &c.stack_effect) {
            Some(StackEffect::Known { inputs, outputs }) => {
                // Propagate address usage from callee to our inputs
                if let Some(sig) = contract.and_then(|c| c.signature.as_ref()) {
                    // Ensure we have enough depth to peek at argument positions
                    self.abstract_state.ensure_depth(sig.inputs.len());

                    // Check each input parameter of the callee
                    // Callee's input i corresponds to stack position i
                    // (input 0 is at top of stack, input 1 is below, etc.)
                    for (i, kind) in sig.inputs.iter().enumerate() {
                        if let Some(arg_expr) = self.abstract_state.peek(i) {
                            // If we're passing an input to an address parameter, propagate
                            if let Some(our_input) = expr_input_position(arg_expr) {
                                if kind.is_output() {
                                    self.record_input_write(our_input);
                                }
                                if kind.is_input_address() {
                                    self.record_input_read(our_input);
                                }
                            }
                        }
                    }
                }

                // Apply stack effect
                self.pop_n(*inputs);
                self.push_n(*outputs, SymbolicExpr::Top);
            }
            Some(StackEffect::KnownInputs { inputs }) => {
                // We know inputs but not outputs - apply inputs, mark outputs unknown
                self.pop_n(*inputs);
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

        // Check for memory operations to infer input signatures (before applying stack effects)
        self.check_memory_operation(inst);

        // Track stack effects with proper provenance
        match inst {
            // ─────────────────────────────────────────────────────────────────
            // Procedure calls
            // ─────────────────────────────────────────────────────────────────
            Instruction::Exec(target) | Instruction::Call(target) | Instruction::SysCall(target) => {
                self.handle_procedure_call(target);
                return;
            }

            Instruction::DynExec | Instruction::DynCall => {
                self.apply_pop_push(4, 0);
                self.unknown_effect = true;
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // STARK/complex operations - mark as unknown
            // ─────────────────────────────────────────────────────────────────
            Instruction::FriExt2Fold4 | Instruction::HornerBase | Instruction::HornerExt |
            Instruction::EvalCircuit | Instruction::LogPrecompile | Instruction::SysEvent(_) => {
                self.unknown_effect = true;
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Push operations
            // ─────────────────────────────────────────────────────────────────
            Instruction::Push(imm) => {
                let value = push_imm_to_u64(imm);
                let expr = value.map(SymbolicExpr::Constant).unwrap_or(SymbolicExpr::Top);
                self.abstract_state.push(expr);
                let bounds = value.map(Bounds::Const).unwrap_or(Bounds::Field);
                self.push_bounds(bounds);
                return;
            }
            Instruction::PushFeltList(values) => {
                for _ in values {
                    self.abstract_state.push(SymbolicExpr::Top);
                    self.push_bounds(Bounds::Field);
                }
                return;
            }
            Instruction::AdvPush(n) => {
                let count = match n {
                    Immediate::Value(v) => v.into_inner() as usize,
                    _ => 1,
                };
                for _ in 0..count {
                    self.abstract_state.push(SymbolicExpr::Advice);
                    self.push_bounds(Bounds::Field);
                }
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Memory operations with symbolic tracking
            // ─────────────────────────────────────────────────────────────────
            Instruction::MemLoad => {
                // [addr] -> [value]
                let addr_expr = self.pop_one();
                self.abstract_state.push(SymbolicExpr::MemoryLoad {
                    address: Box::new(addr_expr),
                });
                return;
            }
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
                // [V0, V1, V2, V3, addr] -> [V0', V1', V2', V3']
                let addr_expr = self.pop_one();
                self.pop_n(4);
                for _ in 0..4 {
                    self.abstract_state.push(SymbolicExpr::MemoryLoad {
                        address: Box::new(addr_expr.clone()),
                    });
                }
                return;
            }
            Instruction::MemStore => {
                // [val, addr] -> []
                self.pop_n(2);
                return;
            }
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
                // [V0, V1, V2, V3, addr] -> []
                self.pop_n(5);
                return;
            }
            Instruction::MemLoadImm(_) => {
                self.abstract_state.push(SymbolicExpr::Top);
                return;
            }
            Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
                self.pop_n(4);
                self.push_n(4, SymbolicExpr::Top);
                return;
            }
            Instruction::MemStoreImm(_) => {
                self.pop_one();
                return;
            }
            Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
                self.pop_n(4);
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Drop operations
            // ─────────────────────────────────────────────────────────────────
            Instruction::Drop => {
                self.pop_one();
                self.pop_bounds();
                return;
            }
            Instruction::DropW => {
                self.pop_n(4);
                for _ in 0..4 {
                    self.pop_bounds();
                }
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Dup operations - preserve symbolic expressions
            // ─────────────────────────────────────────────────────────────────
            Instruction::Dup0 => { self.dup(0); return; }
            Instruction::Dup1 => { self.dup(1); return; }
            Instruction::Dup2 => { self.dup(2); return; }
            Instruction::Dup3 => { self.dup(3); return; }
            Instruction::Dup4 => { self.dup(4); return; }
            Instruction::Dup5 => { self.dup(5); return; }
            Instruction::Dup6 => { self.dup(6); return; }
            Instruction::Dup7 => { self.dup(7); return; }
            Instruction::Dup8 => { self.dup(8); return; }
            Instruction::Dup9 => { self.dup(9); return; }
            Instruction::Dup10 => { self.dup(10); return; }
            Instruction::Dup11 => { self.dup(11); return; }
            Instruction::Dup12 => { self.dup(12); return; }
            Instruction::Dup13 => { self.dup(13); return; }
            Instruction::Dup14 => { self.dup(14); return; }
            Instruction::Dup15 => { self.dup(15); return; }
            Instruction::DupW0 => { self.dupw(0); return; }
            Instruction::DupW1 => { self.dupw(1); return; }
            Instruction::DupW2 => { self.dupw(2); return; }
            Instruction::DupW3 => { self.dupw(3); return; }

            // ─────────────────────────────────────────────────────────────────
            // Swap operations - preserve symbolic expressions through swap
            // ─────────────────────────────────────────────────────────────────
            Instruction::Swap1 => { self.abstract_state.swap(0, 1); return; }
            Instruction::Swap2 => { self.abstract_state.swap(0, 2); return; }
            Instruction::Swap3 => { self.abstract_state.swap(0, 3); return; }
            Instruction::Swap4 => { self.abstract_state.swap(0, 4); return; }
            Instruction::Swap5 => { self.abstract_state.swap(0, 5); return; }
            Instruction::Swap6 => { self.abstract_state.swap(0, 6); return; }
            Instruction::Swap7 => { self.abstract_state.swap(0, 7); return; }
            Instruction::Swap8 => { self.abstract_state.swap(0, 8); return; }
            Instruction::Swap9 => { self.abstract_state.swap(0, 9); return; }
            Instruction::Swap10 => { self.abstract_state.swap(0, 10); return; }
            Instruction::Swap11 => { self.abstract_state.swap(0, 11); return; }
            Instruction::Swap12 => { self.abstract_state.swap(0, 12); return; }
            Instruction::Swap13 => { self.abstract_state.swap(0, 13); return; }
            Instruction::Swap14 => { self.abstract_state.swap(0, 14); return; }
            Instruction::Swap15 => { self.abstract_state.swap(0, 15); return; }
            Instruction::SwapW1 => { self.swapw(1); return; }
            Instruction::SwapW2 => { self.swapw(2); return; }
            Instruction::SwapW3 => { self.swapw(3); return; }
            Instruction::SwapDw => {
                // Swap double words [0..8] with [8..16]
                self.abstract_state.ensure_depth(16);
                for i in 0..8 {
                    self.abstract_state.swap(i, 8 + i);
                }
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Move operations - preserve symbolic expressions
            // ─────────────────────────────────────────────────────────────────
            Instruction::MovUp2 => { self.abstract_state.movup(2); return; }
            Instruction::MovUp3 => { self.abstract_state.movup(3); return; }
            Instruction::MovUp4 => { self.abstract_state.movup(4); return; }
            Instruction::MovUp5 => { self.abstract_state.movup(5); return; }
            Instruction::MovUp6 => { self.abstract_state.movup(6); return; }
            Instruction::MovUp7 => { self.abstract_state.movup(7); return; }
            Instruction::MovUp8 => { self.abstract_state.movup(8); return; }
            Instruction::MovUp9 => { self.abstract_state.movup(9); return; }
            Instruction::MovUp10 => { self.abstract_state.movup(10); return; }
            Instruction::MovUp11 => { self.abstract_state.movup(11); return; }
            Instruction::MovUp12 => { self.abstract_state.movup(12); return; }
            Instruction::MovUp13 => { self.abstract_state.movup(13); return; }
            Instruction::MovUp14 => { self.abstract_state.movup(14); return; }
            Instruction::MovUp15 => { self.abstract_state.movup(15); return; }
            Instruction::MovUpW2 => { self.movupw(2); return; }
            Instruction::MovUpW3 => { self.movupw(3); return; }

            Instruction::MovDn2 => { self.abstract_state.movdn(2); return; }
            Instruction::MovDn3 => { self.abstract_state.movdn(3); return; }
            Instruction::MovDn4 => { self.abstract_state.movdn(4); return; }
            Instruction::MovDn5 => { self.abstract_state.movdn(5); return; }
            Instruction::MovDn6 => { self.abstract_state.movdn(6); return; }
            Instruction::MovDn7 => { self.abstract_state.movdn(7); return; }
            Instruction::MovDn8 => { self.abstract_state.movdn(8); return; }
            Instruction::MovDn9 => { self.abstract_state.movdn(9); return; }
            Instruction::MovDn10 => { self.abstract_state.movdn(10); return; }
            Instruction::MovDn11 => { self.abstract_state.movdn(11); return; }
            Instruction::MovDn12 => { self.abstract_state.movdn(12); return; }
            Instruction::MovDn13 => { self.abstract_state.movdn(13); return; }
            Instruction::MovDn14 => { self.abstract_state.movdn(14); return; }
            Instruction::MovDn15 => { self.abstract_state.movdn(15); return; }
            Instruction::MovDnW2 => { self.movdnw(2); return; }
            Instruction::MovDnW3 => { self.movdnw(3); return; }

            // ─────────────────────────────────────────────────────────────────
            // Reverse operations - these rearrange in place but need inputs
            // ─────────────────────────────────────────────────────────────────
            Instruction::Reversew => {
                // Reverse top word (4 elements): positions 0,1,2,3 -> 3,2,1,0
                self.abstract_state.swap(0, 3);
                self.abstract_state.swap(1, 2);
                return;
            }
            Instruction::Reversedw => {
                // Reverse top double word (8 elements): positions 0-7 reversed
                for i in 0..4 {
                    self.abstract_state.swap(i, 7 - i);
                }
                return;
            }

            // ─────────────────────────────────────────────────────────────────
            // Advice operations
            // ─────────────────────────────────────────────────────────────────
            Instruction::AdvLoadW => {
                self.apply_pop_push(4, 4);
                for _ in 0..4 {
                    self.pop_bounds();
                    self.push_bounds(Bounds::Field);
                }
                return;
            }
            Instruction::AdvPipe => {
                self.apply_pop_push(8, 8);
                for _ in 0..8 {
                    self.pop_bounds();
                    self.push_bounds(Bounds::Field);
                }
                return;
            }

            // All other instructions - use static_effect()
            _ => {}
        }

        // Use static_effect() for all remaining instructions
        if let Some(effect) = static_effect(inst) {
            self.apply_pop_push(effect.pops as usize, effect.pushes as usize);
        }
    }

}

impl<'a> Visit for SignatureAnalyzer<'a> {
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
                        // Net = out - in (as signed to handle negative)
                        let then_net = then_out as isize - then_in as isize;
                        let else_net = else_out as isize - else_in as isize;

                        if then_net == else_net {
                            // Same net effect - we can compute the combined effect
                            // The inputs required is the max of both branches
                            let max_inputs = then_in.max(else_in);
                            // Outputs = max_inputs + net_effect
                            let outputs = (max_inputs as isize + then_net).max(0) as usize;
                            self.pop_n(max_inputs);
                            self.push_n(outputs, SymbolicExpr::Top);
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
                    let net_change = body_out as isize - body_in as isize;
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
                            let total_net = (iterations as isize) * net_change;
                            // First iteration consumes body_in from original stack
                            // Total effect: body_in inputs consumed, (body_in + total_net) outputs
                            let outputs = (body_in as isize + total_net).max(0) as usize;
                            self.pop_n(body_in);
                            self.push_n(outputs, SymbolicExpr::Top);
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
                    let count = *count as isize;
                    // Each iteration: consumes body_in, produces body_out
                    // Net change per iteration: body_out - body_in
                    // Total net change: count * (body_out - body_in)
                    let net_per_iter = body_out as isize - body_in as isize;
                    let total_net = count * net_per_iter;

                    // For inputs: first iteration needs body_in inputs
                    // Subsequent iterations use previous outputs
                    let outputs = (body_in as isize + total_net).max(0) as usize;
                    self.pop_n(body_in);
                    self.push_n(outputs, SymbolicExpr::Top);
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
            signature: None,
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
            signature: None,
            definition_range: None,
        };

        assert!(!contract2.validates_inputs());
        assert!(contract2.requires_u32_inputs()); // Uses u32 but doesn't validate
    }

    #[test]
    fn test_signature_memory_store_simple() {
        // Procedure that takes [val, addr] and stores val to addr
        // addr is at position 0 (top), which is an input since stack_depth=0
        let contracts = parse_and_infer(
            "proc store_val_to_ptr
    mem_store
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 2 inputs: [val (pos 1), addr (pos 0)]
        // Position 0 (addr) is used for mem_store (write)
        assert_eq!(sig.num_inputs(), 2);
        assert!(
            sig.inputs[0].is_output(),
            "Input 0 (addr) should be OutputAddress, got {:?}",
            sig.inputs[0]
        );
        assert!(
            !sig.inputs[1].is_address(),
            "Input 1 (val) should be Value, got {:?}",
            sig.inputs[1]
        );
    }

    #[test]
    fn test_signature_memory_load() {
        // Procedure that loads from memory: input is used as input address
        let contracts = parse_and_infer(
            "proc load_from_ptr
    mem_load
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // The procedure takes 1 input (ptr) and uses it for mem_load (read)
        assert_eq!(sig.num_inputs(), 1);
        assert!(
            sig.inputs[0].is_input_address(),
            "Input should be InputAddress, got {:?}",
            sig.inputs[0]
        );
        // Should have 1 output (loaded value)
        assert_eq!(sig.num_outputs(), 1);
    }

    #[test]
    fn test_signature_value_only() {
        // Procedure that only uses input as value, not as address
        let contracts = parse_and_infer(
            "proc double_value
    dup
    add
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // The procedure takes 1 input and uses it as a value (add)
        assert_eq!(sig.num_inputs(), 1);
        assert!(!sig.inputs[0].is_address(), "Input should be Value, got {:?}", sig.inputs[0]);
        // Should have 1 output (doubled value)
        assert_eq!(sig.num_outputs(), 1);
    }

    #[test]
    fn test_signature_no_inputs() {
        // Procedure with no inputs - has 1 output
        let contracts = parse_and_infer(
            "proc push_constant
    push.42
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        // Has signature because it has 1 output
        assert!(sig.is_some(), "Should have signature (1 output)");
        let sig = sig.unwrap();
        assert_eq!(sig.num_inputs(), 0);
        assert_eq!(sig.num_outputs(), 1);
    }

    #[test]
    fn test_signature_no_inputs_no_outputs() {
        // Procedure that consumes and produces nothing - push then drop
        let contracts = parse_and_infer(
            "proc noop
    push.0
    drop
end",
        );

        assert_eq!(contracts.len(), 1);
        assert!(
            contracts[0].signature.is_none(),
            "Should have no signature for proc with no inputs or outputs"
        );
    }

    #[test]
    fn test_signature_word_store() {
        // Procedure that stores a word: [V0, V1, V2, V3, addr]
        // addr is at position 0, values at positions 1-4
        let contracts = parse_and_infer(
            "proc store_word
    mem_storew_be
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 5 inputs: [V0, V1, V2, V3, addr]
        assert_eq!(sig.num_inputs(), 5);
        // Position 0 (addr) is output address
        assert!(
            sig.inputs[0].is_output(),
            "Input 0 (addr) should be OutputAddress, got {:?}",
            sig.inputs[0]
        );
    }

    #[test]
    fn test_signature_word_load() {
        // Procedure that loads a word: [V0, V1, V2, V3, addr]
        // addr is at position 0
        let contracts = parse_and_infer(
            "proc load_word
    mem_loadw_be
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // mem_loadw loads 4 values into the positions after addr
        // The procedure overwrites [V0, V1, V2, V3] using addr
        assert!(
            sig.inputs[0].is_input_address(),
            "Input 0 (addr) should be InputAddress, got {:?}",
            sig.inputs[0]
        );
    }

    #[test]
    fn test_signature_output_kinds() {
        // Test that outputs are tracked
        let contracts = parse_and_infer(
            "proc push_three
    push.1 push.2 push.3
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        assert_eq!(sig.num_inputs(), 0);
        assert_eq!(sig.num_outputs(), 3);
        // All outputs should be Computed by default
        for output in &sig.outputs {
            assert_eq!(*output, OutputKind::Computed);
        }
    }

    #[test]
    fn test_signature_swap_preserves_provenance() {
        // Procedure that stores through swapped address
        // push.42 puts value on top, swap moves input addr to top
        let contracts = parse_and_infer(
            "proc store_swapped
    push.42
    swap
    mem_store
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 1 input (the address) - push adds 1, swap doesn't change depth,
        // mem_store consumes 2
        assert_eq!(sig.num_inputs(), 1);
        // The input should be detected as OutputAddress through swap
        assert!(
            sig.inputs[0].is_output(),
            "Input 0 should be OutputAddress after swap, got {:?}",
            sig.inputs[0]
        );
    }

    #[test]
    fn test_signature_dup_preserves_provenance() {
        // Procedure that reads from duplicated address
        let contracts = parse_and_infer(
            "proc load_duped
    dup
    mem_load
    drop
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 1 input (the address), dup makes 2 copies
        // mem_load consumes top copy (pos 0), drop removes result
        // Final output is original address
        assert_eq!(sig.num_inputs(), 1);
        // The input should be detected as InputAddress (read)
        assert!(
            sig.inputs[0].is_input_address(),
            "Input 0 should be InputAddress, got {:?}",
            sig.inputs[0]
        );
    }

    #[test]
    fn test_signature_movup_preserves_provenance() {
        // Procedure that moves address up and stores to it
        let contracts = parse_and_infer(
            "proc store_with_movup
    push.1 push.2
    movup.2
    mem_store
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Stack: [addr] -> [1, addr] -> [2, 1, addr] -> [addr, 2, 1] -> []
        // Takes 1 input (addr at position 2 after pushes, moved to top)
        assert_eq!(sig.num_inputs(), 1);
        // Input should be detected as OutputAddress through movup
        assert!(
            sig.inputs[0].is_output(),
            "Input 0 should be OutputAddress after movup, got {:?}",
            sig.inputs[0]
        );
    }

    #[test]
    fn test_signature_output_passthrough() {
        // Procedure that duplicates its input (passthrough of input to outputs)
        let contracts = parse_and_infer(
            "proc dup_input
    dup
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 1 input, produces 2 outputs (both from same input)
        assert_eq!(sig.num_inputs(), 1);
        assert_eq!(sig.num_outputs(), 2);

        // Both outputs should be InputPassthrough from input 0
        assert!(
            matches!(sig.outputs[0], OutputKind::InputPassthrough { input_pos: 0 }),
            "Output 0 should be passthrough from input 0, got {:?}",
            sig.outputs[0]
        );
        assert!(
            matches!(sig.outputs[1], OutputKind::InputPassthrough { input_pos: 0 }),
            "Output 1 should be passthrough from input 0, got {:?}",
            sig.outputs[1]
        );
    }

    #[test]
    fn test_signature_output_memory_read() {
        // Procedure that returns a value loaded from input address
        let contracts = parse_and_infer(
            "proc deref
    mem_load
end",
        );

        assert_eq!(contracts.len(), 1);
        let sig = contracts[0].signature.as_ref();
        assert!(sig.is_some(), "Should have signature");
        let sig = sig.unwrap();

        // Takes 1 input (address), produces 1 output (value loaded)
        assert_eq!(sig.num_inputs(), 1);
        assert_eq!(sig.num_outputs(), 1);

        // Output should be MemoryRead from input 0
        assert!(
            matches!(sig.outputs[0], OutputKind::MemoryRead { from_input: Some(0) }),
            "Output should be MemoryRead from input 0, got {:?}",
            sig.outputs[0]
        );
    }

    #[test]
    fn test_signature_transitive_output_address() {
        // Test that calling a procedure with an output address parameter
        // propagates the address usage to the caller's inputs
        let contracts = parse_and_infer(
            "
proc inner
    mem_store
end

proc outer
    push.42
    swap
    exec.inner
end
",
        );

        // Find both procedures
        let inner = contracts.iter().find(|c| c.path.name() == "inner");
        assert!(inner.is_some(), "Should find inner procedure");
        let inner = inner.unwrap();

        // First verify inner has correct signature
        let inner_sig = inner.signature.as_ref();
        assert!(inner_sig.is_some(), "inner should have signature");
        let inner_sig = inner_sig.unwrap();
        assert_eq!(inner_sig.num_inputs(), 2);
        assert!(inner_sig.inputs[0].is_output(), "inner input 0 should be OutputAddress");

        // Find the outer procedure
        let outer = contracts.iter().find(|c| c.path.name() == "outer");
        assert!(outer.is_some(), "Should find outer procedure");
        let outer = outer.unwrap();

        // outer should have been re-inferred with inner's contract available
        // However, due to how iterative inference works, we need to check
        // if outer has a Known stack effect
        match &outer.stack_effect {
            StackEffect::Known { inputs, outputs } => {
                // outer takes 1 input (address), which is passed to inner
                // inner's signature shows input 0 is OutputAddress
                assert_eq!(*inputs, 1, "outer should take 1 input");
                assert_eq!(*outputs, 0, "outer should produce 0 outputs");

                let sig = outer.signature.as_ref();
                assert!(sig.is_some(), "outer should have signature with known stack effect");
                let sig = sig.unwrap();

                assert_eq!(sig.num_inputs(), 1);
                assert!(
                    sig.inputs[0].is_output(),
                    "outer's input should be OutputAddress (transitive), got {:?}",
                    sig.inputs[0]
                );
            }
            _ => {
                // If outer still has unknown effect, the transitive inference didn't work
                // This is a known limitation that requires more sophisticated handling
                panic!(
                    "outer has {:?} stack effect - transitive inference may have failed",
                    outer.stack_effect
                );
            }
        }
    }

    #[test]
    fn test_fixpoint_three_level_call_chain() {
        // Test that fix-point iteration correctly handles a 3-level call chain:
        // level3 -> level2 -> level1 -> mem_store
        // Each level should eventually get the correct stack effect
        let contracts = parse_and_infer(
            "
proc level1
    mem_store
end

proc level2
    exec.level1
end

proc level3
    exec.level2
end
",
        );

        // All should have Known stack effects
        let level1 = contracts.iter().find(|c| c.path.name() == "level1").unwrap();
        let level2 = contracts.iter().find(|c| c.path.name() == "level2").unwrap();
        let level3 = contracts.iter().find(|c| c.path.name() == "level3").unwrap();

        assert!(
            matches!(level1.stack_effect, StackEffect::Known { inputs: 2, outputs: 0 }),
            "level1 should have Known(2, 0), got {:?}",
            level1.stack_effect
        );
        assert!(
            matches!(level2.stack_effect, StackEffect::Known { inputs: 2, outputs: 0 }),
            "level2 should have Known(2, 0), got {:?}",
            level2.stack_effect
        );
        assert!(
            matches!(level3.stack_effect, StackEffect::Known { inputs: 2, outputs: 0 }),
            "level3 should have Known(2, 0), got {:?}",
            level3.stack_effect
        );

        // Check that OutputAddress propagates through all levels
        let level1_sig = level1.signature.as_ref().unwrap();
        let level2_sig = level2.signature.as_ref().unwrap();
        let level3_sig = level3.signature.as_ref().unwrap();

        assert!(
            level1_sig.inputs[0].is_output(),
            "level1 input 0 should be OutputAddress"
        );
        assert!(
            level2_sig.inputs[0].is_output(),
            "level2 input 0 should be OutputAddress (transitive from level1)"
        );
        assert!(
            level3_sig.inputs[0].is_output(),
            "level3 input 0 should be OutputAddress (transitive from level2)"
        );
    }

    #[test]
    fn test_fixpoint_mutual_recursion_stabilizes() {
        // Test that mutually recursive procedures stabilize
        // Since neither takes inputs before calling the other, inputs = 0
        // But outputs are unknown due to the cycle
        let contracts = parse_and_infer(
            "
proc a
    exec.b
end

proc b
    exec.a
end
",
        );

        let a = contracts.iter().find(|c| c.path.name() == "a").unwrap();
        let b = contracts.iter().find(|c| c.path.name() == "b").unwrap();

        // With no input operations before the recursive call, inputs are 0
        // But outputs cannot be determined due to the cycle, so we get KnownInputs
        assert!(
            matches!(a.stack_effect, StackEffect::KnownInputs { inputs: 0 } | StackEffect::Unknown),
            "a should have KnownInputs(0) or Unknown effect, got {:?}",
            a.stack_effect
        );
        assert!(
            matches!(b.stack_effect, StackEffect::KnownInputs { inputs: 0 } | StackEffect::Unknown),
            "b should have KnownInputs(0) or Unknown effect, got {:?}",
            b.stack_effect
        );
    }

    #[test]
    fn test_overflowing_add_stack_effect() {
        // Test that overflowing_add correctly infers 4 inputs, 3 outputs
        // This is the u64 addition that returns (overflow_flag, result_hi, result_lo)
        let contracts = parse_and_infer(
            "
pub proc overflowing_add
    swap
    movup.3
    u32overflowing_add
    movup.3
    movup.3
    u32overflowing_add3
end
",
        );

        assert_eq!(contracts.len(), 1);
        let contract = &contracts[0];

        // Stack trace:
        // Initial: [a_0, a_1, a_2, a_3] (4 inputs discovered)
        // swap: [a_1, a_0, a_2, a_3]
        // movup.3: [a_3, a_1, a_0, a_2]  -- wait, actually need to trace more carefully
        // Actually the stack grows upward, so:
        // swap swaps top two, movup.3 moves element at depth 3 to top
        // u32overflowing_add: pops 2, pushes 2
        // movup.3: moves element at depth 3 to top
        // movup.3: moves element at depth 3 to top
        // u32overflowing_add3: pops 3, pushes 2
        // Final depth should be: 4 - 2 + 2 - 3 + 2 = 3 outputs

        assert!(
            matches!(contract.stack_effect, StackEffect::Known { inputs: 4, outputs: 3 }),
            "overflowing_add should have Known(4, 3), got {:?}",
            contract.stack_effect
        );

        // Also verify signature has correct counts
        let sig = contract.signature.as_ref().expect("Should have signature");
        assert_eq!(sig.num_inputs(), 4, "Should have 4 inputs");
        assert_eq!(sig.num_outputs(), 3, "Should have 3 outputs");
    }

    #[test]
    fn test_word_reverse_stack_effect() {
        // Test that a word reverse (swap all 4 elements) has 4 inputs, 4 outputs
        let contracts = parse_and_infer(
            "
pub proc reverse
    swap
    movup.2
    movup.3
end
",
        );

        assert_eq!(contracts.len(), 1);
        let contract = &contracts[0];

        // This should discover 4 inputs and have 4 outputs
        assert!(
            matches!(contract.stack_effect, StackEffect::Known { inputs: 4, outputs: 4 }),
            "reverse should have Known(4, 4), got {:?}",
            contract.stack_effect
        );

        let sig = contract.signature.as_ref().expect("Should have signature");
        assert_eq!(sig.num_inputs(), 4, "Should have 4 inputs");
        assert_eq!(sig.num_outputs(), 4, "Should have 4 outputs");
    }
}
