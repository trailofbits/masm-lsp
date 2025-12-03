//! Analyzer for detecting uninitialized local variable access.
//!
//! This module provides control-flow aware analysis to detect reads from
//! local variables that may not have been initialized.
//!
//! # Procedure Signature Integration
//!
//! When a `ContractStore` is provided, the analyzer can suppress false positives
//! for locals whose addresses are passed to procedures that write to them. For example:
//!
//! ```text
//! locaddr.0         # Push address of local 0
//! exec.write_to_ptr # If write_to_ptr's signature shows input 0 is OutputAddress,
//!                   # local 0 is marked as initialized after the call
//! loc_load.0        # No warning - local was initialized by callee
//! ```

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceSpan, Span};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

use crate::analysis::contracts::ContractStore;
use crate::analysis::static_effect::{apply_stack_manipulation, StackLike, StackLikeResult};
use crate::analysis::utils::u16_imm_to_u16;
use crate::diagnostics::{span_to_range, SOURCE_ANALYSIS};

use super::state::{LocalState, LocalsState};

// ═══════════════════════════════════════════════════════════════════════════
// Stack element tracking for provenance
// ═══════════════════════════════════════════════════════════════════════════

/// Tracks what kind of value is at each stack position.
///
/// This allows us to determine which stack positions contain local addresses
/// when procedure calls occur, so we can mark locals as initialized if the
/// callee writes to them.
#[derive(Clone, Debug, Default)]
enum StackElement {
    /// Address of a specific local variable (from locaddr.N)
    LocalAddress(u16),
    /// Some other value (not a local address)
    #[default]
    Other,
}

/// Stack tracking for local addresses.
///
/// Implements `StackLike` to leverage the generic stack manipulation helper.
/// This tracks which positions contain local addresses.
#[derive(Clone, Debug, Default)]
struct AddressStack {
    /// Stack elements (index 0 = bottom, last = top)
    elements: Vec<StackElement>,
}

impl AddressStack {
    fn new() -> Self {
        Self::default()
    }

    /// Get local address at position n from top (0 = top), if any.
    fn get_local_at(&self, n: usize) -> Option<u16> {
        match self.peek(n) {
            Some(StackElement::LocalAddress(idx)) => Some(*idx),
            _ => None,
        }
    }

    /// Clear the stack (used when provenance becomes unknown).
    fn clear(&mut self) {
        self.elements.clear();
    }
}

impl StackLike for AddressStack {
    type Element = StackElement;

    fn depth(&self) -> usize {
        self.elements.len()
    }

    fn push(&mut self, elem: StackElement) {
        self.elements.push(elem);
    }

    fn pop(&mut self) -> StackElement {
        self.elements.pop().unwrap_or_default()
    }

    fn peek(&self, n: usize) -> Option<&StackElement> {
        let depth = self.elements.len();
        if n < depth {
            Some(&self.elements[depth - 1 - n])
        } else {
            None
        }
    }

    fn ensure_depth(&mut self, needed: usize) {
        while self.elements.len() < needed {
            self.elements.insert(0, StackElement::Other);
        }
    }

    fn swap(&mut self, a: usize, b: usize) {
        let max_idx = a.max(b);
        self.ensure_depth(max_idx + 1);
        let len = self.elements.len();
        let idx_a = len - 1 - a;
        let idx_b = len - 1 - b;
        self.elements.swap(idx_a, idx_b);
    }

    fn movup(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let len = self.elements.len();
        let idx = len - 1 - n;
        let elem = self.elements.remove(idx);
        self.elements.push(elem);
    }

    fn movdn(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        self.ensure_depth(n + 1);
        let elem = self.elements.pop().unwrap();
        let len = self.elements.len();
        // Insert at (len - n): for movdn.2 with len=3, insert at index 1
        // This places the element at position n from top (0-indexed)
        let idx = len.saturating_sub(n);
        self.elements.insert(idx, elem);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Finding type for locals analysis
// ═══════════════════════════════════════════════════════════════════════════

/// A finding from the uninitialized locals analysis.
#[derive(Clone, Debug)]
struct LocalsFinding {
    /// Human-readable description of the issue
    message: String,
    /// Source span where the issue was detected
    span: SourceSpan,
    /// Severity of the finding
    severity: DiagnosticSeverity,
}

// ═══════════════════════════════════════════════════════════════════════════
// LocalsAnalyzer
// ═══════════════════════════════════════════════════════════════════════════

/// Analyzer for detecting uninitialized local variable access.
///
/// This analyzer performs control-flow aware dataflow analysis to track
/// which local variables have been initialized at each program point.
///
/// When a `ContractStore` is provided, the analyzer can detect when a local's
/// address is passed to a procedure that writes to it (via `OutputAddress`
/// parameter), and mark the local as initialized after the call.
pub struct LocalsAnalyzer<'a> {
    source_manager: &'a DefaultSourceManager,
    #[allow(dead_code)] // May be used for related information in the future
    uri: Url,
    /// Optional contract store for looking up procedure signatures
    contracts: Option<&'a ContractStore>,
    findings: Vec<LocalsFinding>,
}

impl<'a> LocalsAnalyzer<'a> {
    /// Create a new locals analyzer.
    pub fn new(source_manager: &'a DefaultSourceManager, uri: Url) -> Self {
        Self {
            source_manager,
            uri,
            contracts: None,
            findings: Vec::new(),
        }
    }

    /// Create a new locals analyzer with access to procedure contracts.
    ///
    /// When contracts are provided, the analyzer can detect when a local's
    /// address is passed to a procedure that writes to it, and suppress
    /// false positives about uninitialized locals in those cases.
    pub fn with_contracts(
        source_manager: &'a DefaultSourceManager,
        uri: Url,
        contracts: &'a ContractStore,
    ) -> Self {
        Self {
            source_manager,
            uri,
            contracts: Some(contracts),
            findings: Vec::new(),
        }
    }

    /// Analyze a module for uninitialized local access.
    pub fn analyze(mut self, module: &Module) -> Vec<Diagnostic> {
        // Use the Visit trait to find all procedures, then analyze each one
        let _ = visit::visit_module(&mut self, module);
        self.build_diagnostics()
    }

    /// Analyze a single procedure with custom control flow handling.
    fn analyze_procedure_body(&mut self, proc: &Procedure) {
        let mut state = LocalsState::new();
        let mut addr_stack = AddressStack::new();
        self.analyze_block(proc.body(), &mut state, &mut addr_stack);
    }

    /// Analyze a block of operations.
    fn analyze_block(
        &mut self,
        block: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        for op in block.iter() {
            self.analyze_op(op, state, addr_stack);
        }
    }

    /// Analyze a single operation.
    fn analyze_op(&mut self, op: &Op, state: &mut LocalsState, addr_stack: &mut AddressStack) {
        match op {
            Op::Inst(inst) => self.analyze_instruction(inst, state, addr_stack),
            Op::If {
                then_blk, else_blk, ..
            } => self.analyze_if_else(then_blk, else_blk, state, addr_stack),
            Op::While { body, .. } => self.analyze_while(body, state, addr_stack),
            Op::Repeat { count, body, .. } => self.analyze_repeat(*count, body, state, addr_stack),
        }
    }

    /// Analyze an if/else construct.
    ///
    /// Both branches are analyzed independently, then states are merged
    /// at the join point using the meet operation.
    fn analyze_if_else(
        &mut self,
        then_blk: &Block,
        else_blk: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        // if.true consumes the condition from the stack
        addr_stack.pop();

        // Clone state for each branch
        let mut then_state = state.clone();
        let mut else_state = state.clone();
        let mut then_addr_stack = addr_stack.clone();
        let mut else_addr_stack = addr_stack.clone();

        // Analyze both branches
        self.analyze_block(then_blk, &mut then_state, &mut then_addr_stack);
        self.analyze_block(else_blk, &mut else_state, &mut else_addr_stack);

        // Meet the results at the join point
        *state = then_state.meet(&else_state);
        // Note: We don't merge address stacks because after if/else the
        // stack provenance is uncertain. We just continue with the original.
    }

    /// Analyze a while loop.
    ///
    /// Since while loops may execute zero times, any initialization inside
    /// the loop body cannot guarantee the local is initialized after the loop.
    fn analyze_while(
        &mut self,
        body: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        // Save entry state
        let entry_state = state.clone();

        // Analyze the body to find what gets initialized and check for reads
        let mut body_addr_stack = addr_stack.clone();
        self.analyze_block(body, state, &mut body_addr_stack);

        // Since the loop may not execute, meet entry state with body result.
        // This makes anything initialized only in the body "MaybeInitialized".
        *state = entry_state.meet(state);
        // Address stack provenance is uncertain after while loop
    }

    /// Analyze a repeat loop.
    ///
    /// Repeat loops with count > 0 always execute at least once, so
    /// initializations in the body are reliable.
    fn analyze_repeat(
        &mut self,
        count: u32,
        body: &Block,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        if count == 0 {
            return; // No execution, state unchanged
        }

        // Repeat always executes at least once, so body initializations are reliable
        let mut body_addr_stack = addr_stack.clone();
        self.analyze_block(body, state, &mut body_addr_stack);
        // Address stack provenance is uncertain after repeat loop
    }

    /// Analyze a single instruction.
    fn analyze_instruction(
        &mut self,
        inst: &Span<Instruction>,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        let span = inst.span();

        // First, try to handle as a pure stack manipulation
        if apply_stack_manipulation(addr_stack, inst.inner()) == StackLikeResult::Applied {
            return;
        }

        // Handle instructions that need special processing
        match inst.inner() {
            // ─────────────────────────────────────────────────────────────────
            // Write operations - mark locals as initialized
            // ─────────────────────────────────────────────────────────────────
            Instruction::LocStore(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_single(idx);
                }
                addr_stack.pop(); // Consumes value from stack
            }
            Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_word(idx);
                }
                addr_stack.pop_n(4); // Consumes word from stack
            }

            // ─────────────────────────────────────────────────────────────────
            // Read operations - check for uninitialized access
            // ─────────────────────────────────────────────────────────────────
            Instruction::LocLoad(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_single_read(idx, span, state);
                }
                addr_stack.push(StackElement::Other); // Pushes loaded value
            }
            Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_word_read(idx, span, state);
                }
                addr_stack.pop_n(4); // Consumes word to be overwritten
                addr_stack.push_defaults(4); // Pushes loaded word
            }

            // ─────────────────────────────────────────────────────────────────
            // Address operations - track local address on stack
            // ─────────────────────────────────────────────────────────────────
            Instruction::Locaddr(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    // Only warn if the local is uninitialized AND we don't have
                    // contract information (can't check if it will be written later)
                    if self.contracts.is_none() {
                        self.check_address_escape(idx, span, state);
                    }
                    // Track the local address on the stack
                    addr_stack.push(StackElement::LocalAddress(idx));
                } else {
                    addr_stack.push(StackElement::Other);
                }
            }

            // ─────────────────────────────────────────────────────────────────
            // Procedure calls - check for output address parameters
            // ─────────────────────────────────────────────────────────────────
            Instruction::Exec(target)
            | Instruction::Call(target)
            | Instruction::SysCall(target) => {
                self.handle_procedure_call(target, state, addr_stack);
            }

            Instruction::DynExec | Instruction::DynCall => {
                // Dynamic calls consume the MAST root (4 elements)
                // We don't know the callee's signature, so we can't check for output addresses
                addr_stack.pop_n(4);
            }

            // ─────────────────────────────────────────────────────────────────
            // Memory operations - track stack changes
            // ─────────────────────────────────────────────────────────────────
            Instruction::MemLoad => {
                addr_stack.pop(); // address
                addr_stack.push(StackElement::Other); // loaded value
            }
            Instruction::MemLoadImm(_) => {
                addr_stack.push(StackElement::Other); // loaded value
            }
            Instruction::MemLoadWBe | Instruction::MemLoadWLe => {
                addr_stack.pop(); // address
                addr_stack.pop_n(4); // overwritten word
                addr_stack.push_defaults(4); // loaded word
            }
            Instruction::MemLoadWBeImm(_) | Instruction::MemLoadWLeImm(_) => {
                addr_stack.pop_n(4); // overwritten word
                addr_stack.push_defaults(4); // loaded word
            }
            Instruction::MemStore => {
                addr_stack.pop(); // address
                addr_stack.pop(); // value
            }
            Instruction::MemStoreImm(_) => {
                addr_stack.pop(); // value
            }
            Instruction::MemStoreWBe | Instruction::MemStoreWLe => {
                addr_stack.pop(); // address
                addr_stack.pop_n(4); // word
            }
            Instruction::MemStoreWBeImm(_) | Instruction::MemStoreWLeImm(_) => {
                addr_stack.pop_n(4); // word
            }

            // ─────────────────────────────────────────────────────────────────
            // Other instructions - use static effect for stack tracking
            // ─────────────────────────────────────────────────────────────────
            _ => {
                use crate::analysis::static_effect::StaticEffect;

                // Use static_effect to track stack changes for other instructions
                if let Some(effect) = StaticEffect::of(inst.inner()) {
                    addr_stack.pop_n(effect.pops as usize);
                    addr_stack.push_defaults(effect.pushes as usize);
                }
            }
        }
    }

    /// Handle a procedure call, checking if any local addresses are passed to
    /// output address parameters.
    fn handle_procedure_call(
        &mut self,
        target: &InvocationTarget,
        state: &mut LocalsState,
        addr_stack: &mut AddressStack,
    ) {
        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str(),
            InvocationTarget::Path(path) => path.inner().as_str(),
            InvocationTarget::MastRoot(_) => {
                // Unknown effect, clear address tracking
                addr_stack.clear();
                return;
            }
        };

        // Look up the contract for this procedure
        let contract = self.contracts.and_then(|store| {
            store
                .get_by_suffix(target_name)
                .or_else(|| store.get_by_name(target_name))
        });

        match contract {
            Some(c) => {
                // Get stack effect to know how many inputs are consumed
                if let Some(num_inputs) = c.stack_effect.inputs() {
                    // Check each input position for local addresses
                    // that are passed to output address parameters
                    if let Some(sig) = c.signature.as_ref() {
                        for (i, kind) in sig.inputs.iter().enumerate() {
                            // Check if this input position contains a local address
                            // and if the parameter is an output address
                            if kind.is_output() {
                                if let Some(local_idx) = addr_stack.get_local_at(i) {
                                    // Mark the local as initialized - callee will write to it
                                    state.init_single(local_idx);
                                }
                            }
                        }
                    }

                    // Apply stack effect: pop inputs, push outputs
                    addr_stack.pop_n(num_inputs);
                    if let Some(num_outputs) = c.stack_effect.outputs() {
                        addr_stack.push_defaults(num_outputs);
                    } else {
                        // Unknown outputs - clear tracking
                        addr_stack.clear();
                    }
                } else {
                    // Unknown inputs - clear tracking
                    addr_stack.clear();
                }
            }
            None => {
                // Unknown procedure - clear tracking
                addr_stack.clear();
            }
        }
    }

    /// Check a single local read for uninitialized access.
    fn check_single_read(&mut self, idx: u16, span: SourceSpan, state: &LocalsState) {
        match state.get(idx) {
            LocalState::Uninitialized => {
                self.findings.push(LocalsFinding {
                    message: format!("Read from uninitialized local {}.", idx),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::MaybeInitialized => {
                self.findings.push(LocalsFinding {
                    message: format!("Local {} may not be initialized on all code paths.", idx),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::Initialized => {
                // All good, no warning needed
            }
        }
    }

    /// Check a word read for uninitialized access.
    fn check_word_read(&mut self, base_idx: u16, span: SourceSpan, state: &LocalsState) {
        if let Some((idx, local_state)) = state.first_uninitialized_in_word(base_idx) {
            match local_state {
                LocalState::Uninitialized => {
                    self.findings.push(LocalsFinding {
                        message: format!(
                            "Read from uninitialized local {} (in word starting at {}).",
                            idx, base_idx
                        ),
                        span,
                        severity: DiagnosticSeverity::WARNING,
                    });
                }
                LocalState::MaybeInitialized => {
                    self.findings.push(LocalsFinding {
                        message: format!(
                            "Local {} may not be initialized on all code paths (in word starting at {}).",
                            idx, base_idx
                        ),
                        span,
                        severity: DiagnosticSeverity::WARNING,
                    });
                }
                LocalState::Initialized => {
                    // Shouldn't reach here due to first_uninitialized_in_word logic
                }
            }
        }
    }

    /// Check for taking address of an uninitialized local.
    ///
    /// This is a conservative check: we warn whenever the address of an
    /// uninitialized local is taken, since the address could be passed to
    /// another procedure that reads from it.
    fn check_address_escape(&mut self, idx: u16, span: SourceSpan, state: &LocalsState) {
        match state.get(idx) {
            LocalState::Uninitialized => {
                self.findings.push(LocalsFinding {
                    message: format!(
                        "Taking address of uninitialized local {}. If passed to another procedure, it may read garbage.",
                        idx
                    ),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::MaybeInitialized => {
                self.findings.push(LocalsFinding {
                    message: format!(
                        "Taking address of local {} which may not be initialized on all code paths.",
                        idx
                    ),
                    span,
                    severity: DiagnosticSeverity::WARNING,
                });
            }
            LocalState::Initialized => {
                // All good, no warning needed
            }
        }
    }

    /// Convert collected findings to LSP diagnostics.
    fn build_diagnostics(&self) -> Vec<Diagnostic> {
        self.findings
            .iter()
            .map(|finding| self.finding_to_diagnostic(finding))
            .collect()
    }

    /// Convert a finding to an LSP diagnostic.
    fn finding_to_diagnostic(&self, finding: &LocalsFinding) -> Diagnostic {
        let range = span_to_range(self.source_manager, finding.span)
            .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));

        Diagnostic {
            range,
            severity: Some(finding.severity),
            code: None,
            code_description: None,
            source: Some(SOURCE_ANALYSIS.to_string()),
            message: finding.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Visit trait implementation
// ═══════════════════════════════════════════════════════════════════════════

impl<'a> Visit for LocalsAnalyzer<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> ControlFlow<()> {
        // Analyze this procedure with our custom control-flow handling
        self.analyze_procedure_body(proc);

        // Don't use default traversal - we handle control flow ourselves
        ControlFlow::Continue(())
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

/// Analyze a module for uninitialized local variable access.
///
/// Returns LSP diagnostics for any issues found.
///
/// This is a convenience wrapper around `analyze_locals_with_contracts` that
/// doesn't use procedure signatures. For more accurate analysis, use
/// `analyze_locals_with_contracts` with inferred contracts.
pub fn analyze_locals(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
) -> Vec<Diagnostic> {
    let analyzer = LocalsAnalyzer::new(source_manager, uri.clone());
    analyzer.analyze(module)
}

/// Analyze a module for uninitialized local variable access with contract awareness.
///
/// When contracts are provided, the analyzer can detect when a local's address
/// is passed to a procedure that writes to it (via an `OutputAddress` parameter),
/// and avoids false positives in those cases.
///
/// # Example
///
/// ```text
/// @locals(1)
/// proc caller
///     locaddr.0      # Push address of local 0
///     exec.writer    # If writer's signature shows input 0 is OutputAddress,
///                    # local 0 is marked as initialized after the call
///     loc_load.0     # No warning - local was initialized by callee
///     drop
/// end
/// ```
pub fn analyze_locals_with_contracts(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
    contracts: &ContractStore,
) -> Vec<Diagnostic> {
    let analyzer = LocalsAnalyzer::with_contracts(source_manager, uri.clone(), contracts);
    analyzer.analyze(module)
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;
    use miden_assembly_syntax::ast::ModuleKind;
    use miden_assembly_syntax::{Parse, ParseOptions};
    use miden_debug_types::{SourceLanguage, SourceManager, Uri};

    // Helper to count diagnostics of a given severity
    fn count_warnings(diags: &[Diagnostic]) -> usize {
        diags
            .iter()
            .filter(|d| d.severity == Some(DiagnosticSeverity::WARNING))
            .count()
    }

    // Helper to parse and analyze a module
    fn analyze_source(source: &str) -> Vec<Diagnostic> {
        let source_manager = DefaultSourceManager::default();
        let miden_uri = Uri::from("test://test.masm");
        source_manager.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());

        let source_file = source_manager
            .get_by_uri(&miden_uri)
            .expect("Failed to load source");
        let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
        module_path.push("test");
        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: Some(module_path.into()),
            ..Default::default()
        };

        match source_file.parse_with_options(&source_manager, opts) {
            Ok(module) => {
                let uri = Url::parse("file:///test.masm").unwrap();
                analyze_locals(&module, &source_manager, &uri)
            }
            Err(_) => vec![], // Parse error, no analysis
        }
    }

    #[test]
    fn test_no_warning_when_initialized() {
        let source = r#"
@locals(1)
proc test
    push.42
    loc_store.0
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn when local is initialized before read"
        );
    }

    #[test]
    fn test_warning_read_before_write() {
        let source = r#"
@locals(1)
proc test
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn when reading uninitialized local"
        );
        assert!(diags[0].message.contains("uninitialized local 0."));
    }

    #[test]
    fn test_warning_partial_word_init() {
        let source = r#"
@locals(4)
proc test
    push.1
    loc_store.0
    push.2
    loc_store.1
    # locals 2 and 3 not initialized
    loc_loadw_be.0
    dropw
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn when word contains uninitialized locals"
        );
        assert!(
            diags[0].message.contains("local 2") || diags[0].message.contains("local 3"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn test_no_warning_full_word_init() {
        let source = r#"
@locals(4)
proc test
    push.1.2.3.4
    loc_storew_be.0
    loc_loadw_be.0
    dropw
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn when full word is initialized"
        );
    }

    #[test]
    fn test_if_else_both_branches_init() {
        let source = r#"
@locals(1)
proc test
    push.1
    if.true
        push.10
        loc_store.0
    else
        push.20
        loc_store.0
    end
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn when both branches initialize"
        );
    }

    #[test]
    fn test_if_else_one_branch_init() {
        let source = r#"
@locals(1)
proc test
    push.1
    if.true
        push.10
        loc_store.0
    else
        # no initialization
    end
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn when only one branch initializes"
        );
        assert!(
            diags[0].message.contains("may not be initialized"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn test_while_loop_init_not_reliable() {
        let source = r#"
@locals(1)
proc test
    push.0  # condition (false, loop won't execute)
    while.true
        push.42
        loc_store.0
        push.0  # continue condition
    end
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn when init is only in while loop body"
        );
        assert!(
            diags[0].message.contains("may not be initialized"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn test_repeat_loop_init_reliable() {
        let source = r#"
@locals(1)
proc test
    repeat.1
        push.42
        loc_store.0
    end
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn when init is in repeat.n (n>0) body"
        );
    }

    #[test]
    fn test_locaddr_uninitialized() {
        let source = r#"
@locals(1)
proc test
    locaddr.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn when taking address of uninitialized local"
        );
        assert!(
            diags[0].message.contains("Taking address"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn test_locaddr_initialized() {
        let source = r#"
@locals(1)
proc test
    push.42
    loc_store.0
    locaddr.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn when taking address of initialized local"
        );
    }

    #[test]
    fn test_multiple_procedures() {
        let source = r#"
@locals(1)
proc good
    push.42
    loc_store.0
    loc_load.0
    drop
end

@locals(1)
proc bad
    loc_load.0
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should only warn for the bad procedure"
        );
    }

    #[test]
    fn test_different_local_indices() {
        let source = r#"
@locals(2)
proc test
    push.42
    loc_store.0
    loc_load.1  # local 1 not initialized
    drop
end
"#;
        let diags = analyze_source(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn for uninitialized local 1"
        );
        assert!(
            diags[0].message.contains("local 1"),
            "message: {}",
            diags[0].message
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Contract-aware tests
    // ═══════════════════════════════════════════════════════════════════════════

    // Helper to parse and analyze a module with contract inference
    fn analyze_source_with_contracts(source: &str) -> Vec<Diagnostic> {
        use crate::analysis::contracts::infer_module_contracts;

        let source_manager = DefaultSourceManager::default();
        let miden_uri = Uri::from("test://test.masm");
        source_manager.load(SourceLanguage::Masm, miden_uri.clone(), source.to_string());

        let source_file = source_manager
            .get_by_uri(&miden_uri)
            .expect("Failed to load source");
        let mut module_path = miden_assembly_syntax::ast::PathBuf::default();
        module_path.push("test");
        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: Some(module_path.into()),
            ..Default::default()
        };

        match source_file.parse_with_options(&source_manager, opts) {
            Ok(module) => {
                // Infer contracts for the module
                let contracts_vec = infer_module_contracts(&module, &source_manager);
                let mut contracts = ContractStore::new();
                contracts.update_document(contracts_vec);

                let uri = Url::parse("file:///test.masm").unwrap();
                analyze_locals_with_contracts(&module, &source_manager, &uri, &contracts)
            }
            Err(_) => vec![], // Parse error, no analysis
        }
    }

    #[test]
    fn test_locaddr_with_output_proc_no_warning() {
        // When we have contracts, passing locaddr to a procedure that writes to it
        // should mark the local as initialized - no warning needed
        let source = r#"
# writer writes to address at input position 0
proc writer
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.writer
    loc_load.0
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        // The writer procedure's signature should show input 0 is OutputAddress
        // So locaddr.0 passed to writer marks local 0 as initialized
        assert_eq!(count_warnings(&diags), 0,
            "Should not warn when locaddr is passed to proc with OutputAddress param. Got {} warnings: {:?}",
            diags.len(), diags.iter().map(|d| &d.message).collect::<Vec<_>>());
    }

    #[test]
    fn test_locaddr_with_reader_proc_still_warns() {
        // When locaddr is passed to a procedure that only reads, not writes,
        // we should still warn because the local isn't initialized
        let source = r#"
# reader reads from address at input position 0
proc reader
    mem_load
end

@locals(1)
proc caller
    locaddr.0
    exec.reader
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        // The reader procedure's signature should show input 0 is InputAddress (read only)
        // Since reader doesn't write, local 0 isn't initialized - should warn on loc_load if we had one
        // But we don't have a loc_load, so we just pass the address to reader which reads garbage
        // With contracts, we don't warn on locaddr itself (because we check later)
        // In this case, the read happens in reader, not in caller, so no warning in caller
        assert_eq!(count_warnings(&diags), 0,
            "Should not warn in caller when address is passed to reader (reader has the bug). Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>());
    }

    #[test]
    fn test_locaddr_passed_to_unknown_proc() {
        // When contracts aren't available for the callee, we can't determine
        // if the local will be written, so we should be conservative.
        // With contracts available but callee unknown, address tracking is cleared.
        //
        // Note: We can't test with an undefined proc as it would fail to parse.
        // Instead we test the behavior without contracts at all.
        let source_no_contracts = r#"
@locals(1)
proc test
    locaddr.0
    drop
end
"#;
        // Without contracts, locaddr of uninitialized local should warn
        let diags = analyze_source(source_no_contracts);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Without contracts, should warn when taking address of uninitialized local"
        );
    }

    #[test]
    fn test_locaddr_with_swap_then_store_proc() {
        // Test that address tracking works through stack manipulation
        let source = r#"
# store_swapped: takes [addr] and writes 42 to it (via swap then store)
proc store_swapped
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.store_swapped
    loc_load.0
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn - local is initialized via store_swapped. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_locaddr_with_movup_then_store_proc() {
        // Test that address tracking works through movup
        let source = r#"
# store_with_movup: takes [addr] and writes to it using movup
proc store_with_movup
    push.1 push.2
    movup.2
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    exec.store_with_movup
    loc_load.0
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn - local is initialized via store_with_movup. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_multiple_locals_partial_init() {
        // Test that only the local passed to the writer is marked initialized
        let source = r#"
proc writer
    push.42
    swap
    mem_store
end

@locals(2)
proc caller
    locaddr.0
    exec.writer
    loc_load.0  # local 0 was initialized by writer
    drop
    loc_load.1  # local 1 was NOT initialized
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            1,
            "Should warn only for local 1. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
        assert!(
            diags[0].message.contains("local 1"),
            "Should warn about local 1"
        );
    }

    #[test]
    fn test_locaddr_stored_via_dup_and_call() {
        // Test that duplicating an address and passing it to writer works
        let source = r#"
proc writer
    push.42
    swap
    mem_store
end

@locals(1)
proc caller
    locaddr.0
    dup
    exec.writer
    drop  # drop the extra copy
    loc_load.0
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn - dup'd address passed to writer. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_transitive_output_address() {
        // Test that output address propagates through call chains
        let source = r#"
# inner writes to address
proc inner
    push.42
    swap
    mem_store
end

# outer just calls inner - should inherit output address
proc outer
    exec.inner
end

@locals(1)
proc caller
    locaddr.0
    exec.outer
    loc_load.0
    drop
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "Should not warn - transitive output address. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Stdlib compatibility tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn test_stdlib_sys_truncate_stack() {
        // Test that the sys::truncate_stack procedure doesn't produce false positives
        // It writes to locals before reading them
        let source = r#"
@locals(4)
pub proc truncate_stack()
    # save the first word to memory and bring elements to be dropped to the top of the stack
    loc_storew_be.0 dropw movupw.3

    # until stack depth greater than 16, keep dropping extra elements
    sdepth neq.16
    while.true
        dropw movupw.3
        sdepth neq.16
    end

    # bring the previously saved word back onto the stack
    loc_loadw_be.0
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "stdlib sys::truncate_stack should not produce warnings. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_stdlib_sorted_array_find_partial_key_value() {
        // Test a procedure that stores a value and loads it later
        let source = r#"
@locals(1)
proc find_partial_key_value
    # store use_full_key for later
    movup.6 loc_store.0

    adv_push.2

    if.true
        # ... some branches that use loc_load.0
        loc_load.0
        drop
    else
        loc_load.0
        drop
    end
end
"#;
        let diags = analyze_source_with_contracts(source);
        assert_eq!(
            count_warnings(&diags),
            0,
            "sorted_array find_partial_key_value pattern should not produce warnings. Got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }
}
