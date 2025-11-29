//! Analyzer for detecting uninitialized local variable access.
//!
//! This module provides control-flow aware analysis to detect reads from
//! local variables that may not have been initialized.

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Immediate, Instruction, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan, Span};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

use super::state::{LocalState, LocalsState};

// ═══════════════════════════════════════════════════════════════════════════
// Helper functions
// ═══════════════════════════════════════════════════════════════════════════

/// Extract u16 value from an immediate.
fn u16_imm_to_u16(imm: &Immediate<u16>) -> Option<u16> {
    match imm {
        Immediate::Value(span) => Some(*span.inner()),
        _ => None,
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
pub struct LocalsAnalyzer<'a> {
    source_manager: &'a DefaultSourceManager,
    #[allow(dead_code)] // May be used for related information in the future
    uri: Url,
    findings: Vec<LocalsFinding>,
}

impl<'a> LocalsAnalyzer<'a> {
    /// Create a new locals analyzer.
    pub fn new(source_manager: &'a DefaultSourceManager, uri: Url) -> Self {
        Self {
            source_manager,
            uri,
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
        self.analyze_block(proc.body(), &mut state);
    }

    /// Analyze a block of operations.
    fn analyze_block(&mut self, block: &Block, state: &mut LocalsState) {
        for op in block.iter() {
            self.analyze_op(op, state);
        }
    }

    /// Analyze a single operation.
    fn analyze_op(&mut self, op: &Op, state: &mut LocalsState) {
        match op {
            Op::Inst(inst) => self.analyze_instruction(inst, state),
            Op::If {
                then_blk,
                else_blk,
                ..
            } => self.analyze_if_else(then_blk, else_blk, state),
            Op::While { body, .. } => self.analyze_while(body, state),
            Op::Repeat { count, body, .. } => self.analyze_repeat(*count, body, state),
        }
    }

    /// Analyze an if/else construct.
    ///
    /// Both branches are analyzed independently, then states are merged
    /// at the join point using the meet operation.
    fn analyze_if_else(&mut self, then_blk: &Block, else_blk: &Block, state: &mut LocalsState) {
        // Clone state for each branch
        let mut then_state = state.clone();
        let mut else_state = state.clone();

        // Analyze both branches
        self.analyze_block(then_blk, &mut then_state);
        self.analyze_block(else_blk, &mut else_state);

        // Meet the results at the join point
        *state = then_state.meet(&else_state);
    }

    /// Analyze a while loop.
    ///
    /// Since while loops may execute zero times, any initialization inside
    /// the loop body cannot guarantee the local is initialized after the loop.
    fn analyze_while(&mut self, body: &Block, state: &mut LocalsState) {
        // Save entry state
        let entry_state = state.clone();

        // Analyze the body to find what gets initialized and check for reads
        self.analyze_block(body, state);

        // Since the loop may not execute, meet entry state with body result.
        // This makes anything initialized only in the body "MaybeInitialized".
        *state = entry_state.meet(state);
    }

    /// Analyze a repeat loop.
    ///
    /// Repeat loops with count > 0 always execute at least once, so
    /// initializations in the body are reliable.
    fn analyze_repeat(&mut self, count: u32, body: &Block, state: &mut LocalsState) {
        if count == 0 {
            return; // No execution, state unchanged
        }

        // Repeat always executes at least once, so body initializations are reliable
        self.analyze_block(body, state);
    }

    /// Analyze a single instruction.
    fn analyze_instruction(&mut self, inst: &Span<Instruction>, state: &mut LocalsState) {
        let span = inst.span();

        match inst.inner() {
            // ─────────────────────────────────────────────────────────────────
            // Write operations - mark locals as initialized
            // ─────────────────────────────────────────────────────────────────
            Instruction::LocStore(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_single(idx);
                }
            }
            Instruction::LocStoreWBe(idx) | Instruction::LocStoreWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    state.init_word(idx);
                }
            }

            // ─────────────────────────────────────────────────────────────────
            // Read operations - check for uninitialized access
            // ─────────────────────────────────────────────────────────────────
            Instruction::LocLoad(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_single_read(idx, span, state);
                }
            }
            Instruction::LocLoadWBe(idx) | Instruction::LocLoadWLe(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_word_read(idx, span, state);
                }
            }

            // ─────────────────────────────────────────────────────────────────
            // Address operations - warn if taking address of uninitialized local
            // ─────────────────────────────────────────────────────────────────
            Instruction::Locaddr(idx) => {
                if let Some(idx) = u16_imm_to_u16(idx) {
                    self.check_address_escape(idx, span, state);
                }
            }

            // All other instructions don't affect local initialization tracking
            _ => {}
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
                    message: format!(
                        "Local {} may not be initialized on all code paths.",
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
        let range = self.span_to_range(finding.span);

        Diagnostic {
            range,
            severity: Some(finding.severity),
            code: None,
            code_description: None,
            source: Some("masm-lsp/uninitialized-locals".to_string()),
            message: finding.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    /// Convert a source span to an LSP range.
    fn span_to_range(&self, span: SourceSpan) -> Range {
        let byte_range = span.into_range();
        let start = self
            .source_manager
            .file_line_col(SourceSpan::at(span.source_id(), byte_range.start))
            .ok();
        let end = self
            .source_manager
            .file_line_col(SourceSpan::at(span.source_id(), byte_range.end))
            .ok();

        match (start, end) {
            (Some(s), Some(e)) => Range::new(
                Position::new(
                    s.line.to_usize().saturating_sub(1) as u32,
                    s.column.to_usize().saturating_sub(1) as u32,
                ),
                Position::new(
                    e.line.to_usize().saturating_sub(1) as u32,
                    e.column.to_usize().saturating_sub(1) as u32,
                ),
            ),
            _ => Range::new(Position::new(0, 0), Position::new(0, 0)),
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
pub fn analyze_locals(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
) -> Vec<Diagnostic> {
    let analyzer = LocalsAnalyzer::new(source_manager, uri.clone());
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
    use miden_debug_types::{SourceLanguage, Uri};

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

        let source_file = source_manager.get_by_uri(&miden_uri).expect("Failed to load source");
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
        assert_eq!(count_warnings(&diags), 0, "Should not warn when local is initialized before read");
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
        assert_eq!(count_warnings(&diags), 1, "Should warn when reading uninitialized local");
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
        assert_eq!(count_warnings(&diags), 1, "Should warn when word contains uninitialized locals");
        assert!(diags[0].message.contains("local 2") || diags[0].message.contains("local 3"), "message: {}", diags[0].message);
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
        assert_eq!(count_warnings(&diags), 0, "Should not warn when full word is initialized");
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
        assert_eq!(count_warnings(&diags), 0, "Should not warn when both branches initialize");
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
        assert_eq!(count_warnings(&diags), 1, "Should warn when only one branch initializes");
        assert!(diags[0].message.contains("may not be initialized"), "message: {}", diags[0].message);
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
        assert_eq!(count_warnings(&diags), 1, "Should warn when init is only in while loop body");
        assert!(diags[0].message.contains("may not be initialized"), "message: {}", diags[0].message);
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
        assert_eq!(count_warnings(&diags), 0, "Should not warn when init is in repeat.n (n>0) body");
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
        assert_eq!(count_warnings(&diags), 1, "Should warn when taking address of uninitialized local");
        assert!(diags[0].message.contains("Taking address"), "message: {}", diags[0].message);
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
        assert_eq!(count_warnings(&diags), 0, "Should not warn when taking address of initialized local");
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
        assert_eq!(count_warnings(&diags), 1, "Should only warn for the bad procedure");
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
        assert_eq!(count_warnings(&diags), 1, "Should warn for uninitialized local 1");
        assert!(diags[0].message.contains("local 1"), "message: {}", diags[0].message);
    }
}
