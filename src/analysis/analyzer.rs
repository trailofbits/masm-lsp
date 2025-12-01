//! Main analyzer that composes stack simulation with security checkers.
//!
//! This module provides the `Analyzer` struct that:
//! 1. Walks the AST using the Visit trait
//! 2. Runs all configured checkers on each instruction
//! 3. Applies stack effects to track value provenance
//! 4. Collects findings and converts them to LSP diagnostics

use std::ops::ControlFlow;

use miden_assembly_syntax::ast::{
    visit::{self, Visit},
    Block, Instruction, InvocationTarget, Module, Op, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan, Span, Spanned};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, Location, Position, Range, Url,
};

use crate::diagnostics::SOURCE_ANALYSIS;
use crate::symbol_resolution::SymbolResolver;

use super::checker::{AnalysisFinding, CheckContext, Checker, Severity};
use super::checkers::default_checkers;
use super::contracts::{ContractStore, StackEffect};
use super::stack_effects::apply_effect;
use super::types::{AnalysisState, ValueOrigin};

// ═══════════════════════════════════════════════════════════════════════════
// Analyzer
// ═══════════════════════════════════════════════════════════════════════════

/// Main analyzer that composes stack simulation with security checkers.
pub struct Analyzer<'a> {
    source_manager: &'a DefaultSourceManager,
    uri: Url,
    resolver: SymbolResolver<'a>,
    contracts: Option<&'a ContractStore>,
    checkers: Vec<Box<dyn Checker>>,

    // Analysis state
    current_state: Option<AnalysisState>,
    findings: Vec<(SourceSpan, AnalysisFinding)>,
}

impl<'a> Analyzer<'a> {
    /// Create a new analyzer with default checkers.
    pub fn new(
        module: &'a Module,
        source_manager: &'a DefaultSourceManager,
        uri: Url,
        contracts: Option<&'a ContractStore>,
    ) -> Self {
        Self {
            source_manager,
            uri,
            resolver: crate::symbol_resolution::create_resolver(module),
            contracts,
            checkers: default_checkers(),
            current_state: None,
            findings: Vec::new(),
        }
    }

    /// Add a custom checker to the analyzer.
    pub fn with_checker(mut self, checker: Box<dyn Checker>) -> Self {
        self.checkers.push(checker);
        self
    }

    /// Run analysis on a module and return LSP diagnostics.
    pub fn analyze(mut self, module: &Module) -> Vec<Diagnostic> {
        let _ = visit::visit_module(&mut self, module);
        self.build_diagnostics()
    }

    /// Handle a single instruction: run checkers, then apply stack effects.
    fn handle_instruction(&mut self, inst: &Instruction, span: SourceSpan) {
        // 1. Run all checkers BEFORE modifying state
        if let Some(state) = &self.current_state {
            let ctx = CheckContext {
                contracts: self.contracts,
                uri: &self.uri,
                span,
            };

            for checker in &self.checkers {
                let checker_findings = checker.check(inst, state, &ctx);
                for finding in checker_findings {
                    self.findings.push((span, finding));
                }
            }
        }

        // 2. Apply stack effects
        if let Some(state) = &mut self.current_state {
            apply_effect(inst, state, span);
        }

        // 3. Handle procedure calls - apply stack effects or clear tracking
        self.handle_procedure_call_effects(inst);
    }

    /// Apply stack effects for procedure calls.
    ///
    /// For procedures with known stack effects, we pop/push accordingly.
    /// For procedures with unknown stack effects, we clear the taint tracking
    /// to avoid false positives (we can't know if tainted values are still on stack).
    fn handle_procedure_call_effects(&mut self, inst: &Instruction) {
        let target = match inst {
            Instruction::Exec(t) | Instruction::Call(t) | Instruction::SysCall(t) => t,
            _ => return,
        };

        // Get target name for error messages and taint tracking
        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str().to_string(),
            InvocationTarget::Path(path) => path.inner().as_str().to_string(),
            InvocationTarget::MastRoot(_) => {
                // MAST root calls - unknown effect, clear tracking
                if let Some(state) = &mut self.current_state {
                    state.stack.clear();
                }
                return;
            }
        };

        // Use the unified symbol resolution to look up the contract
        let stack_effect = self.contracts.and_then(|store| {
            let resolved_path = self.resolver.resolve_target(target)?;
            store.get(&resolved_path).map(|c| &c.stack_effect)
        });

        let state = match &mut self.current_state {
            Some(s) => s,
            None => return,
        };

        match stack_effect {
            Some(StackEffect::Known { inputs, outputs }) => {
                // Pop inputs
                for _ in 0..*inputs {
                    state.stack.pop();
                }
                // Push outputs as derived values (procedure return values)
                for _ in 0..*outputs {
                    let taint = state.make_proc_return(target_name.clone());
                    state.stack.push(taint);
                }
            }
            Some(StackEffect::KnownInputs { inputs }) => {
                // We know inputs but not outputs - pop inputs, then clear tracking
                for _ in 0..*inputs {
                    state.stack.pop();
                }
                // Unknown outputs - clear tracking to avoid false positives
                state.stack.clear();
            }
            Some(StackEffect::Unknown) | None => {
                // Unknown stack effect - clear tracking to avoid false positives
                state.stack.clear();
            }
        }
    }

    /// Analyze a block and return the net stack effect (change in depth).
    ///
    /// Returns `None` if analysis fails (e.g., unknown procedure call).
    fn analyze_block_effect(&mut self, block: &Block) -> Option<isize> {
        let entry_depth = self.current_state.as_ref()?.stack.depth() as isize;

        for op in block.iter() {
            self.analyze_op_for_effect(op);
        }

        let exit_depth = self.current_state.as_ref()?.stack.depth() as isize;
        Some(exit_depth - entry_depth)
    }

    /// Analyze a single op for stack effects (used by analyze_block_effect).
    /// Also checks nested if/else blocks for branch balance.
    fn analyze_op_for_effect(&mut self, op: &Op) {
        match op {
            Op::Inst(inst) => {
                self.handle_instruction(inst.inner(), inst.span());
            }
            Op::If {
                then_blk,
                else_blk,
                ..
            } => {
                // Check nested if/else for branch balance
                self.check_branch_balance(then_blk, else_blk, op.span());
            }
            Op::While { body, .. } => {
                // Condition is consumed at start of each iteration
                if let Some(state) = &mut self.current_state {
                    state.stack.pop();
                }
                // Analyze body once for effect
                for op in body.iter() {
                    self.analyze_op_for_effect(op);
                }
            }
            Op::Repeat { body, .. } => {
                // Analyze body once (repeat has known iteration count)
                for op in body.iter() {
                    self.analyze_op_for_effect(op);
                }
            }
        }
    }

    /// Check if/else branches for stack balance and emit diagnostic if mismatched.
    fn check_branch_balance(
        &mut self,
        then_blk: &Block,
        else_blk: &Block,
        span: SourceSpan,
    ) {
        let Some(entry_state) = self.current_state.clone() else {
            return;
        };

        // Condition is consumed by if.true
        let _entry_depth = entry_state.stack.depth().saturating_sub(1) as isize;

        // Analyze then branch
        self.current_state = Some(entry_state.clone());
        if let Some(state) = &mut self.current_state {
            state.stack.pop(); // consume condition
        }
        let then_effect = self.analyze_block_effect(then_blk);
        let then_depth = self.current_state.as_ref().map(|s| s.stack.depth() as isize);

        // Analyze else branch
        self.current_state = Some(entry_state);
        if let Some(state) = &mut self.current_state {
            state.stack.pop(); // consume condition
        }
        let else_effect = self.analyze_block_effect(else_blk);
        let else_depth = self.current_state.as_ref().map(|s| s.stack.depth() as isize);

        // Compare effects
        match (then_effect, else_effect, then_depth, else_depth) {
            (Some(then_eff), Some(else_eff), Some(then_d), Some(_else_d)) => {
                if then_eff != else_eff {
                    let finding = AnalysisFinding {
                        severity: Severity::Warning,
                        message: format!(
                            "Branch stack mismatch: 'then' branch has net effect {:+}, 'else' branch has net effect {:+}.",
                            then_eff, else_eff
                        ),
                        related_value: None,
                    };
                    self.findings.push((span, finding));
                }
                // Use the then branch state going forward (arbitrary choice, both have same effect)
                // Restore to then branch final depth
                if let Some(state) = &mut self.current_state {
                    // Adjust stack to then_depth (we're currently at else_depth)
                    let current = state.stack.depth() as isize;
                    let target = then_d;
                    if current > target {
                        for _ in 0..(current - target) {
                            state.stack.pop();
                        }
                    }
                }
            }
            _ => {
                // Analysis failed for one branch, can't compare
            }
        }
    }

    /// Convert collected findings to LSP diagnostics.
    fn build_diagnostics(&self) -> Vec<Diagnostic> {
        self.findings
            .iter()
            .map(|(span, finding)| self.finding_to_diagnostic(*span, finding))
            .collect()
    }

    /// Convert a finding to an LSP diagnostic.
    fn finding_to_diagnostic(&self, span: SourceSpan, finding: &AnalysisFinding) -> Diagnostic {
        let range = self.span_to_range(span);

        // Build related information if we have a source location
        let related_information = finding.related_value.as_ref().and_then(|value| {
            value.source_span.map(|source_span| {
                let source_range = self.span_to_range(source_span);
                let source_desc = match &value.origin {
                    ValueOrigin::AdviceStack => "Untrusted value loaded from advice stack here",
                    ValueOrigin::AdviceMap => "Untrusted value loaded from advice map here",
                    ValueOrigin::MerkleStore => "Untrusted value loaded from Merkle store here",
                    _ => "Value originated here",
                };
                vec![DiagnosticRelatedInformation {
                    location: Location {
                        uri: self.uri.clone(),
                        range: source_range,
                    },
                    message: source_desc.to_string(),
                }]
            })
        });

        Diagnostic {
            range,
            severity: Some(finding.severity.into()),
            code: None,
            code_description: None,
            source: Some(SOURCE_ANALYSIS.to_string()),
            message: finding.message.clone(),
            related_information,
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

impl<'a> Visit for Analyzer<'a> {
    fn visit_procedure(&mut self, proc: &Procedure) -> ControlFlow<()> {
        let proc_name = proc.name().as_str().to_string();

        // Initialize state for this procedure
        self.current_state = Some(AnalysisState::new(proc_name));

        // Continue traversal into procedure body
        let result = visit::visit_procedure(self, proc);

        // Clear state after procedure
        self.current_state = None;

        result
    }

    fn visit_op(&mut self, op: &Op) -> ControlFlow<()> {
        match op {
            Op::If {
                then_blk,
                else_blk,
                ..
            } => {
                // Check for branch stack balance mismatch
                self.check_branch_balance(then_blk, else_blk, op.span());

                // Don't recurse - check_branch_balance already analyzed both branches
                ControlFlow::Continue(())
            }
            _ => {
                // Default traversal for other ops
                visit::visit_op(self, op)
            }
        }
    }

    fn visit_inst(&mut self, inst: &Span<Instruction>) -> ControlFlow<()> {
        self.handle_instruction(inst.inner(), inst.span());

        // Continue default traversal for nested structures (if, while, etc.)
        visit::visit_inst(self, inst)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

/// Analyze a module for security issues and return LSP diagnostics.
///
/// This is the main entry point for the analysis module.
///
/// # Arguments
///
/// * `module` - The parsed Miden assembly module to analyze
/// * `source_manager` - Source manager for span-to-position conversion
/// * `uri` - URI of the document being analyzed
/// * `contracts` - Optional workspace contracts for inter-procedural analysis
///
/// # Returns
///
/// A vector of LSP diagnostics for any issues found.
pub fn analyze_module(
    module: &Module,
    source_manager: &DefaultSourceManager,
    uri: &Url,
    contracts: Option<&ContractStore>,
) -> Vec<Diagnostic> {
    let analyzer = Analyzer::new(module, source_manager, uri.clone(), contracts);
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
    use tower_lsp::lsp_types::DiagnosticSeverity;

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
                analyze_module(&module, &source_manager, &uri, None)
            }
            Err(_) => vec![], // Parse error, no analysis
        }
    }

    #[test]
    fn test_branch_mismatch_detected() {
        let source = r#"
proc test
    push.1
    if.true
        push.1
    else
        nop
    end
end
"#;
        let diags = analyze_source(source);
        assert!(
            diags.iter().any(|d| d.message.contains("Branch stack mismatch")),
            "Expected branch mismatch diagnostic, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_branch_mismatch_severity_is_warning() {
        let source = r#"
proc test
    push.1
    if.true
        push.1 push.2
    else
        push.1
    end
end
"#;
        let diags = analyze_source(source);
        let mismatch = diags
            .iter()
            .find(|d| d.message.contains("Branch stack mismatch"));
        assert!(mismatch.is_some(), "Expected branch mismatch diagnostic");
        assert_eq!(mismatch.unwrap().severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn test_balanced_branches_no_diagnostic() {
        let source = r#"
proc test
    push.1
    if.true
        push.1
    else
        push.2
    end
end
"#;
        let diags = analyze_source(source);
        assert!(
            !diags.iter().any(|d| d.message.contains("Branch stack mismatch")),
            "Should not report mismatch for balanced branches, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_empty_else_matches_empty_then() {
        let source = r#"
proc test
    push.1
    if.true
        nop
    else
        nop
    end
end
"#;
        let diags = analyze_source(source);
        assert!(
            !diags.iter().any(|d| d.message.contains("Branch stack mismatch")),
            "Empty branches should be balanced, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_nested_if_mismatch() {
        let source = r#"
proc test
    push.1
    if.true
        push.1
        if.true
            push.1
        else
            nop
        end
    else
        push.1
    end
end
"#;
        let diags = analyze_source(source);
        assert!(
            diags.iter().any(|d| d.message.contains("Branch stack mismatch")),
            "Expected branch mismatch for nested if, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_mismatch_message_shows_effects() {
        let source = r#"
proc test
    push.1 push.2
    if.true
        push.1 push.2
    else
        drop
    end
end
"#;
        let diags = analyze_source(source);
        let mismatch = diags
            .iter()
            .find(|d| d.message.contains("Branch stack mismatch"))
            .expect("Expected branch mismatch diagnostic");
        // then: +2, else: -1 (drop from non-empty stack)
        assert!(
            mismatch.message.contains("+2") && mismatch.message.contains("-1"),
            "Message should show net effects, got: {}",
            mismatch.message
        );
    }
}
