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
    Instruction, InvocationTarget, Module, Procedure,
};
use miden_debug_types::{DefaultSourceManager, SourceManager, SourceSpan, Span};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, Location, Position, Range, Url,
};

use crate::diagnostics::SOURCE_ANALYSIS;

use super::checker::{AnalysisFinding, CheckContext, Checker};
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
    contracts: Option<&'a ContractStore>,
    checkers: Vec<Box<dyn Checker>>,

    // Analysis state
    current_state: Option<AnalysisState>,
    findings: Vec<(SourceSpan, AnalysisFinding)>,
}

impl<'a> Analyzer<'a> {
    /// Create a new analyzer with default checkers.
    pub fn new(
        source_manager: &'a DefaultSourceManager,
        uri: Url,
        contracts: Option<&'a ContractStore>,
    ) -> Self {
        Self {
            source_manager,
            uri,
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

        let target_name = match target {
            InvocationTarget::Symbol(ident) => ident.as_str(),
            InvocationTarget::Path(path) => path.inner().as_str(),
            InvocationTarget::MastRoot(_) => {
                // MAST root calls - unknown effect, clear tracking
                if let Some(state) = &mut self.current_state {
                    state.stack.clear();
                }
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
                    let taint = state.make_proc_return(target_name.to_string());
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
    let analyzer = Analyzer::new(source_manager, uri.clone(), contracts);
    analyzer.analyze(module)
}
