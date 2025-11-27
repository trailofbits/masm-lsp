//! Checker trait for pluggable security analyses.
//!
//! This module defines the `Checker` trait that allows implementing
//! modular security checks that can be composed into the analyzer.

use miden_assembly_syntax::ast::Instruction;
use miden_debug_types::SourceSpan;
use tower_lsp::lsp_types::{DiagnosticSeverity, Url};

use super::contracts::ContractStore;
use super::types::{AnalysisState, Taint};

// ═══════════════════════════════════════════════════════════════════════════
// Finding - result of a check
// ═══════════════════════════════════════════════════════════════════════════

/// Severity level for findings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    /// Potential security issue
    Warning,
    /// Definite problem
    Error,
    /// Informational note
    Hint,
}

impl From<Severity> for DiagnosticSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Hint => DiagnosticSeverity::HINT,
        }
    }
}

/// A finding from a security check.
#[derive(Clone, Debug)]
pub struct Finding {
    /// Human-readable description of the issue
    pub message: String,
    /// Severity of the finding
    pub severity: Severity,
    /// The taint that caused this finding (for related information)
    pub related_taint: Option<Taint>,
}

impl Finding {
    /// Create a new warning finding.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            severity: Severity::Warning,
            related_taint: None,
        }
    }

    /// Create a new error finding.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            severity: Severity::Error,
            related_taint: None,
        }
    }

    /// Attach a related taint to this finding.
    pub fn with_taint(mut self, taint: Taint) -> Self {
        self.related_taint = Some(taint);
        self
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Check context
// ═══════════════════════════════════════════════════════════════════════════

/// Context provided to checkers during analysis.
pub struct CheckContext<'a> {
    /// Workspace-wide procedure contracts
    pub contracts: Option<&'a ContractStore>,
    /// URI of the current document
    pub uri: &'a Url,
    /// Source span of the current instruction
    pub span: SourceSpan,
}

// ═══════════════════════════════════════════════════════════════════════════
// Checker trait
// ═══════════════════════════════════════════════════════════════════════════

/// Trait for security checkers that examine instructions.
///
/// Checkers are called for each instruction during analysis, receiving
/// the current analysis state (before the instruction executes).
/// They return any findings (warnings, errors) detected.
///
/// # Example
///
/// ```ignore
/// struct MyChecker;
///
/// impl Checker for MyChecker {
///     fn check(
///         &self,
///         inst: &Instruction,
///         state: &AnalysisState,
///         ctx: &CheckContext,
///     ) -> Vec<Finding> {
///         // Check something about the instruction and state
///         vec![]
///     }
///
///     fn name(&self) -> &'static str {
///         "my-checker"
///     }
/// }
/// ```
pub trait Checker: Send + Sync {
    /// Check an instruction and return any findings.
    ///
    /// This is called BEFORE the instruction's stack effects are applied,
    /// so the state reflects the stack as it was before the instruction.
    fn check(
        &self,
        inst: &Instruction,
        state: &AnalysisState,
        ctx: &CheckContext,
    ) -> Vec<Finding>;

    /// Human-readable name for this checker (used in diagnostics).
    fn name(&self) -> &'static str;
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper functions for checkers
// ═══════════════════════════════════════════════════════════════════════════

/// Check if an instruction is a u32 operation (excluding assertions/tests).
pub fn is_u32_op(inst: &Instruction) -> bool {
    let inst_str = inst.to_string();
    inst_str.starts_with("u32")
        && !inst_str.starts_with("u32assert")
        && !inst_str.starts_with("u32test")
        && !inst_str.starts_with("u32split")
        && !inst_str.starts_with("u32cast")
}

/// Check if an instruction is a Merkle tree operation.
pub fn is_merkle_op(inst: &Instruction) -> bool {
    matches!(
        inst,
        Instruction::MTreeGet
            | Instruction::MTreeSet
            | Instruction::MTreeMerge
            | Instruction::MTreeVerify
            | Instruction::MTreeVerifyWithError(_)
    )
}

/// Check if the top n stack values contain unvalidated untrusted input.
pub fn has_unvalidated_advice(state: &AnalysisState, n: usize) -> Option<Taint> {
    for i in 0..n.min(state.stack.depth()) {
        if let Some(t) = state.stack.peek(i) {
            if t.source.is_untrusted() && !t.is_validated() {
                return Some(t.clone());
            }
        }
    }
    None
}
