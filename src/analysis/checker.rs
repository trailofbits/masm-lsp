//! Checker trait for pluggable security analyses.
//!
//! This module defines the `Checker` trait that allows implementing
//! modular security checks that can be composed into the analyzer.

use miden_assembly_syntax::ast::Instruction;
use miden_debug_types::SourceSpan;
use tower_lsp::lsp_types::{DiagnosticSeverity, Url};

use super::contracts::ContractStore;
use super::types::{AnalysisState, TrackedValue};

// ═══════════════════════════════════════════════════════════════════════════
// AnalysisFinding - result of a check
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
pub struct AnalysisFinding {
    /// Human-readable description of the issue
    pub message: String,
    /// Severity of the finding
    pub severity: Severity,
    /// The tracked value that caused this finding (for related information)
    pub related_value: Option<TrackedValue>,
}

impl AnalysisFinding {
    /// Create a new warning finding.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            severity: Severity::Warning,
            related_value: None,
        }
    }

    /// Create a new error finding.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            severity: Severity::Error,
            related_value: None,
        }
    }

    /// Attach a related tracked value to this finding.
    pub fn with_tracked_value(mut self, value: TrackedValue) -> Self {
        self.related_value = Some(value);
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
///     ) -> Vec<AnalysisFinding> {
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
    ) -> Vec<AnalysisFinding>;

    /// Human-readable name for this checker (used in diagnostics).
    fn name(&self) -> &'static str;
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper functions for checkers
// ═══════════════════════════════════════════════════════════════════════════

/// Check if an instruction is a u32 operation (excluding assertions/tests/casts).
pub fn is_u32_op(inst: &Instruction) -> bool {
    let inst_str = inst.to_string();
    inst_str.starts_with("u32")
        && !inst_str.starts_with("u32assert")
        && !inst_str.starts_with("u32test")
        && !inst_str.starts_with("u32split")
        && !inst_str.starts_with("u32cast")
}

/// Returns the number of stack inputs consumed by a u32 operation.
/// Returns 0 for non-u32 operations or those that don't consume inputs.
pub fn u32_op_input_count(inst: &Instruction) -> usize {
    use Instruction::*;
    match inst {
        // Ternary: pop 3
        U32OverflowingAdd3 | U32OverflowingMadd | U32WrappingAdd3 | U32WrappingMadd => 3,

        // Binary: pop 2
        U32WrappingAdd | U32WrappingSub | U32WrappingMul | U32OverflowingAdd
        | U32OverflowingSub | U32OverflowingMul | U32DivMod | U32Div | U32Mod | U32And
        | U32Or | U32Xor | U32Shl | U32Shr | U32Rotl | U32Rotr | U32Lt | U32Lte | U32Gt
        | U32Gte | U32Min | U32Max => 2,

        // Unary with immediate: pop 1
        U32WrappingAddImm(_)
        | U32WrappingSubImm(_)
        | U32WrappingMulImm(_)
        | U32OverflowingAddImm(_)
        | U32OverflowingSubImm(_)
        | U32OverflowingMulImm(_)
        | U32DivModImm(_)
        | U32DivImm(_)
        | U32ModImm(_)
        | U32ShlImm(_)
        | U32ShrImm(_)
        | U32RotlImm(_)
        | U32RotrImm(_) => 1,

        // Unary: pop 1
        U32Not | U32Popcnt | U32Clz | U32Ctz | U32Clo | U32Cto => 1,

        // Operations that don't consume inputs or are excluded
        _ => 0,
    }
}

/// Check if the top n stack values contain unvalidated untrusted input.
pub fn has_unvalidated_advice(state: &AnalysisState, n: usize) -> Option<TrackedValue> {
    for i in 0..n.min(state.stack.depth()) {
        if let Some(value) = state.stack.peek(i) {
            if value.origin.is_untrusted() && !value.is_validated() {
                return Some(value.clone());
            }
        }
    }
    None
}
