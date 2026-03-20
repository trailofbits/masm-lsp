//! Summary and diagnostic types for unconstrained-advice analysis.

use std::collections::HashMap;

use miden_debug_types::SourceSpan;

use crate::SymbolPath;

use super::domain::AdviceFact;

/// Summary of unconstrained-advice flow for one procedure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdviceSummary {
    /// Per-output unconstrained-advice provenance.
    pub outputs: Vec<AdviceFact>,
    /// Whether this summary is opaque.
    pub unknown: bool,
}

impl AdviceSummary {
    /// Create a known summary.
    pub fn new(outputs: Vec<AdviceFact>) -> Self {
        Self {
            outputs,
            unknown: false,
        }
    }

    /// Create an opaque summary with explicit output arity.
    pub fn unknown_with_arity(outputs: usize) -> Self {
        Self {
            outputs: vec![AdviceFact::bottom(); outputs],
            unknown: true,
        }
    }

    /// Return an opaque summary without arity information.
    pub fn unknown() -> Self {
        Self::unknown_with_arity(0)
    }

    /// Return true if the summary is opaque.
    pub fn is_unknown(&self) -> bool {
        self.unknown
    }
}

impl Default for AdviceSummary {
    fn default() -> Self {
        Self::unknown()
    }
}

/// Kind of sink that triggered an advice warning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AdviceSinkKind {
    /// A U32-producing or U32-requiring expression.
    U32Expression,
    /// A U32 intrinsic statement.
    U32Intrinsic,
    /// An operand to `div`/`inv` that must be proven non-zero.
    NonZeroOperand,
    /// A call argument whose callee expects `U32`.
    CallArgument,
}

/// Diagnostic emitted by unconstrained-advice analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdviceDiagnostic {
    /// Procedure in which the diagnostic was emitted.
    pub procedure: SymbolPath,
    /// Source span associated with the sink.
    pub span: SourceSpan,
    /// Concrete advice source spans that may reach this sink.
    pub origins: Vec<SourceSpan>,
    /// Human-readable message.
    pub message: String,
    /// Optional callee for call-argument diagnostics.
    pub callee: Option<SymbolPath>,
    /// Optional argument index for call-argument diagnostics.
    pub arg_index: Option<usize>,
    /// Kind of sink that triggered the diagnostic.
    pub sink: AdviceSinkKind,
}

impl AdviceDiagnostic {
    /// Create a new diagnostic with the given sink kind and message.
    pub fn new(
        procedure: SymbolPath,
        span: SourceSpan,
        sink: AdviceSinkKind,
        message: impl Into<String>,
    ) -> Self {
        Self {
            procedure,
            span,
            origins: Vec::new(),
            message: message.into(),
            callee: None,
            arg_index: None,
            sink,
        }
    }
}

/// Map of advice summaries by procedure.
pub type AdviceSummaryMap = HashMap<SymbolPath, AdviceSummary>;

/// Map of advice diagnostics by procedure.
pub type AdviceDiagnosticsMap = HashMap<SymbolPath, Vec<AdviceDiagnostic>>;
