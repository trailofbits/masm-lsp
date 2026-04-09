//! Diagnostic and summary types for uninitialized-local-read analysis.

use std::collections::{BTreeMap, HashMap};

use miden_debug_types::SourceSpan;

use crate::SymbolPath;

use super::domain::CellSet;

/// Kind of local-init diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocalInitDiagnosticKind {
    /// Direct local read (`loc_load.i`).
    LocalRead,
    /// Direct local word read (`loc_loadw_*.i`).
    LocalWordRead,
    /// Memory read via `locaddr`-derived address.
    MemReadViaLocalAddr,
    /// Read-before-write detected in a callee via `exec`.
    ExecReadBeforeWrite,
}

/// Diagnostic for a possibly-uninitialized local read.
#[derive(Debug, Clone)]
pub struct LocalInitDiagnostic {
    /// Procedure that contains the read.
    pub procedure: SymbolPath,
    /// Source span of the read instruction.
    pub span: SourceSpan,
    /// Kind of access.
    pub kind: LocalInitDiagnosticKind,
    /// Human-readable message.
    pub message: String,
    /// Local cell indices that may be uninitialized.
    pub local_indices: Vec<u16>,
    /// Related spans (e.g., origin of `locaddr`).
    pub related: Vec<SourceSpan>,
}

/// Map of local-init diagnostics keyed by procedure path.
pub type LocalInitDiagnosticsMap = HashMap<SymbolPath, Vec<LocalInitDiagnostic>>;

/// Summary of a procedure's local-init effects on addresses passed as inputs.
///
/// During interprocedural analysis, each procedure that receives addresses via
/// its inputs is summarized. The summary describes:
/// - Which input-relative cells the procedure reads before writing
///   (`reads_before_write`)
/// - Which input-relative cells the procedure definitely writes
///   (`definitely_writes`)
///
/// When the summary is applied at an `exec` call site, these input-relative
/// effects are mapped to the caller's local frame using the actual addresses
/// passed as arguments.
#[derive(Debug, Clone)]
pub struct LocalInitSummary {
    /// Whether this summary is opaque (no precise information available).
    pub unknown: bool,
    /// For each input index, the set of cells the callee reads-before-write
    /// via that input's address. Empty means no reads-before-write.
    pub reads_before_write: BTreeMap<usize, CellSet>,
    /// For each input index, the set of cells the callee definitely writes
    /// via that input's address. Empty means no definite writes.
    pub definitely_writes: BTreeMap<usize, CellSet>,
}

impl LocalInitSummary {
    /// Create an opaque summary with no precise information.
    pub fn unknown() -> Self {
        Self {
            unknown: true,
            reads_before_write: BTreeMap::new(),
            definitely_writes: BTreeMap::new(),
        }
    }

    /// Create a known summary with the given read-before-write and definite-write
    /// effects.
    pub fn known(
        reads_before_write: BTreeMap<usize, CellSet>,
        definitely_writes: BTreeMap<usize, CellSet>,
    ) -> Self {
        Self {
            unknown: false,
            reads_before_write,
            definitely_writes,
        }
    }
}

/// Map of local-init summaries keyed by procedure path.
pub type LocalInitSummaryMap = HashMap<SymbolPath, LocalInitSummary>;
