//! Analysis pass for detecting reads of uninitialized local memory cells.

pub mod domain;
pub(crate) mod inter;
pub(crate) mod state;
pub mod summary;
pub(crate) mod transfer;

#[cfg(test)]
mod tests;

pub use inter::infer_uninitialized_locals_in_workspace;
pub use summary::{
    LocalInitDiagnostic, LocalInitDiagnosticKind, LocalInitDiagnosticsMap, LocalInitSummary,
    LocalInitSummaryMap,
};
