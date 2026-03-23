//! Interprocedural analysis for unconstrained advice reaching U32 and non-zero sinks.

mod address;
mod domain;
mod inter;
mod merkle;
mod nonzero;
mod provenance;
mod shared;
mod summary;
mod u32;
mod walker;

#[cfg(test)]
mod tests;

pub use inter::{infer_unconstrained_advice, infer_unconstrained_advice_in_workspace};
pub use summary::{
    AdviceDiagnostic, AdviceDiagnosticsMap, AdviceSinkKind, AdviceSummary, AdviceSummaryMap,
};
