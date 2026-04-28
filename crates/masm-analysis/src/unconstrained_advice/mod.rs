//! Interprocedural analysis for unconstrained advice reaching U32 and non-zero sinks.

mod address;
mod domain;
mod grouping;
mod inter;
mod merkle;
mod nonzero;
mod provenance;
mod shared;
mod summary;
mod u32;
mod u32_domain;
mod walker;

#[cfg(test)]
mod tests;

pub use grouping::{group_advice_diagnostics_by_origin, AdviceRootCauseGroup};
pub use inter::{infer_unconstrained_advice, infer_unconstrained_advice_in_workspace};
pub use summary::{
    AdviceDiagnostic, AdviceDiagnosticsMap, AdviceSinkKind, AdviceSummary, AdviceSummaryMap,
    CallArgumentRequirement,
};
