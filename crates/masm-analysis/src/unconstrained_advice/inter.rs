//! Interprocedural driver for unconstrained-advice analysis.

use masm_decompiler::{
    callgraph::CallGraph, frontend::Workspace, signature::SignatureMap, types::TypeSummaryMap,
};

use super::{
    address::collect_address_diagnostics,
    merkle::collect_merkle_diagnostics,
    nonzero::infer_nonzero_summaries_and_diagnostics,
    provenance::analyze_proc_provenance,
    summary::{AdviceDiagnosticsMap, AdviceSummary, AdviceSummaryMap},
    u32::collect_u32_diagnostics,
};
use crate::{
    analysis_inputs,
    prepared::{prepare_procs, PreparedProcMap},
};

/// Infer unconstrained-advice summaries and diagnostics using precomputed analysis inputs.
pub fn infer_unconstrained_advice(
    workspace: &Workspace,
    callgraph: &CallGraph,
    signatures: &SignatureMap,
    type_summaries: &TypeSummaryMap,
) -> (AdviceSummaryMap, AdviceDiagnosticsMap) {
    let prepared = prepare_procs(workspace, callgraph, signatures, type_summaries);
    let provenance_summaries = infer_provenance_summaries(callgraph, &prepared);
    let mut diagnostics = collect_u32_diagnostics(&prepared, &provenance_summaries, type_summaries);
    let address_diagnostics = collect_address_diagnostics(&prepared, &provenance_summaries);
    merge_diagnostics(&mut diagnostics, address_diagnostics);
    let merkle_diagnostics = collect_merkle_diagnostics(&prepared, &provenance_summaries);
    merge_diagnostics(&mut diagnostics, merkle_diagnostics);
    let (_, nonzero_diagnostics) =
        infer_nonzero_summaries_and_diagnostics(callgraph, &prepared, &provenance_summaries);
    merge_diagnostics(&mut diagnostics, nonzero_diagnostics);

    (provenance_summaries, diagnostics)
}

/// Infer unconstrained-advice summaries and diagnostics by building analysis inputs locally.
pub fn infer_unconstrained_advice_in_workspace(
    workspace: &Workspace,
) -> (AdviceSummaryMap, AdviceDiagnosticsMap) {
    let (callgraph, signatures, type_summaries) = analysis_inputs(workspace);
    infer_unconstrained_advice(workspace, &callgraph, &signatures, &type_summaries)
}

/// Infer bottom-up provenance summaries for all procedures.
fn infer_provenance_summaries(
    callgraph: &CallGraph,
    prepared: &PreparedProcMap,
) -> AdviceSummaryMap {
    let mut summaries = AdviceSummaryMap::default();

    for node in callgraph.iter() {
        let Some(proc) = prepared.get(&node.name) else {
            summaries.insert(node.name.clone(), AdviceSummary::unknown());
            continue;
        };
        let summary = match proc.stmts.as_deref() {
            Some(stmts) => analyze_proc_provenance(proc.inputs, proc.outputs, stmts, &summaries),
            None => AdviceSummary::unknown_with_arity(proc.outputs),
        };
        summaries.insert(node.name.clone(), summary);
    }

    summaries
}

/// Merge diagnostics from one pass into the combined diagnostic map.
fn merge_diagnostics(combined: &mut AdviceDiagnosticsMap, next: AdviceDiagnosticsMap) {
    for (proc, mut proc_diags) in next {
        combined.entry(proc).or_default().append(&mut proc_diags);
    }
}
