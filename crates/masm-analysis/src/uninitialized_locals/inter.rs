//! Interprocedural driver for uninitialized-local-read analysis.
//!
//! Phase 1 analyzed each procedure independently. Phase 2 adds interprocedural
//! propagation of local-init summaries through `exec` calls.
//!
//! The driver first builds exec summaries bottom-up over the call graph, then
//! runs the diagnostic pass using those summaries. For `exec` calls, the
//! callee's summary is applied at the call site to propagate reads-before-write
//! and definite writes. For `call` and `syscall`, no frame effects are
//! propagated because those invocations create new memory contexts.

use masm_decompiler::{
    callgraph::CallGraph, frontend::Workspace, signature::SignatureMap, types::TypeSummaryMap,
};

use crate::{
    analysis_inputs,
    prepared::{prepare_procs, PreparedProc, PreparedProcMap},
    SymbolPath,
};

use super::domain::FrameLayout;
use super::summary::{
    LocalInitDiagnostic, LocalInitDiagnosticsMap, LocalInitSummary, LocalInitSummaryMap,
};
use super::transfer::{eval_block_with_summaries, extract_summary, seed_input_env};

/// Infer uninitialized-local diagnostics using precomputed analysis inputs.
///
/// This is the main entry point for the analysis. It:
/// 1. Prepares (lifts) all procedures
/// 2. Builds exec summaries bottom-up over the call graph
/// 3. Runs the diagnostic pass using those summaries
pub(crate) fn infer_uninitialized_locals_with_inputs(
    workspace: &Workspace,
    callgraph: &CallGraph,
    signatures: &SignatureMap,
    type_summaries: &TypeSummaryMap,
) -> (LocalInitSummaryMap, LocalInitDiagnosticsMap) {
    let prepared = prepare_procs(workspace, callgraph, signatures, type_summaries);

    // Phase 1: Build exec summaries bottom-up.
    let summaries = infer_exec_summaries(callgraph, &prepared);

    // Phase 2: Run diagnostic pass with summaries.
    let mut diagnostics = LocalInitDiagnosticsMap::default();

    for node in callgraph.iter() {
        let proc_path = &node.name;
        let Some(proc) = prepared.get(proc_path) else {
            continue;
        };

        let proc_diags = analyze_proc_with_summaries(proc_path, proc, &summaries, &prepared);
        if !proc_diags.is_empty() {
            diagnostics.insert(proc_path.clone(), proc_diags);
        }
    }

    (summaries, diagnostics)
}

/// Convenience wrapper that builds analysis inputs and then infers diagnostics.
pub fn infer_uninitialized_locals_in_workspace(
    workspace: &Workspace,
) -> (LocalInitSummaryMap, LocalInitDiagnosticsMap) {
    let (callgraph, signatures, type_summaries) = analysis_inputs(workspace);
    infer_uninitialized_locals_with_inputs(workspace, &callgraph, &signatures, &type_summaries)
}

/// Build exec summaries for all procedures in bottom-up order.
///
/// Callees are processed before callers (reverse post-order from the call
/// graph). Each procedure's summary describes what it reads and writes through
/// addresses passed as inputs. Unknown or un-liftable procedures get opaque
/// summaries. Recursive procedures (SCCs) also get opaque summaries.
fn infer_exec_summaries(callgraph: &CallGraph, prepared: &PreparedProcMap) -> LocalInitSummaryMap {
    let mut summaries = LocalInitSummaryMap::default();

    for node in callgraph.iter() {
        let proc_path = &node.name;
        let summary = analyze_proc_for_summary(proc_path, prepared);
        summaries.insert(proc_path.clone(), summary);
    }

    summaries
}

/// Analyze a single procedure to produce its exec summary.
///
/// If the procedure has no stmts (un-liftable) or no inputs, returns
/// an opaque summary. Otherwise, runs the summary-inference variant
/// of the transfer functions which tracks reads and writes through
/// input-derived addresses.
fn analyze_proc_for_summary(
    proc_path: &SymbolPath,
    prepared: &PreparedProcMap,
) -> LocalInitSummary {
    let Some(proc) = prepared.get(proc_path) else {
        return LocalInitSummary::unknown();
    };

    let Some(stmts) = proc.stmts.as_deref() else {
        return LocalInitSummary::unknown();
    };

    if proc.inputs == 0 {
        // No inputs means no input-derived addresses to track.
        return LocalInitSummary::known(Default::default(), Default::default());
    }

    extract_summary(stmts, proc.inputs, prepared)
}

/// Analyze a single procedure for uninitialized local reads using exec summaries.
///
/// Returns an empty vector if the procedure has no locals or cannot be analyzed.
fn analyze_proc_with_summaries(
    proc_path: &SymbolPath,
    proc: &PreparedProc,
    summaries: &LocalInitSummaryMap,
    prepared: &PreparedProcMap,
) -> Vec<LocalInitDiagnostic> {
    if proc.num_locals == 0 {
        return Vec::new();
    }

    let Some(stmts) = proc.stmts.as_deref() else {
        return Vec::new();
    };

    let frame = FrameLayout::new(proc.num_locals);
    let env = seed_input_env(proc.inputs, frame);
    let result = eval_block_with_summaries(proc_path, stmts, env, summaries, prepared);
    result.diagnostics
}
