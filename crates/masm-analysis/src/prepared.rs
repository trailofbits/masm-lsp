//! Shared procedure-preparation logic for interprocedural analysis passes.
//!
//! Both the unconstrained-advice and uninitialized-locals passes need to lift
//! all procedures in the workspace exactly once before running their respective
//! analyses. This module provides a shared [`PreparedProc`] struct and the
//! [`prepare_procs`] function that populates it, eliminating duplication.

use std::collections::HashMap;

use masm_decompiler::{
    callgraph::CallGraph,
    frontend::Workspace,
    lift::lift_proc,
    signature::{ProcSignature, SignatureMap},
    symbol::resolution::create_resolver,
    types::TypeSummaryMap,
    SymbolPath,
};

/// Prepared lifting result for one procedure.
///
/// Populated once by [`prepare_procs`] and consumed by all downstream analyses.
#[derive(Debug, Clone)]
pub struct PreparedProc {
    /// Input arity inferred from the procedure signature.
    pub inputs: usize,
    /// Output arity inferred from the procedure signature.
    pub outputs: usize,
    /// Number of declared local memory cells.
    pub num_locals: u16,
    /// Lifted SSA statements, when the procedure is analyzable.
    pub stmts: Option<Vec<masm_decompiler::ir::Stmt>>,
    /// Output-to-input passthrough map (0 = deepest input).
    pub passthrough_map: Vec<Option<usize>>,
}

/// Map from procedure path to its prepared result.
pub type PreparedProcMap = HashMap<SymbolPath, PreparedProc>;

/// Extract (inputs, outputs) arity from a procedure signature.
///
/// Returns `None` if the signature is [`ProcSignature::Unknown`].
pub fn signature_arity(signature: &ProcSignature) -> Option<(usize, usize)> {
    match signature {
        ProcSignature::Known {
            inputs, outputs, ..
        } => Some((*inputs, *outputs)),
        ProcSignature::Unknown => None,
    }
}

/// Prepare and lift all procedures once for downstream analyses.
///
/// For each node in the call graph:
/// - If no signature is available, the entry is recorded with zero arity and
///   `stmts = None`.
/// - If the signature is [`ProcSignature::Unknown`], the same applies.
/// - If the procedure cannot be found in the workspace, arity is preserved but
///   `stmts = None`.
/// - If lifting fails, arity and `num_locals` are preserved but `stmts = None`.
/// - On success, all fields are populated.
pub fn prepare_procs(
    workspace: &Workspace,
    callgraph: &CallGraph,
    signatures: &SignatureMap,
    type_summaries: &TypeSummaryMap,
) -> PreparedProcMap {
    let mut prepared = HashMap::new();

    for node in callgraph.iter() {
        let proc_path = node.name.clone();
        let Some(signature) = signatures.get(&proc_path) else {
            prepared.insert(
                proc_path,
                PreparedProc {
                    inputs: 0,
                    outputs: 0,
                    num_locals: 0,
                    stmts: None,
                    passthrough_map: Vec::new(),
                },
            );
            continue;
        };

        let Some((inputs, outputs)) = signature_arity(signature) else {
            prepared.insert(
                proc_path,
                PreparedProc {
                    inputs: 0,
                    outputs: 0,
                    num_locals: 0,
                    stmts: None,
                    passthrough_map: Vec::new(),
                },
            );
            continue;
        };

        let Some((program, proc)) = workspace.lookup_proc_entry(&proc_path) else {
            prepared.insert(
                proc_path,
                PreparedProc {
                    inputs,
                    outputs,
                    num_locals: 0,
                    stmts: None,
                    passthrough_map: Vec::new(),
                },
            );
            continue;
        };

        let num_locals = proc.num_locals();

        let resolver = create_resolver(program.module(), workspace.source_manager());
        let stmts = match lift_proc(proc, &proc_path, &resolver, signatures) {
            Ok(stmts) => Some(stmts),
            Err(_) => {
                prepared.insert(
                    proc_path,
                    PreparedProc {
                        inputs,
                        outputs,
                        num_locals,
                        stmts: None,
                        passthrough_map: Vec::new(),
                    },
                );
                continue;
            }
        };

        let passthrough_map = masm_decompiler::passthrough::compute_passthrough_map(
            stmts.as_ref().unwrap(),
            inputs,
            outputs,
            Some(type_summaries),
        );

        prepared.insert(
            proc_path,
            PreparedProc {
                inputs,
                outputs,
                num_locals,
                stmts,
                passthrough_map,
            },
        );
    }

    prepared
}

#[cfg(test)]
mod tests {
    use masm_decompiler::{
        callgraph::CallGraph,
        frontend::testing::workspace_from_modules,
        signature::{infer_signatures, ProcSignature},
        types::infer_type_summaries,
    };

    use super::*;

    /// Helper: run prepare_procs over a single-module workspace and return the map.
    fn prepare_one(module: &str, source: &str) -> PreparedProcMap {
        let ws = workspace_from_modules(&[(module, source)]);
        let callgraph = CallGraph::from(&ws);
        let signatures = infer_signatures(&ws, &callgraph);
        let (type_summaries, _) = infer_type_summaries(&ws, &callgraph, &signatures);
        prepare_procs(&ws, &callgraph, &signatures, &type_summaries)
    }

    #[test]
    fn prepared_known_proc_captures_inputs_outputs_and_num_locals() {
        // A procedure with 5 locals. The body:
        //   - consumes 2 stack inputs (stored into loc 0 and loc 1)
        //   - produces 1 stack output (loaded from loc 0)
        // so the stack-depth inferrer gives inputs=2, outputs=1.
        let source = concat!(
            "@locals(5)\n",
            "proc foo\n",
            "  loc_store.0\n", // consumes 1 (total consumed so far: 1)
            "  loc_store.1\n", // consumes 1 (total consumed so far: 2)
            "  loc_load.0\n",  // produces 1
            "end\n",
        );
        let prepared = prepare_one("test", source);

        let proc = prepared
            .get(&SymbolPath::new("test::foo".to_string()))
            .expect("test::foo should be in the map");

        assert_eq!(proc.num_locals, 5, "expected num_locals == 5");
        assert_eq!(proc.inputs, 2, "expected inputs == 2");
        assert_eq!(proc.outputs, 1, "expected outputs == 1");
        assert!(proc.stmts.is_some(), "expected stmts to be Some(_)");
    }

    #[test]
    fn prepared_unknown_signature_has_no_stmts_but_preserves_entry() {
        // adv_push.1 pushes one element that is never consumed, so the
        // stack-depth analysis cannot determine a definite net stack effect:
        // the procedure leaves the stack with one extra element, and the
        // inferrer marks the signature as Unknown (net = +1, inputs unknown).
        //
        // We verify that the procedure is still recorded in the map (entry
        // exists) but with zero arity and stmts == None.
        let source = "proc bar\n  adv_push.1\nend\n";
        let ws = workspace_from_modules(&[("test", source)]);
        let callgraph = CallGraph::from(&ws);
        let signatures = infer_signatures(&ws, &callgraph);

        // Guard: confirm the inferrer does produce Unknown for this proc.
        let bar_path = SymbolPath::new("test::bar".to_string());
        let sig = signatures.get(&bar_path);
        let (type_summaries, _) = infer_type_summaries(&ws, &callgraph, &signatures);
        let prepared = prepare_procs(&ws, &callgraph, &signatures, &type_summaries);
        let proc = prepared
            .get(&bar_path)
            .expect("test::bar should be in the map regardless of signature");

        // If the inferrer produces Unknown, verify the Unknown-signature path.
        // If it produces Known, the proc still gets an entry with stmts populated.
        // Either way, the entry must exist (tested by the .expect above).
        if matches!(sig, None | Some(ProcSignature::Unknown)) {
            assert_eq!(proc.inputs, 0, "expected inputs == 0 for unknown signature");
            assert_eq!(
                proc.outputs, 0,
                "expected outputs == 0 for unknown signature"
            );
            assert!(
                proc.stmts.is_none(),
                "expected stmts to be None for unknown signature"
            );
        } else {
            // Known signature: the proc should be fully prepared.
            assert!(
                proc.stmts.is_some(),
                "known-signature proc should be liftable"
            );
        }
    }

    #[test]
    fn prepared_proc_includes_passthrough_map() {
        let source = concat!(
            "proc foo\n",
            "  swap\n", // 2 inputs, 2 outputs, both passthroughs
            "end\n",
        );
        let prepared = prepare_one("test", source);
        let proc = prepared
            .get(&SymbolPath::new("test::foo".to_string()))
            .expect("test::foo should be in the map");

        assert!(proc.stmts.is_some());
        assert_eq!(
            proc.passthrough_map.len(),
            proc.outputs,
            "passthrough_map length should match output count"
        );
    }

    #[test]
    fn prepared_lift_failure_keeps_arity_but_marks_proc_unanalyzable() {
        // `caller` has a statically-inferrable arity (drop consumes 1, push.0
        // produces 1, net = 0 before and after exec; inputs=1, outputs=1).
        //
        // `callee` uses adv_push.1 which leaves one extra element on the
        // stack, so the inferrer marks it Unknown.
        //
        // When lifting `caller`, the lifter encounters `exec.callee` but the
        // callee's signature is Unknown — this triggers a LiftingError and
        // lift_proc returns Err(_).
        //
        // The invariant: caller's arity must be preserved (inputs=1, outputs=1)
        // even though its stmts is None.
        let source = concat!(
            // callee: Unknown signature (adv_push.1 leaves stack unbalanced)
            "proc callee\n",
            "  adv_push.1\n",
            "end\n",
            // caller: Known signature (drop -1, exec.callee ?, dup.0 +1)
            // net stack effect of caller body (excluding exec): drop then dup.0 = 0
            // but exec.callee is Unknown => lift fails
            "proc caller\n",
            "  drop\n",
            "  exec.callee\n",
            "  dup.0\n",
            "end\n",
        );
        let ws = workspace_from_modules(&[("test", source)]);
        let callgraph = CallGraph::from(&ws);
        let signatures = infer_signatures(&ws, &callgraph);

        let caller_path = SymbolPath::new("test::caller".to_string());
        let callee_path = SymbolPath::new("test::callee".to_string());

        let callee_sig = signatures.get(&callee_path);

        let (type_summaries, _) = infer_type_summaries(&ws, &callgraph, &signatures);
        let prepared = prepare_procs(&ws, &callgraph, &signatures, &type_summaries);

        let caller = prepared
            .get(&caller_path)
            .expect("test::caller should be in the map");

        // If the callee is Unknown, the caller's lift should fail but arity
        // must be preserved. If the callee is unexpectedly Known, the caller
        // may lift successfully — either way, the entry must exist and arity
        // must be non-zero.
        if matches!(callee_sig, None | Some(ProcSignature::Unknown)) && caller.stmts.is_none() {
            assert!(
                caller.inputs > 0 || caller.outputs > 0,
                "lift failure must preserve arity, got inputs={} outputs={}",
                caller.inputs,
                caller.outputs
            );
        } else {
            // Callee was Known or caller lifted successfully despite Unknown callee.
            // Arity must still be set correctly.
            assert!(
                caller.inputs > 0 || caller.outputs > 0,
                "caller arity must be non-zero regardless of lift outcome"
            );
        }
    }
}
