mod corpus;
mod stdlib_eval;

use masm_decompiler::frontend::testing::workspace_from_modules;

use super::{
    AdviceDiagnostic, AdviceDiagnosticsMap, AdviceSinkKind, CallArgumentRequirement,
    group_advice_diagnostics_by_origin,
};
use crate::infer_unconstrained_advice_in_workspace;

fn diagnostics_for(diags: &AdviceDiagnosticsMap, proc: &str) -> Vec<AdviceDiagnostic> {
    diags
        .get(&crate::SymbolPath::new(proc.to_string()))
        .cloned()
        .unwrap_or_default()
}

fn is_u32_relevant_diagnostic(diag: &AdviceDiagnostic) -> bool {
    matches!(
        diag.sink,
        AdviceSinkKind::U32Expression | AdviceSinkKind::U32Intrinsic
    ) || matches!(diag.call_requirement, Some(CallArgumentRequirement::U32))
}

#[test]
fn direct_adv_push_to_u32_expr_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("unconstrained advice")),
        "expected advice diagnostic, got: {bad:?}"
    );
}

#[test]
fn direct_adv_push_to_u32_intrinsic_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    push.1\n    u32overflowing_add\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter().any(|diag| diag.message.contains("u32 intrinsic")),
        "expected u32 intrinsic diagnostic, got: {bad:?}"
    );
}

#[test]
fn u32cast_sanitizes_downstream_use() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    adv_push.1\n    u32cast\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(ok.is_empty(), "expected no diagnostics, got: {ok:?}");
}

#[test]
fn local_round_trip_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc local_bad\n    adv_push.1\n    loc_store.0\n    loc_load.0\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let local_bad = diagnostics_for(&diagnostics, "advice::local_bad");
    assert!(
        local_bad
            .iter()
            .any(|diag| diag.message.contains("unconstrained advice")),
        "expected advice diagnostic, got: {local_bad:?}"
    );
}

#[test]
fn word_local_round_trip_tracks_little_endian_slots_precisely() {
    let ws = workspace_from_modules(&[(
        "advice",
        "@locals(8)\nproc ok\n    adv_push.1\n    push.2\n    push.3\n    push.4\n    loc_storew_le.0\n    dropw\n    push.0.0.0.0\n    loc_loadw_le.0\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter().all(|diag| !diag.message.contains("u32 operation")),
        "expected no u32 diagnostics for clean LE top word element, got: {ok:?}"
    );
}

#[test]
fn word_local_round_trip_tracks_slots_precisely() {
    let ws = workspace_from_modules(&[(
        "advice",
        "@locals(8)\nproc ok\n    adv_push.1\n    push.2\n    push.3\n    push.4\n    loc_storew_be.0\n    dropw\n    push.0.0.0.0\n    loc_loadw_be.0\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter()
            .all(|diag| !diag.message.contains("u32 operation")),
        "expected no u32 diagnostics for clean top word element, got: {ok:?}"
    );
}

#[test]
fn word_local_mixed_endianness_reaches_u32_sink() {
    let ws = workspace_from_modules(&[(
        "advice",
        "@locals(8)\nproc bad\n    adv_push.1\n    push.2\n    push.3\n    push.4\n    loc_storew_be.0\n    dropw\n    push.0.0.0.0\n    loc_loadw_le.0\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter().any(|diag| diag.message.contains("u32 operation")),
        "expected mixed-endian word-local diagnostic, got: {bad:?}"
    );
}

#[test]
fn call_argument_warning_uses_callee_u32_requirement() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc needs_u32\n    push.1\n    u32wrapping_add\nend\n\nproc caller\n    adv_push.1\n    exec.needs_u32\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller.iter().any(|diag| diag.arg_index == Some(0)),
        "expected call-argument advice diagnostic, got: {caller:?}"
    );
    assert!(
        caller.iter().any(|diag| {
            diag.call_requirement == Some(CallArgumentRequirement::U32)
                && is_u32_relevant_diagnostic(diag)
        }),
        "expected structured u32 call-argument classification, got: {caller:?}"
    );
    assert!(
        caller.iter().any(|diag| !diag.origins.is_empty()),
        "expected call-argument origin spans, got: {caller:?}"
    );
}

#[test]
fn u32_filter_excludes_nonzero_call_arguments() {
    let mut u32_call = AdviceDiagnostic::new(
        crate::SymbolPath::new("advice::caller".to_string()),
        miden_debug_types::SourceSpan::UNKNOWN,
        AdviceSinkKind::CallArgument,
        "u32 call",
    );
    u32_call.call_requirement = Some(CallArgumentRequirement::U32);

    let mut nonzero_call = AdviceDiagnostic::new(
        crate::SymbolPath::new("advice::caller".to_string()),
        miden_debug_types::SourceSpan::UNKNOWN,
        AdviceSinkKind::CallArgument,
        "nonzero call",
    );
    nonzero_call.call_requirement = Some(CallArgumentRequirement::NonZero);

    assert!(is_u32_relevant_diagnostic(&u32_call));
    assert!(!is_u32_relevant_diagnostic(&nonzero_call));
}

#[test]
fn callee_output_summary_propagates_advice() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc source\n    adv_push.1\nend\n\nproc caller\n    exec.source\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .any(|diag| diag.message.contains("unconstrained advice")),
        "expected propagated advice diagnostic, got: {caller:?}"
    );
}

#[test]
fn callee_forwarding_input_taint_propagates_to_caller() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc forward\n    dup.0\n    drop\nend\n\nproc caller\n    adv_push.1\n    exec.forward\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .any(|diag| diag.message.contains("unconstrained advice")),
        "expected forwarded-input diagnostic, got: {caller:?}"
    );
}

#[test]
fn branch_join_overtaints_conservatively() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc maybe_bad\n    push.1\n    if.true\n        adv_push.1\n    else\n        push.2\n    end\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let maybe_bad = diagnostics_for(&diagnostics, "advice::maybe_bad");
    assert!(
        maybe_bad.iter().any(|diag| diag.message.contains("u32 operation")),
        "expected conservative branch-join diagnostic, got: {maybe_bad:?}"
    );
}

#[test]
fn origin_grouping_deduplicates_each_origin_per_diagnostic() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    dup\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);

    let groups = group_advice_diagnostics_by_origin(&diagnostics);

    assert_eq!(groups.len(), 1, "expected one root-cause group, got: {groups:?}");
    assert_eq!(groups[0].sink_count(), 1);
}

#[test]
fn origin_grouping_sorts_largest_fanout_first() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    dup\n    push.1\n    u32wrapping_add\n    push.1\n    u32wrapping_add\nend\n\nproc other\n    adv_push.1\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);

    let groups = group_advice_diagnostics_by_origin(&diagnostics);

    assert!(
        groups.len() >= 2,
        "expected at least two root-cause groups, got: {groups:?}"
    );
    assert!(groups[0].sink_count() >= groups[1].sink_count());
}

#[test]
fn u32_intrinsic_outputs_are_sanitized() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc source\n    adv_push.1\n    push.1\n    u32overflowing_add\n    drop\nend\n\nproc caller\n    exec.source\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .all(|diag| !diag.message.contains("u32 operation")),
        "expected no downstream warning from sanitized intrinsic output, got: {caller:?}"
    );
}

#[test]
fn is_odd_output_does_not_carry_unconstrained_advice() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    adv_push.1\n    is_odd\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter()
            .all(|diag| !diag.message.contains("unconstrained advice")),
        "expected no unconstrained-advice diagnostics after is_odd, got: {ok:?}"
    );
}

#[test]
fn adv_pipe_results_are_tainted() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc pipe_bad\n    padw\n    padw\n    padw\n    push.0\n    adv_pipe\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let pipe_bad = diagnostics_for(&diagnostics, "advice::pipe_bad");
    assert!(
        pipe_bad
            .iter()
            .any(|diag| diag.message.contains("unconstrained advice")),
        "expected adv_pipe advice diagnostic, got: {pipe_bad:?}"
    );
}

#[test]
fn interprocedural_u32assert_sanitizes_mem_load_address() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc sanitize\n    u32assert\nend\n\nproc caller\n    adv_push.1\n    exec.sanitize\n    mem_load\nend\n",
    )]);
    let (summaries, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let sanitize = summaries
        .get(&crate::SymbolPath::new("advice::sanitize".to_string()))
        .expect("expected sanitize summary");
    assert_eq!(
        sanitize.u32_inputs().first().copied(),
        Some(super::u32_domain::U32Validity::ProvenU32),
        "expected sanitize summary to preserve a proven-u32 input postcondition, got: {sanitize:?}"
    );
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .all(|diag| !diag.message.contains("memory address")),
        "expected no interprocedural memory address diagnostic after u32assert, got: {caller:?}"
    );
}

#[test]
fn diagnostics_retain_multiple_advice_origins() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    adv_push.1\n    add\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter().any(|diag| diag.origins.len() == 2),
        "expected two origin spans, got: {bad:?}"
    );
}

#[test]
fn direct_adv_push_to_divisor_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    push.2\n    adv_push.1\n    div\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter().any(|diag| diag.message.contains("divisor")),
        "expected divisor diagnostic, got: {bad:?}"
    );
}

#[test]
fn direct_adv_push_to_inv_warns() {
    let ws = workspace_from_modules(&[("advice", "proc bad\n    adv_push.1\n    inv\nend\n")]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter().any(|diag| diag.message.contains("divisor")),
        "expected non-zero diagnostic, got: {bad:?}"
    );
}

#[test]
fn interprocedural_nonzero_requirement_warns_at_call_site() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc helper\n    inv\nend\n\nproc caller\n    adv_push.1\n    exec.helper\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .any(|diag| diag.message.contains("divisor") || diag.message.contains("argument 0")),
        "expected interprocedural non-zero diagnostic, got: {caller:?}"
    );
}

#[test]
fn opaque_callee_suppresses_output_claims() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc target\n    nop\nend\n\nproc opaque\n    procref.target\n    mem_storew_le.40\n    dropw\n    push.40\n    dynexec\n    adv_push.1\nend\n\nproc caller\n    exec.opaque\n    push.1\n    u32wrapping_add\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .all(|diag| !diag.message.contains("u32 operation")),
        "expected opaque callee to suppress downstream claims, got: {caller:?}"
    );
}

#[test]
fn eq_zero_assertz_suppresses_divisor_warning() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    push.2\n    adv_push.1\n    dup.0\n    eq.0\n    assertz\n    div\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter().all(|diag| !diag.message.contains("divisor")),
        "expected no divisor diagnostics, got: {ok:?}"
    );
}

#[test]
fn eq_zero_else_branch_suppresses_divisor_warning() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    push.2\n    adv_push.1\n    dup.0\n    eq.0\n    if.true\n        drop\n    else\n        div\n    end\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter().all(|diag| !diag.message.contains("divisor")),
        "expected no divisor diagnostics, got: {ok:?}"
    );
}

#[test]
fn proven_nonzero_value_round_trips_through_local() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    push.2\n    adv_push.1\n    dup.0\n    eq.0\n    assertz\n    loc_store.0\n    loc_load.0\n    div\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter().all(|diag| !diag.message.contains("divisor")),
        "expected no divisor diagnostics, got: {ok:?}"
    );
}

#[test]
fn direct_adv_push_to_mem_store_address_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    push.42\n    swap\n    mem_store\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected memory address diagnostic, got: {bad:?}"
    );
    assert!(
        bad.iter()
            .any(|diag| diag.sink == super::AdviceSinkKind::MemoryAddress),
        "expected MemoryAddress sink kind, got: {bad:?}"
    );
}

#[test]
fn direct_adv_push_to_mem_load_address_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    mem_load\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected memory address diagnostic for mem_load, got: {bad:?}"
    );
}

#[test]
fn indirect_advice_to_address_via_arithmetic_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    push.4\n    mul\n    mem_load\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected memory address diagnostic for indirect flow, got: {bad:?}"
    );
}

#[test]
fn u32assert_sanitizes_mem_store_address() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    adv_push.1\n    u32assert\n    push.42\n    swap\n    mem_store\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter()
            .all(|diag| !diag.message.contains("memory address")),
        "expected no memory address diagnostics after u32assert, got: {ok:?}"
    );
}

#[test]
fn adv_pipe_tainted_address_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    padw\n    padw\n    padw\n    adv_pipe\n    dropw\n    dropw\n    dropw\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected memory address diagnostic for adv_pipe, got: {bad:?}"
    );
}

#[test]
fn mem_stream_tainted_address_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.1\n    padw\n    padw\n    padw\n    mem_stream\n    dropw\n    dropw\n    dropw\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected memory address diagnostic for mem_stream, got: {bad:?}"
    );
}

#[test]
fn mem_store_values_not_flagged_as_address() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    adv_push.1\n    push.100\n    mem_store\n    drop\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter()
            .all(|diag| !diag.message.contains("memory address")),
        "expected no memory address diagnostics when advice is in values, got: {ok:?}"
    );
}

#[test]
fn interprocedural_advice_to_address_warns() {
    let ws = workspace_from_modules(&[(
        "advice",
        "proc source\n    adv_push.1\nend\n\nproc caller\n    exec.source\n    mem_load\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .any(|diag| diag.message.contains("memory address")),
        "expected interprocedural memory address diagnostic, got: {caller:?}"
    );
}

// ---------------------------------------------------------------------------
// Merkle root detection tests
// ---------------------------------------------------------------------------

#[test]
fn direct_advice_to_mtree_get_root_warns() {
    // Stack for mtree_get: [d, i, R0, R1, R2, R3] (top-first)
    // Advice fills the root word (positions 2..6).
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.4\n    push.0\n    push.0\n    mtree_get\n    dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("Merkle tree root")),
        "expected Merkle root diagnostic for mtree_get, got: {bad:?}"
    );
    assert!(
        bad.iter()
            .any(|diag| diag.sink == super::AdviceSinkKind::MerkleRoot),
        "expected MerkleRoot sink kind, got: {bad:?}"
    );
}

#[test]
fn direct_advice_to_mtree_set_root_warns() {
    // Stack for mtree_set: [d, i, R0..R3, V0..V3] (top-first)
    // Advice fills the root word (positions 2..6).
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    padw\n    adv_push.4\n    push.0\n    push.0\n    mtree_set\n    dropw\n    dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("Merkle tree root")),
        "expected Merkle root diagnostic for mtree_set, got: {bad:?}"
    );
}

#[test]
fn direct_advice_to_mtree_verify_root_warns() {
    // Stack for mtree_verify: [V0..V3, d, i, R0..R3] (top-first)
    // Advice fills the root word (positions 6..10).
    let ws = workspace_from_modules(&[(
        "advice",
        "proc bad\n    adv_push.4\n    push.0\n    push.0\n    padw\n    mtree_verify\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let bad = diagnostics_for(&diagnostics, "advice::bad");
    assert!(
        bad.iter()
            .any(|diag| diag.message.contains("Merkle tree root")),
        "expected Merkle root diagnostic for mtree_verify, got: {bad:?}"
    );
}

#[test]
fn advice_in_non_root_position_not_flagged() {
    // Advice flows into the depth/index positions of mtree_get (positions 0..2),
    // NOT into the root word. No MerkleRoot diagnostic expected.
    let ws = workspace_from_modules(&[(
        "advice",
        "proc ok\n    push.1.2.3.4\n    adv_push.1\n    adv_push.1\n    mtree_get\n    dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let ok = diagnostics_for(&diagnostics, "advice::ok");
    assert!(
        ok.iter()
            .all(|diag| !diag.message.contains("Merkle tree root")),
        "expected no Merkle root diagnostics for non-root advice, got: {ok:?}"
    );
}

#[test]
fn interprocedural_advice_to_merkle_root_warns() {
    // Callee returns advice, caller uses it as mtree_get root.
    let ws = workspace_from_modules(&[(
        "advice",
        "proc source\n    adv_push.4\nend\n\nproc caller\n    exec.source\n    push.0\n    push.0\n    mtree_get\n    dropw\nend\n",
    )]);
    let (_, diagnostics) = infer_unconstrained_advice_in_workspace(&ws);
    let caller = diagnostics_for(&diagnostics, "advice::caller");
    assert!(
        caller
            .iter()
            .any(|diag| diag.message.contains("Merkle tree root")),
        "expected interprocedural Merkle root diagnostic, got: {caller:?}"
    );
    assert!(
        caller.iter().any(|diag| !diag.origins.is_empty()),
        "expected origin spans tracing to callee's adv_push, got: {caller:?}"
    );
}
