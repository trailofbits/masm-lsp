//! Tests for the diagnostics module.
//!
//! These tests verify that parse errors and unresolved references are correctly
//! converted to LSP diagnostics.

mod common;

use common::fixtures::inline;
use common::fixtures::load_fixture;
use common::harness::TestHarness;
use masm_lsp::masm::diagnostics::{SOURCE_ANALYSIS, SOURCE_DECOMPILATION};
use tower_lsp::lsp_types::Diagnostic;
use tower_lsp::lsp_types::DiagnosticSeverity;

fn analysis_warnings(diagnostics: &[Diagnostic]) -> Vec<&Diagnostic> {
    diagnostics
        .iter()
        .filter(|diag| {
            diag.severity == Some(DiagnosticSeverity::WARNING)
                && diag.source.as_deref() == Some(SOURCE_ANALYSIS)
        })
        .collect()
}

fn analysis_warning_containing<'a>(
    diagnostics: &'a [Diagnostic],
    needle: &str,
) -> Option<&'a Diagnostic> {
    analysis_warnings(diagnostics)
        .into_iter()
        .find(|diag| diag.message.contains(needle))
}

#[tokio::test]
async fn valid_source_produces_no_diagnostics() {
    let harness = TestHarness::new().await;
    let uri = harness.open_inline("valid.masm", inline::SIMPLE_PROC).await;

    // Wait briefly for async processing
    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn syntax_error_produces_diagnostics() {
    let harness = TestHarness::new().await;
    let uri = harness
        .open_inline("error.masm", inline::SYNTAX_ERROR_MISSING_END)
        .await;

    tokio::task::yield_now().await;

    harness.assert_has_diagnostics(&uri).await;
}

#[tokio::test]
async fn invalid_nested_control_flow_produces_decompilation_diagnostic() {
    let harness = TestHarness::new().await;
    let content = load_fixture("nested_blocks_invalid.masm");

    // Invalid but parseable control flow should surface a decompilation diagnostic
    // instead of crashing the server during open-time diagnostics.
    let uri = harness.open_inline("nested_invalid.masm", &content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let decompilation_diag = diags.iter().find(|diag| {
        diag.source.as_deref() == Some(SOURCE_DECOMPILATION)
            && diag.message.contains("could not decompile procedure")
    });
    assert!(
        decompilation_diag.is_some(),
        "expected decompilation diagnostic for invalid nested control flow, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn unresolved_reference_produces_diagnostic() {
    let harness = TestHarness::new().await;
    // Use valid MASM syntax with an unresolved external reference
    let content = r#"proc foo
    exec.::missing::nonexistent
end
"#;
    let uri = harness.open_inline("unresolved.masm", content).await;

    tokio::task::yield_now().await;

    harness.assert_has_diagnostics(&uri).await;
    harness.assert_diagnostic_contains(&uri, "Unresolved").await;
}

#[tokio::test]
async fn diagnostics_have_error_severity() {
    let harness = TestHarness::new().await;
    let uri = harness
        .open_inline("error_severity.masm", inline::UNRESOLVED_REF)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    for diag in &diags {
        assert_eq!(
            diag.severity,
            Some(DiagnosticSeverity::ERROR),
            "expected ERROR severity, got {:?}",
            diag.severity
        );
    }
}

#[tokio::test]
async fn signature_mismatch_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc foo(a: u64) -> u64
    nop
end
"#;
    let uri = harness.open_inline("sig_mismatch.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning = diags.iter().find(|diag| {
        diag.severity == Some(DiagnosticSeverity::WARNING)
            && diag.source.as_deref() == Some(SOURCE_ANALYSIS)
            && diag.message.contains("definition declares")
            && diag.message.contains("inferred")
    });
    assert!(
        warning.is_some(),
        "expected signature mismatch warning, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn type_inconsistency_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc needs_bool
    if.true
        nop
    else
        nop
    end
end

proc caller_bad_bool
    push.2
    exec.needs_bool
end
"#;
    let uri = harness.open_inline("type_mismatch.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning = diags.iter().find(|diag| {
        diag.severity == Some(DiagnosticSeverity::WARNING)
            && diag.source.as_deref() == Some(SOURCE_ANALYSIS)
            && diag.message.contains("expects Bool")
    });
    assert!(
        warning.is_some(),
        "expected type inconsistency warning, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn unknown_type_does_not_emit_mismatch_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc needs_bool
    if.true
        nop
    else
        nop
    end
end

proc caller_unknown_bool
    adv_push.1
    exec.needs_bool
end
"#;
    let uri = harness.open_inline("type_unknown.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let analysis_warnings: Vec<_> = diags
        .iter()
        .filter(|diag| {
            diag.severity == Some(DiagnosticSeverity::WARNING)
                && diag.source.as_deref() == Some(SOURCE_ANALYSIS)
        })
        .collect();
    assert!(
        analysis_warnings.is_empty(),
        "expected no analysis warnings for unknown types, got: {:?}",
        analysis_warnings
    );
}

#[tokio::test]
async fn unresolved_modules_suppress_unconstrained_advice_warnings() {
    let harness = TestHarness::new().await;
    let content = r#"use ::tmp::missing::add->plus

proc bad
    adv_push.1
    push.1
    u32wrapping_add
    exec.plus
end
"#;
    let uri = harness
        .open_inline("advice_unresolved_module_guard.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        analysis_warnings(&diags).is_empty(),
        "expected unresolved modules to suppress analysis warnings, got: {:?}",
        diags
    );
    assert!(
        diags.iter().any(|diag| diag.source.as_deref() != Some(SOURCE_ANALYSIS)),
        "expected a non-analysis diagnostic from the unresolved dependency, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn unconstrained_advice_to_u32_operation_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc bad
    adv_push.1
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_u32_op.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning =
        analysis_warning_containing(&diags, "Unconstrained advice reaches a u32 operation")
            .expect("expected unconstrained-advice warning");
    let related = warning
        .related_information
        .as_ref()
        .expect("expected related source information");
    assert!(
        related.iter().any(|info| info.location.uri == uri),
        "expected source-related information in the same file, got: {:?}",
        related
    );
}

#[tokio::test]
async fn unconstrained_advice_to_u32_call_argument_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc needs_u32
    push.1
    u32wrapping_add
end

proc caller
    adv_push.1
    exec.needs_u32
end
"#;
    let uri = harness.open_inline("advice_u32_call.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings
            .iter()
            .any(|diag| diag.message.contains("expects U32")),
        "expected call-argument unconstrained-advice warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn u32cast_sanitizes_unconstrained_advice_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc ok
    adv_push.1
    u32cast
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_u32cast.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.is_empty(),
        "expected no analysis warnings after u32cast, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn direct_u32_intrinsic_sink_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc bad
    adv_push.1
    push.1
    u32overflowing_add
    drop
end
"#;
    let uri = harness
        .open_inline("advice_u32_intrinsic.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning =
        analysis_warning_containing(&diags, "Unconstrained advice reaches a u32 intrinsic")
            .expect("expected unconstrained-advice intrinsic warning");
    assert_eq!(
        warning.source.as_deref(),
        Some(SOURCE_ANALYSIS),
        "expected analysis diagnostic, got: {warning:?}"
    );
}

#[tokio::test]
async fn local_round_trip_of_unconstrained_advice_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc local_bad
    adv_push.1
    loc_store.0
    loc_load.0
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_locals.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().any(|diag| diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected local-flow unconstrained-advice warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn interprocedural_unconstrained_advice_output_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc source
    adv_push.1
end

proc caller
    exec.source
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_interproc.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().any(|diag| diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected interprocedural unconstrained-advice warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn callee_returning_local_advice_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"@locals(1)
proc source
    adv_push.1
    loc_store.0
    loc_load.0
end

proc caller
    exec.source
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_interproc_local.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().any(|diag| diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected local interprocedural unconstrained-advice warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn u32_intrinsic_output_is_sanitized_for_follow_on_u32_use() {
    let harness = TestHarness::new().await;
    let content = r#"proc source
    adv_push.1
    push.1
    u32overflowing_add
    drop
end

proc caller
    exec.source
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_u32_intrinsic_sanitized.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().all(|diag| !diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected no follow-on u32 warning from sanitized intrinsic output, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn is_odd_output_does_not_trigger_unconstrained_advice_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc ok
    adv_push.1
    is_odd
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_is_odd.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        analysis_warning_containing(&diags, "Unconstrained advice").is_none(),
        "expected no unconstrained-advice warning after is_odd, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn unconstrained_advice_warning_includes_multiple_related_sources() {
    let harness = TestHarness::new().await;
    let content = r#"proc bad
    adv_push.1
    adv_push.1
    add
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_multi_source.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning =
        analysis_warning_containing(&diags, "Unconstrained advice reaches a u32 operation")
            .expect("expected unconstrained-advice warning");
    let related = warning
        .related_information
        .as_ref()
        .expect("expected related source information");
    assert_eq!(
        related.len(),
        2,
        "expected two related advice sources, got: {:?}",
        related
    );
}

#[tokio::test]
async fn interprocedural_warning_includes_cross_file_related_source() {
    let harness = TestHarness::new().await;
    let source_uri = harness
        .open_inline("utils.masm", "proc source\n    adv_push.1\nend\n")
        .await;
    let caller_uri = harness
        .open_inline(
            "caller.masm",
            "proc caller\n    exec.utils::source\n    push.1\n    u32wrapping_add\nend\n",
        )
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&caller_uri).await;
    let warning =
        analysis_warning_containing(&diags, "Unconstrained advice reaches a u32 operation")
            .expect("expected unconstrained-advice warning");
    let related = warning
        .related_information
        .as_ref()
        .expect("expected related source information");
    assert!(
        related.iter().any(|info| info.location.uri == source_uri),
        "expected cross-file related source information, got: {:?}",
        related
    );
}

#[tokio::test]
async fn adv_pipe_to_u32_operation_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc pipe_bad
    padw
    padw
    padw
    push.0
    adv_pipe
    push.1
    u32wrapping_add
end
"#;
    let uri = harness.open_inline("advice_adv_pipe.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().any(|diag| diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected adv_pipe unconstrained-advice warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn word_local_round_trip_tracks_slots_precisely() {
    let harness = TestHarness::new().await;
    let content = r#"@locals(8)
proc ok
    adv_push.1
    push.2
    push.3
    push.4
    loc_storew_be.0
    dropw

    push.0.0.0.0
    loc_loadw_be.0
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_word_locals_ok.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().all(|diag| !diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected no u32 warning for clean top word element, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn little_endian_word_local_round_trip_tracks_slots_precisely() {
    let harness = TestHarness::new().await;
    let content = r#"@locals(8)
proc ok
    adv_push.1
    push.2
    push.3
    push.4
    loc_storew_le.0
    dropw

    push.0.0.0.0
    loc_loadw_le.0
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_word_locals_le_ok.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings.iter().all(|diag| !diag
            .message
            .contains("Unconstrained advice reaches a u32 operation")),
        "expected no u32 warning for clean LE top word element, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn mixed_endian_word_local_round_trip_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"@locals(8)
proc bad
    adv_push.1
    push.2
    push.3
    push.4
    loc_storew_be.0
    dropw

    push.0.0.0.0
    loc_loadw_le.0
    push.1
    u32wrapping_add
end
"#;
    let uri = harness
        .open_inline("advice_word_locals_mixed_bad.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning =
        analysis_warning_containing(&diags, "Unconstrained advice reaches a u32 operation")
            .expect("expected mixed-endian unconstrained-advice warning");
    assert_eq!(
        warning.source.as_deref(),
        Some(SOURCE_ANALYSIS),
        "expected analysis diagnostic, got: {warning:?}"
    );
}

#[tokio::test]
async fn unconstrained_advice_to_divisor_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc bad
    push.2
    adv_push.1
    div
end
"#;
    let uri = harness.open_inline("advice_divisor.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning = analysis_warning_containing(&diags, "divisor")
        .expect("expected divisor unconstrained-advice warning");
    let related = warning
        .related_information
        .as_ref()
        .expect("expected related source information");
    assert!(
        related.iter().any(|info| info.location.uri == uri),
        "expected source-related information in the same file, got: {:?}",
        related
    );
}

#[tokio::test]
async fn unconstrained_advice_to_inv_produces_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc bad
    adv_push.1
    inv
end
"#;
    let uri = harness.open_inline("advice_inv.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warning = analysis_warning_containing(&diags, "inv")
        .expect("expected inv unconstrained-advice warning");
    assert_eq!(
        warning.source.as_deref(),
        Some(SOURCE_ANALYSIS),
        "expected analysis diagnostic, got: {warning:?}"
    );
}

#[tokio::test]
async fn interprocedural_nonzero_requirement_produces_call_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc helper
    inv
end

proc caller
    adv_push.1
    exec.helper
end
"#;
    let uri = harness
        .open_inline("advice_nonzero_call.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    let warnings = analysis_warnings(&diags);
    assert!(
        warnings
            .iter()
            .any(|diag| diag.message.contains("Argument 0") && diag.message.contains("divisor")),
        "expected interprocedural non-zero call warning, got: {:?}",
        warnings
    );
}

#[tokio::test]
async fn eq_zero_assertz_suppresses_divisor_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc ok
    push.2
    adv_push.1
    dup.0
    eq.0
    assertz
    div
end
"#;
    let uri = harness
        .open_inline("advice_divisor_assertz.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        analysis_warning_containing(&diags, "divisor").is_none(),
        "expected no divisor warning, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn eq_zero_else_branch_suppresses_divisor_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc ok
    push.2
    adv_push.1
    dup.0
    eq.0
    if.true
        drop
    else
        div
    end
end
"#;
    let uri = harness
        .open_inline("advice_divisor_if_else.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        analysis_warning_containing(&diags, "divisor").is_none(),
        "expected no divisor warning, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn proven_nonzero_local_round_trip_suppresses_divisor_warning() {
    let harness = TestHarness::new().await;
    let content = r#"proc ok
    push.2
    adv_push.1
    dup.0
    eq.0
    assertz
    loc_store.0
    loc_load.0
    div
end
"#;
    let uri = harness
        .open_inline("advice_divisor_local_ok.masm", content)
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        analysis_warning_containing(&diags, "divisor").is_none(),
        "expected no divisor warning, got: {:?}",
        diags
    );
}

#[tokio::test]
async fn empty_file_produces_expected_diagnostic() {
    let harness = TestHarness::new().await;
    let uri = harness.open_inline("empty.masm", inline::EMPTY).await;

    tokio::task::yield_now().await;

    // Empty files produce "unexpected end of file" from the MASM parser
    harness.assert_has_diagnostics(&uri).await;
}

#[tokio::test]
async fn proc_with_local_calls_produces_no_diagnostics() {
    let harness = TestHarness::new().await;
    // Test that a proc can call another local proc without errors
    let content = r#"proc helper
    push.1
end

proc caller
    exec.helper
end
"#;
    let uri = harness.open_inline("local_calls.masm", content).await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn diagnostics_cleared_when_error_fixed() {
    let harness = TestHarness::new().await;
    let uri = harness
        .open_inline("fixable.masm", inline::UNRESOLVED_REF)
        .await;

    tokio::task::yield_now().await;

    // First check we have diagnostics
    harness.assert_has_diagnostics(&uri).await;

    // Clear the published diagnostics
    harness.client.take_published().await;

    // Now "fix" the file by adding the missing proc
    let fixed_content = r#"proc nonexistent
    nop
end

proc foo
    exec.nonexistent
end
"#;
    // Re-open with fixed content
    harness
        .open_doc(uri.clone(), fixed_content.to_string())
        .await;

    tokio::task::yield_now().await;

    // Now diagnostics should be empty
    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn diagnostic_range_points_to_error_location() {
    let harness = TestHarness::new().await;
    let content = r#"proc foo
    exec.missing
end
"#;
    let uri = harness.open_inline("range_test.masm", content).await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(!diags.is_empty(), "expected at least one diagnostic");

    // The diagnostic should point somewhere in the file (line >= 0)
    let diag = &diags[0];
    // exec.missing is on line 1
    assert!(
        diag.range.start.line <= 2,
        "diagnostic should be within the file, got line {}",
        diag.range.start.line
    );
}

#[tokio::test]
async fn aliased_module_call_resolves_when_target_in_workspace() {
    // This test verifies the fix for the bug where calls through module aliases
    // (e.g., `use std::stark::constants` then `exec.constants::proc`) were
    // incorrectly marked as unresolved even when the target existed in workspace.
    let harness = TestHarness::new().await;

    // First, open a file that defines the target procedure
    let lib_content = r#"proc target_proc
    push.1
end
"#;
    let _lib_uri = harness.open_inline("utils.masm", lib_content).await;

    tokio::task::yield_now().await;

    // Now open a file that calls the procedure using a qualified path
    // The reference path (utils::target_proc) should resolve via workspace suffix matching
    let caller_content = r#"proc caller
    exec.utils::target_proc
end
"#;
    let caller_uri = harness.open_inline("caller.masm", caller_content).await;

    tokio::task::yield_now().await;

    // The call to utils::target_proc should NOT produce an unresolved diagnostic
    // because the target exists in the workspace and can be found by suffix/name
    let diags = harness.client.diagnostics_for(&caller_uri).await;
    let unresolved_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("unresolved"))
        .collect();
    assert!(
        unresolved_diags.is_empty(),
        "expected no unresolved diagnostics for cross-module call, got: {:?}",
        unresolved_diags
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn analysis_resolves_calls_into_other_open_documents() {
    let harness = TestHarness::new().await;

    let _callee_uri = harness
        .open_inline("utils.masm", "proc target_proc\n    push.1\nend\n")
        .await;
    let caller_uri = harness
        .open_inline(
            "caller.masm",
            "proc caller\n    exec.utils::target_proc\nend\n",
        )
        .await;

    tokio::task::yield_now().await;

    let diags = harness.client.diagnostics_for(&caller_uri).await;
    assert!(
        diags
            .iter()
            .all(|diag| diag.source.as_deref() != Some(SOURCE_ANALYSIS)),
        "expected no analysis diagnostics for cross-open-document call, got: {:?}",
        diags
    );
    assert!(
        diags
            .iter()
            .all(|diag| !diag.message.contains("Unresolved invocation target")),
        "expected no unresolved invocation diagnostics, got: {:?}",
        diags
    );
}
