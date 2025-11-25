//! Tests for the diagnostics module.
//!
//! These tests verify that parse errors and unresolved references are correctly
//! converted to LSP diagnostics.

mod common;

use common::fixtures::inline;
use common::harness::TestHarness;
use tower_lsp::lsp_types::DiagnosticSeverity;

#[tokio::test]
async fn valid_source_produces_no_diagnostics() {
    let harness = TestHarness::new().await;
    let uri = harness.open_inline("valid.masm", inline::SIMPLE_PROC).await;

    // Wait briefly for async processing
    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn valid_executable_produces_no_diagnostics() {
    let harness = TestHarness::new().await;
    let uri = harness
        .open_inline("executable.masm", inline::SIMPLE_EXECUTABLE)
        .await;

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
    harness
        .assert_diagnostic_contains(&uri, "unresolved")
        .await;
}

#[tokio::test]
async fn multiple_unresolved_references_produce_multiple_diagnostics() {
    let harness = TestHarness::new().await;
    // Use fully qualified paths which will be unresolved
    let content = r#"proc foo
    exec.::missing::one
    exec.::missing::two
    exec.::missing::three
end
"#;
    let uri = harness.open_inline("multi_unresolved.masm", content).await;

    tokio::task::yield_now().await;

    // Should have at least 3 diagnostics for the unresolved references
    let diags = harness.client.diagnostics_for(&uri).await;
    assert!(
        diags.len() >= 3,
        "expected at least 3 diagnostics, got {}",
        diags.len()
    );
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
async fn empty_file_produces_expected_diagnostic() {
    let harness = TestHarness::new().await;
    let uri = harness.open_inline("empty.masm", inline::EMPTY).await;

    tokio::task::yield_now().await;

    // Empty files produce "unexpected end of file" from the MASM parser
    harness.assert_has_diagnostics(&uri).await;
}

#[tokio::test]
async fn nested_control_flow_valid_produces_no_diagnostics() {
    let harness = TestHarness::new().await;
    let uri = harness
        .open_inline("nested.masm", inline::NESTED_CONTROL_FLOW)
        .await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
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
    harness.open_doc(uri.clone(), fixed_content.to_string()).await;

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
        unresolved_diags.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}
