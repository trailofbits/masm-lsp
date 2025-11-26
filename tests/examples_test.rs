//! Integration tests using real example files.
//!
//! These tests verify that the LSP works correctly with actual MASM files
//! from the examples/ directory.

mod common;

use common::fixtures::{load_example, load_fixture, FixtureKind};
use common::harness::TestHarness;

#[tokio::test]
async fn fib_example_parses_without_errors() {
    let harness = TestHarness::new().await;
    let content = load_example("fib/fib.masm");
    let uri = harness.open_inline("fib.masm", &content).await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn sha256_example_parses_without_errors() {
    let harness = TestHarness::new().await;
    let content = load_example("hashing/sha256/sha256.masm");
    let uri = harness.open_inline("sha256.masm", &content).await;

    tokio::task::yield_now().await;

    // Note: This may have unresolved references to stdlib if stdlib is not loaded.
    // The test verifies parsing succeeds, not that all references resolve.
    let diags = harness.client.diagnostics_for(&uri).await;
    // Check that there are no syntax errors (unresolved refs are acceptable)
    for diag in &diags {
        assert!(
            diag.message.contains("unresolved") || !diag.message.contains("parse"),
            "unexpected error: {}",
            diag.message
        );
    }
}

#[tokio::test]
async fn multi_proc_fixture_parses_without_errors() {
    let harness = TestHarness::new().await;
    let uri = harness.open_fixture(FixtureKind::MultiProc).await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn multi_proc_fixture_goto_definition_works() {
    let harness = TestHarness::new().await;
    let content = load_fixture("multi_proc.masm");
    let uri = harness.open_inline("multi_proc.masm", &content).await;

    tokio::task::yield_now().await;

    // Find definition of "helper" from exec.helper
    let result = harness.goto_definition_at(&uri, &content, "helper").await;
    assert!(result.is_some(), "expected to find helper definition");
    let loc = result.unwrap();
    assert_eq!(loc.range.start.line, 0); // proc helper is on line 0
}

#[tokio::test]
async fn multi_proc_fixture_find_references_works() {
    let harness = TestHarness::new().await;
    let content = load_fixture("multi_proc.masm");
    let uri = harness.open_inline("multi_proc.masm", &content).await;

    tokio::task::yield_now().await;

    // Find references to "helper"
    let refs = harness
        .find_references_at(&uri, &content, "helper", true)
        .await;

    // Should find: definition (line 0), exec.helper in compute (line 5), exec.helper in main_logic (line 10)
    assert!(
        refs.len() >= 3,
        "expected at least 3 references to helper, got {}",
        refs.len()
    );
}

#[tokio::test]
async fn nested_blocks_fixture_parses_without_errors() {
    let harness = TestHarness::new().await;
    let uri = harness.open_fixture(FixtureKind::NestedBlocks).await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn nested_blocks_fixture_resolves_calls_in_blocks() {
    let harness = TestHarness::new().await;
    let content = load_fixture("nested_blocks.masm");
    let uri = harness.open_inline("nested.masm", &content).await;

    tokio::task::yield_now().await;

    // Should be able to find references to helper from within nested blocks
    let refs = harness
        .find_references_at(&uri, &content, "helper", true)
        .await;

    // Multiple calls to helper in various nested blocks
    assert!(
        refs.len() >= 4,
        "expected at least 4 references to helper in nested blocks, got {}",
        refs.len()
    );
}

#[tokio::test]
async fn alias_fixture_parses_without_errors() {
    let harness = TestHarness::new().await;
    let uri = harness.open_fixture(FixtureKind::WithAliases).await;

    tokio::task::yield_now().await;

    harness.assert_no_diagnostics(&uri).await;
}

#[tokio::test]
async fn alias_fixture_resolves_aliased_symbol() {
    let harness = TestHarness::new().await;
    let content = load_fixture("module_with_aliases.masm");
    let uri = harness.open_inline("aliases.masm", &content).await;

    tokio::task::yield_now().await;

    // Should resolve "plus" to its aliased target
    let result = harness.goto_definition_at(&uri, &content, "plus").await;
    assert!(result.is_some(), "expected to resolve aliased symbol 'plus'");
}

#[tokio::test]
async fn workspace_symbols_finds_procs_in_examples() {
    let harness = TestHarness::new().await;

    // Open multi_proc fixture which has named procedures
    let content = load_fixture("multi_proc.masm");
    let _uri = harness.open_inline("multi.masm", &content).await;

    tokio::task::yield_now().await;

    // Should find helper, compute, main_logic
    harness.assert_workspace_symbol_exists("helper").await;
    harness.assert_workspace_symbol_exists("compute").await;
    harness.assert_workspace_symbol_exists("main_logic").await;
}

#[tokio::test]
async fn multiple_files_workspace_symbols() {
    let harness = TestHarness::new().await;

    // Open multiple files
    let _uri1 = harness
        .open_inline(
            "file1.masm",
            r#"proc alpha
    nop
end
"#,
        )
        .await;
    let _uri2 = harness
        .open_inline(
            "file2.masm",
            r#"proc beta
    nop
end
"#,
        )
        .await;

    tokio::task::yield_now().await;

    // Should find symbols from both files
    harness.assert_workspace_symbol_exists("alpha").await;
    harness.assert_workspace_symbol_exists("beta").await;
}

#[tokio::test]
async fn large_file_does_not_timeout() {
    let harness = TestHarness::new().await;

    // Generate a large file with many procedures
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!(
            "proc proc_{}\n    push.{}\nend\n\n",
            i, i
        ));
    }
    content.push_str("begin\n");
    for i in 0..100 {
        content.push_str(&format!("    exec.proc_{}\n", i));
    }
    content.push_str("end\n");

    let uri = harness.open_inline("large.masm", &content).await;

    tokio::task::yield_now().await;

    // Should complete without timeout
    harness.assert_no_diagnostics(&uri).await;

    // Should be able to find symbols
    let symbols = harness.workspace_symbols("proc_").await;
    assert!(
        symbols.len() >= 50,
        "expected many symbols, got {}",
        symbols.len()
    );
}
