//! Integration tests for DMASM LSP services.

mod common;

use common::harness::TestHarness;
use tower_lsp::lsp_types::{Position, Url};

#[tokio::test]
async fn rename_variable_in_dmasm_file() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///test/rename.dmasm").unwrap();
    let text = "proc foo(v_0: Felt) -> Felt {\n  v_1 = v_0 + 1;\n  return v_1;\n}";

    harness.open_dmasm(&uri, text).await;

    // Rename v_0 (parameter at column 9) to "input".
    let edit = harness
        .rename(&uri, Position::new(0, 9), "input")
        .await;

    let changes = edit.expect("should return edit").changes.expect("should have changes");
    let file_edits = &changes[&uri];

    // v_0 appears twice: parameter + one body usage.
    assert_eq!(file_edits.len(), 2);
    for edit in file_edits {
        assert_eq!(edit.new_text, "input");
    }
}

#[tokio::test]
async fn rename_does_not_cross_procedure_boundaries() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///test/boundary.dmasm").unwrap();
    let text = concat!(
        "proc first(v_0: Felt) -> Felt {\n",
        "  return v_0;\n",
        "}\n",
        "\n",
        "proc second(v_0: Felt) -> Felt {\n",
        "  return v_0;\n",
        "}\n",
    );

    harness.open_dmasm(&uri, text).await;

    // Rename v_0 in the FIRST procedure only (parameter at column 11).
    let edit = harness
        .rename(&uri, Position::new(0, 11), "x")
        .await;

    let changes = edit.expect("should return edit").changes.expect("should have changes");
    let file_edits = &changes[&uri];

    // Only 2 edits (parameter + return), not 4 (which would include second proc).
    assert_eq!(file_edits.len(), 2);
    // All edits should be within the first procedure (lines 0-2).
    for edit in file_edits {
        assert!(
            edit.range.start.line <= 2,
            "edit should be in first procedure"
        );
    }
}
