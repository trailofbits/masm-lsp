//! Integration tests for DMASM LSP services.

mod common;

use common::fixtures::find_position;
use common::harness::TestHarness;
use tower_lsp::lsp_types::{Position, Url};

#[tokio::test]
async fn rename_variable_in_dmasm_file() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///test/rename.dmasm").unwrap();
    let text = "proc foo(v_0: Felt) -> Felt {\n  v_1 = v_0 + 1;\n  return v_1;\n}";

    harness.open_dmasm(&uri, text).await;

    // Rename v_0 (parameter at column 9) to "input".
    let edit = harness.rename(&uri, Position::new(0, 9), "input").await;

    let changes = edit
        .expect("should return edit")
        .changes
        .expect("should have changes");
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
    let edit = harness.rename(&uri, Position::new(0, 11), "x").await;

    let changes = edit
        .expect("should return edit")
        .changes
        .expect("should have changes");
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

/// Apply a set of [`TextEdit`]s to a source string, returning the modified text.
///
/// Edits are applied in reverse document order so that earlier positions
/// remain valid while later edits are applied.
fn apply_edits(text: &str, edits: &[tower_lsp::lsp_types::TextEdit]) -> String {
    let mut sorted: Vec<_> = edits.to_vec();
    sorted.sort_by(|a, b| {
        b.range
            .start
            .line
            .cmp(&a.range.start.line)
            .then(b.range.start.character.cmp(&a.range.start.character))
    });

    let lines: Vec<&str> = text.lines().collect();
    let mut result = text.to_string();

    for edit in &sorted {
        let start = line_col_to_offset(&lines, edit.range.start);
        let end = line_col_to_offset(&lines, edit.range.end);
        result.replace_range(start..end, &edit.new_text);
    }

    result
}

/// Convert an LSP [`Position`] (line, character) to a byte offset in `text`.
fn line_col_to_offset(lines: &[&str], pos: Position) -> usize {
    let mut offset = 0;
    for (i, line) in lines.iter().enumerate() {
        if i == pos.line as usize {
            return offset + pos.character as usize;
        }
        offset += line.len() + 1; // +1 for '\n'
    }
    offset
}

#[tokio::test]
async fn rename_then_rename_again() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///test/renamex2.dmasm").unwrap();
    let text = "proc foo(v_0: Felt) -> Felt {\n  v_1 = v_0 + 1;\n  return v_1;\n}";

    // First rename: v_0 → input.
    harness.open_dmasm(&uri, text).await;
    let edit = harness
        .rename(&uri, Position::new(0, 9), "input")
        .await
        .expect("first rename should succeed");
    let file_edits = &edit.changes.as_ref().unwrap()[&uri];
    let text2 = apply_edits(text, file_edits);

    // Re-open with the modified text (simulates the client applying edits).
    harness.open_dmasm(&uri, &text2).await;

    // Second rename: input → x (on a body occurrence).
    let body_pos = find_position(&text2, "input");
    let edit2 = harness
        .rename(&uri, body_pos, "x")
        .await
        .expect("second rename should succeed");
    let file_edits2 = &edit2.changes.as_ref().unwrap()[&uri];

    // "input" appears twice (parameter + body), so both should be renamed.
    assert_eq!(file_edits2.len(), 2);
    for edit in file_edits2 {
        assert_eq!(edit.new_text, "x");
    }
}

#[tokio::test]
async fn rename_body_local_after_previous_rename() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///test/body_local.dmasm").unwrap();
    // Start with already-renamed text (no v_ prefix on body local).
    let text = "proc foo(v_0: Felt) -> Felt {\n  sum = v_0 + 1;\n  return sum;\n}";

    harness.open_dmasm(&uri, text).await;

    let pos = find_position(text, "sum");
    let edit = harness
        .rename(&uri, pos, "result")
        .await
        .expect("rename of non-v_ body local should succeed");
    let file_edits = &edit.changes.as_ref().unwrap()[&uri];

    // "sum" appears twice: assignment + return.
    assert_eq!(file_edits.len(), 2);
    for edit in file_edits {
        assert_eq!(edit.new_text, "result");
    }
}
