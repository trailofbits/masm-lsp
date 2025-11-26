//! Tests for hover functionality.

mod common;

use common::harness::TestHarness;
use tower_lsp::lsp_types::HoverContents;

/// Test that hovering over exec instruction shows instruction hover, not target proc hover.
#[tokio::test]
async fn hover_exec_shows_instruction_not_proc() {
    let harness = TestHarness::new().await;

    let content = r#"#! This is a helper procedure.
proc helper
    add
end

proc main
    exec.helper
end
"#;

    let uri = harness.open_inline("hover_exec.masm", content).await;
    tokio::task::yield_now().await;

    // Hover over exec.helper should show exec instruction hover
    let hover = harness.hover_at(&uri, content, "exec.helper").await;
    assert!(hover.is_some(), "expected hover for exec.helper");

    let hover = hover.unwrap();
    if let HoverContents::Markup(markup) = hover.contents {
        assert!(
            markup.value.contains("exec"),
            "hover should contain 'exec' instruction, got: {}",
            markup.value
        );
        assert!(
            markup.value.contains("Stack Input"),
            "hover should contain stack input section, got: {}",
            markup.value
        );
        // Should NOT contain the proc's doc comment
        assert!(
            !markup.value.contains("helper procedure"),
            "hover should NOT contain target proc's doc comment, got: {}",
            markup.value
        );
    } else {
        panic!("expected markup hover contents");
    }
}

/// Test that hovering over an instruction shows instruction reference.
#[tokio::test]
async fn hover_instruction_shows_reference() {
    let harness = TestHarness::new().await;

    let content = r#"proc main
    push.1
    push.2
    add
end
"#;

    let uri = harness.open_inline("hover_instr.masm", content).await;
    tokio::task::yield_now().await;

    // Hover over the add instruction
    let hover = harness.hover_at(&uri, content, "add").await;
    assert!(hover.is_some(), "expected hover for add instruction");

    let hover = hover.unwrap();
    if let HoverContents::Markup(markup) = hover.contents {
        // Verify the instruction is in a masm code block
        assert!(
            markup.value.contains("```masm\nadd\n```"),
            "hover should contain instruction in masm code block, got: {}",
            markup.value
        );
        assert!(
            markup.value.contains("**Stack Input**"),
            "hover should contain bold stack input section, got: {}",
            markup.value
        );
        assert!(
            markup.value.contains("**Stack Output**"),
            "hover should contain bold stack output section, got: {}",
            markup.value
        );
        assert!(
            markup.value.contains("**Cycles**"),
            "hover should contain bold cycles section, got: {}",
            markup.value
        );
    } else {
        panic!("expected markup hover contents");
    }
}
