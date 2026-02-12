//! Integration tests for hover rendering.

mod common;

use common::fixtures::find_position;
use common::harness::TestHarness;
use tower_lsp::lsp_types::{HoverContents, TextDocumentContentChangeEvent};

#[tokio::test]
async fn hover_shows_inferred_input_types_in_header() {
    let harness = TestHarness::new().await;
    let content = r#"proc needs_bool
    if.true
        nop
    else
        nop
    end
end

proc caller
    push.1.1
    eq
    exec.needs_bool
end
"#;
    let uri = harness.open_inline("hover_types_input.masm", content).await;

    tokio::task::yield_now().await;

    let mut position = find_position(content, "exec.needs_bool");
    position.character += "exec.".len() as u32;

    let hover = harness
        .hover(&uri, position)
        .await
        .expect("expected hover content");

    let markdown = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected markdown hover, got: {other:?}"),
    };

    assert!(
        markdown.contains("proc needs_bool(v_0: Bool)"),
        "expected typed hover header, got:\n{}",
        markdown
    );
}

#[tokio::test]
async fn hover_shows_inferred_output_types_in_header() {
    let harness = TestHarness::new().await;
    let content = r#"proc bool_result
    push.1.1
    eq
end

proc caller
    exec.bool_result
end
"#;
    let uri = harness
        .open_inline("hover_types_output.masm", content)
        .await;

    tokio::task::yield_now().await;

    let mut position = find_position(content, "exec.bool_result");
    position.character += "exec.".len() as u32;

    let hover = harness
        .hover(&uri, position)
        .await
        .expect("expected hover content");

    let markdown = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected markdown hover, got: {other:?}"),
    };

    assert!(
        markdown.contains("proc bool_result() -> Bool"),
        "expected typed return in hover header, got:\n{}",
        markdown
    );
}

#[tokio::test]
async fn hover_signature_cache_is_invalidated_on_edit() {
    let harness = TestHarness::new().await;
    let initial = r#"proc callee
    if.true
        nop
    else
        nop
    end
end

proc caller
    push.1.1
    eq
    exec.callee
end
"#;
    let uri = harness
        .open_inline("hover_cache_invalidation.masm", initial)
        .await;

    tokio::task::yield_now().await;

    let mut position = find_position(initial, "exec.callee");
    position.character += "exec.".len() as u32;

    // Warm the cache.
    let before = harness
        .hover(&uri, position)
        .await
        .expect("expected hover before edit");
    let before_markdown = match before.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected markdown hover, got: {other:?}"),
    };
    assert!(
        before_markdown.contains("proc callee(v_0: Bool)"),
        "expected Bool input before edit, got:\n{}",
        before_markdown
    );

    let edited = r#"proc callee
    push.1
    add
    drop
end

proc caller
    push.1.1
    eq
    exec.callee
end
"#;
    harness
        .backend
        .handle_change(
            uri.clone(),
            2,
            vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: edited.to_string(),
            }],
        )
        .await;

    tokio::task::yield_now().await;

    let mut edited_position = find_position(edited, "exec.callee");
    edited_position.character += "exec.".len() as u32;

    let after = harness
        .hover(&uri, edited_position)
        .await
        .expect("expected hover after edit");
    let after_markdown = match after.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected markdown hover, got: {other:?}"),
    };
    assert!(
        after_markdown.contains("proc callee(v_0: Felt)"),
        "expected Felt input after edit, got:\n{}",
        after_markdown
    );
}

#[tokio::test]
async fn hover_shows_constant_definition_and_doc_comment() {
    let harness = TestHarness::new().await;
    let content = r#"#! Global constant docs
const FOO = 42

proc main
    push.FOO
end
"#;
    let uri = harness
        .open_inline("hover_constant_definition.masm", content)
        .await;

    tokio::task::yield_now().await;

    let mut position = find_position(content, "push.FOO");
    position.character += "push.".len() as u32;
    let hover = harness
        .hover(&uri, position)
        .await
        .expect("expected hover content");

    let markdown = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected markdown hover, got: {other:?}"),
    };

    assert!(
        markdown.contains("const FOO = 42"),
        "expected constant definition in hover, got:\n{}",
        markdown
    );
    assert!(
        markdown.contains("Global constant docs"),
        "expected doc comment in hover, got:\n{}",
        markdown
    );
}
