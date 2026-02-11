//! Integration tests for hover rendering.

mod common;

use common::fixtures::find_position;
use common::harness::TestHarness;
use tower_lsp::lsp_types::HoverContents;

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
