use super::*;
use crate::client::PublishDiagnostics;
use std::sync::Arc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::{
    Diagnostic, Position, TextDocumentContentChangeEvent, Url, VersionedTextDocumentIdentifier,
};
use tower_lsp::LanguageServer;

#[tokio::test]
async fn publish_diagnostics_sends_updated_version() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/test.masm").unwrap();

    // Open document version 1
    backend
        .did_open(lsp_types::DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: uri.clone(),
                language_id: "masm".to_string(),
                version: 1,
                text: "proc foo\nend\n".to_string(),
            },
        })
        .await;

    // Send diagnostics for version 1
    backend
        .publish_diagnostics(uri.clone())
        .await
        .iter()
        .count();

    // Modify document to version 2
    backend
        .did_change(lsp_types::DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "proc foo\n  invalid\nend\n".to_string(),
            }],
        })
        .await;

    // Publish diagnostics for version 2
    backend.publish_diagnostics(uri.clone()).await;

    // Ensure diagnostics were sent with the updated version (2)
    let published = client.take_published().await;
    assert!(
        published.iter().any(|(u, _, v)| u == &uri && *v == Some(2)),
        "expected diagnostics with version 2, got: {:?}",
        published
    );
}

#[tokio::test]
async fn goto_definition_returns_none_for_unknown_symbol() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let uri = Url::parse("file:///tmp/test.masm").unwrap();
    let text = "proc foo\n  nop\nend\n".to_string();

    backend.handle_open(uri.clone(), 1, text).await.unwrap();

    let params = lsp_types::GotoDefinitionParams {
        text_document_position_params: lsp_types::TextDocumentPositionParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 10,
                character: 0,
            },
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = backend.goto_definition(params).await.unwrap();
    assert!(result.is_none(), "expected None for unknown symbol");
}

#[tokio::test]
async fn publishing_diagnostics_handles_parse_error() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/test_parse_error.masm").unwrap();

    // Create a module with an undefined symbol reference
    let text = r#"proc foo
  bar
end
"#
    .to_string();

    backend
        .handle_open(uri.clone(), 1, text)
        .await
        .expect("open");

    // Update with an invalid reference
    let invalid = r#"proc foo
  invalid_symbol
end

begin
    exec.foo
end
"#
    .to_string();

    backend
        .did_change(lsp_types::DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: invalid.clone(),
            }],
        })
        .await;

    let diags = backend.publish_diagnostics(uri.clone()).await;
    assert!(
        !diags.is_empty(),
        "expected diagnostics for undefined symbol"
    );
}

#[tokio::test]
async fn determine_module_kind_detects_executable_from_entrypoint() {
    use miden_assembly_syntax::ast::ModuleKind;

    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let exec_uri = Url::parse("file:///tmp/exec_test.masm").unwrap();
    let exec_text = "begin\n  push.1\nend\n".to_string();
    backend
        .handle_open(exec_uri.clone(), 1, exec_text)
        .await
        .expect("open");

    let exec_doc = backend.snapshot_document_symbols(&exec_uri).await.unwrap();
    assert_eq!(
        helpers::determine_module_kind_from_ast(&exec_doc.module),
        ModuleKind::Executable,
    );

    let lib_uri = Url::parse("file:///tmp/lib_test.masm").unwrap();
    let lib_text = "proc foo\n  nop\nend\n".to_string();
    backend
        .handle_open(lib_uri.clone(), 1, lib_text)
        .await
        .expect("open");

    let lib_doc = backend.snapshot_document_symbols(&lib_uri).await.unwrap();
    assert_eq!(
        helpers::determine_module_kind_from_ast(&lib_doc.module),
        ModuleKind::Library,
    );
}

#[derive(Clone, Default)]
struct RecordingClient {
    published: Arc<tokio::sync::Mutex<Vec<(Url, Vec<Diagnostic>, Option<i32>)>>>,
}

impl RecordingClient {
    async fn take_published(&self) -> Vec<(Url, Vec<Diagnostic>, Option<i32>)> {
        let mut guard = self.published.lock().await;
        guard.drain(..).collect()
    }
}

#[async_trait::async_trait]
impl PublishDiagnostics for RecordingClient {
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diagnostics: Vec<Diagnostic>,
        version: Option<i32>,
    ) {
        let mut guard = self.published.lock().await;
        guard.push((uri, diagnostics, version));
    }
}
