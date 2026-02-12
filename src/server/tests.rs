use super::*;
use crate::client::PublishDiagnostics;
use crate::LibraryPath;
use std::sync::Arc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::{
    Diagnostic, ExecuteCommandParams, Position, TextDocumentContentChangeEvent, Url,
    VersionedTextDocumentIdentifier,
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

#[tokio::test]
async fn execute_command_sets_stdlib_root() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let base = std::env::temp_dir().join(format!(
        "masm-lsp-set-stdlib-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos()
    ));
    let repo = base.join("miden-vm");
    let asm = repo.join("stdlib").join("asm");
    std::fs::create_dir_all(&asm).expect("create stdlib/asm");
    let expected = asm
        .canonicalize()
        .expect("canonicalize expected stdlib/asm path");

    let params = ExecuteCommandParams {
        command: "masm-lsp.setStdlibRoot".to_string(),
        arguments: vec![serde_json::Value::String(
            repo.to_string_lossy().to_string(),
        )],
        work_done_progress_params: Default::default(),
    };

    let _ = backend
        .execute_command(params)
        .await
        .expect("execute command");
    let cfg = backend.snapshot_config().await;
    assert_eq!(cfg.library_paths.len(), 1);
    assert_eq!(cfg.library_paths[0].prefix, "std");
    assert_eq!(cfg.library_paths[0].root, expected);

    let _ = std::fs::remove_dir_all(base);
}

#[tokio::test]
async fn execute_command_set_stdlib_root_requires_argument() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let params = ExecuteCommandParams {
        command: "masm-lsp.setStdlibRoot".to_string(),
        arguments: vec![],
        work_done_progress_params: Default::default(),
    };

    let result = backend.execute_command(params).await;
    assert!(result.is_err(), "expected invalid params error");
}

#[tokio::test]
async fn reloading_libraries_removes_old_stdlib_entries() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let base = std::env::temp_dir().join(format!(
        "masm-lsp-reload-stdlib-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos()
    ));
    let old_root = base.join("old").join("stdlib").join("asm");
    let new_root = base.join("new").join("stdlib").join("asm");
    std::fs::create_dir_all(&old_root).expect("create old stdlib root");
    std::fs::create_dir_all(&new_root).expect("create new stdlib root");
    std::fs::write(old_root.join("old.masm"), "proc old_proc\n  nop\nend\n")
        .expect("write old stdlib file");
    std::fs::write(new_root.join("new.masm"), "proc new_proc\n  nop\nend\n")
        .expect("write new stdlib file");

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![LibraryPath {
        root: old_root.clone(),
        prefix: "std".to_string(),
    }];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition("::std::old::old_proc").is_some());
    drop(ws);

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![LibraryPath {
        root: new_root.clone(),
        prefix: "std".to_string(),
    }];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    let ws = backend.snapshot_workspace().await;
    assert!(
        ws.definition("::std::old::old_proc").is_none(),
        "old stdlib definition should be cleared after root switch"
    );
    assert!(ws.definition("::std::new::new_proc").is_some());

    let _ = std::fs::remove_dir_all(base);
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
