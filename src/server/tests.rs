use super::*;
use crate::client::PublishDiagnostics;
use crate::LibraryPath;
use serde_json::json;
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
async fn execute_command_decompile_procedure_at_cursor_returns_output() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_success.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "proc foo\n  push.1\n  push.2\n  add\nend\n".to_string(),
        )
        .await
        .expect("open");

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileProcedureAtCursor".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
            "position": {
                "line": 2,
                "character": 2,
            }
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("decompilation payload");

    let procedure = result
        .get("procedure")
        .and_then(serde_json::Value::as_object)
        .expect("procedure object");
    assert_eq!(
        procedure.get("name").and_then(serde_json::Value::as_str),
        Some("foo"),
    );
    let decompiled = result
        .get("decompiled")
        .and_then(serde_json::Value::as_str)
        .expect("decompiled text");
    assert!(
        !decompiled.trim().is_empty(),
        "expected non-empty decompiled output"
    );
}

#[tokio::test]
async fn execute_command_decompile_file_returns_output() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_file_success.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "use std::math::u64\nuse std::crypto::hashes\n\nproc foo\n  push.1\nend\n\nproc bar\n  push.2\nend\n".to_string(),
        )
        .await
        .expect("open");

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileFile".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("decompilation payload");

    let procedures = result
        .get("procedures")
        .and_then(serde_json::Value::as_array)
        .expect("procedures array");
    assert_eq!(
        procedures.len(),
        2,
        "expected both procedures to be decompiled"
    );

    let first = procedures[0].as_object().expect("first procedure object");
    assert_eq!(
        first.get("name").and_then(serde_json::Value::as_str),
        Some("foo"),
    );
    assert!(
        first
            .get("decompiled")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|text| !text.trim().is_empty()),
        "expected non-empty decompiled procedure output"
    );
    let uses = result
        .get("useStatements")
        .and_then(serde_json::Value::as_array)
        .expect("use statements array");
    assert_eq!(
        uses,
        &vec![
            json!("use std::math::u64"),
            json!("use std::crypto::hashes"),
        ]
    );
}

#[tokio::test]
async fn execute_command_decompile_file_requires_uri_argument() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileFile".to_string(),
        arguments: vec![],
        work_done_progress_params: Default::default(),
    };

    let result = backend.execute_command(params).await;
    assert!(result.is_err(), "expected invalid params error");
}

#[tokio::test]
async fn execute_command_decompile_file_returns_failure_payload() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_file_failure.masm").expect("valid URI");

    backend
        .handle_open(uri.clone(), 1, "proc foo\n  dynexec\nend\n".to_string())
        .await
        .expect("open");

    let _ = client.take_published().await;

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileFile".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("decompilation payload");

    assert_eq!(
        result.get("status").and_then(serde_json::Value::as_str),
        Some("failure")
    );
    let summary = result
        .get("summary")
        .and_then(serde_json::Value::as_object)
        .expect("summary object");
    assert_eq!(
        summary
            .get("totalProcedures")
            .and_then(serde_json::Value::as_u64),
        Some(1)
    );
    assert!(
        result
            .get("failures")
            .and_then(serde_json::Value::as_array)
            .is_some_and(|failures| failures.len() == 1),
        "expected single failed procedure, got: {result:?}"
    );
    let failure = result
        .get("failures")
        .and_then(serde_json::Value::as_array)
        .and_then(|arr| arr.first())
        .and_then(serde_json::Value::as_object)
        .expect("first failure object");
    assert!(
        failure
            .get("message")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|msg| msg.contains("Could not decompile procedure `foo`")),
        "unexpected failure payload: {failure:?}"
    );

    let published = client.take_published().await;
    assert!(
        published.is_empty(),
        "decompileFile should not publish diagnostics, got: {:?}",
        published
    );
}

#[tokio::test]
async fn execute_command_decompile_file_returns_partial_payload() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_file_partial.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "proc ok\n  push.1\nend\n\nproc broken\n  dynexec\nend\n".to_string(),
        )
        .await
        .expect("open");

    let _ = client.take_published().await;

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileFile".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("decompilation payload");

    assert_eq!(
        result.get("status").and_then(serde_json::Value::as_str),
        Some("partial")
    );
    let summary = result
        .get("summary")
        .and_then(serde_json::Value::as_object)
        .expect("summary object");
    assert_eq!(
        summary
            .get("decompiledProcedures")
            .and_then(serde_json::Value::as_u64),
        Some(1)
    );
    assert_eq!(
        summary
            .get("failedProcedures")
            .and_then(serde_json::Value::as_u64),
        Some(1)
    );

    let published = client.take_published().await;
    assert!(
        published.is_empty(),
        "decompileFile should not publish diagnostics, got: {:?}",
        published
    );
}

#[tokio::test]
async fn execute_command_decompile_file_includes_all_failures_in_payload() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_file_multi_failure.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "proc foo\n  dynexec\nend\n\nproc bar\n  dynexec\nend\n".to_string(),
        )
        .await
        .expect("open");

    let _ = client.take_published().await;

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileFile".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("decompilation payload");

    assert_eq!(
        result.get("status").and_then(serde_json::Value::as_str),
        Some("failure")
    );
    let failures = result
        .get("failures")
        .and_then(serde_json::Value::as_array)
        .expect("failures array");
    assert_eq!(
        failures.len(),
        2,
        "expected one failure entry per failed procedure"
    );
    assert!(failures.iter().any(|entry| entry
        .get("message")
        .and_then(serde_json::Value::as_str)
        .is_some_and(|msg| msg.contains("Could not decompile procedure `foo`"))));
    assert!(failures.iter().any(|entry| entry
        .get("message")
        .and_then(serde_json::Value::as_str)
        .is_some_and(|msg| msg.contains("Could not decompile procedure `bar`"))));

    let published = client.take_published().await;
    assert!(
        published.is_empty(),
        "decompileFile should not publish diagnostics, got: {:?}",
        published
    );
}

#[tokio::test]
async fn execute_command_decompile_procedure_at_cursor_errors_when_outside_procedure() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_outside_proc.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "const FOO = 42\n\nproc foo\n  push.FOO\nend\n".to_string(),
        )
        .await
        .expect("open");

    let _ = client.take_published().await;

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileProcedureAtCursor".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
            "position": {
                "line": 0,
                "character": 1,
            }
        })],
        work_done_progress_params: Default::default(),
    };

    let err = backend
        .execute_command(params)
        .await
        .expect_err("expected error outside procedure");
    assert_eq!(err.code, tower_lsp::jsonrpc::ErrorCode::InvalidRequest);

    let diagnostic = command_error_diagnostic(&err).expect("diagnostic in error data");
    assert_eq!(
        diagnostic.source.as_deref(),
        Some("masm-lsp/decompilation"),
        "unexpected diagnostic source: {:?}",
        diagnostic.source
    );
    assert!(
        diagnostic
            .message
            .contains("Cursor is not inside a procedure"),
        "unexpected diagnostic message: {}",
        diagnostic.message
    );

    let published = client.take_published().await;
    assert!(
        published.iter().any(|(_, diags, _)| diags
            .iter()
            .any(|diag| diag.source.as_deref() == Some("masm-lsp/decompilation"))),
        "expected decompilation diagnostic to be published, got: {:?}",
        published
    );
}

#[tokio::test]
async fn execute_command_decompile_procedure_at_cursor_returns_decompilation_error() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_failure.masm").expect("valid URI");

    backend
        .handle_open(uri.clone(), 1, "proc foo\n  dynexec\nend\n".to_string())
        .await
        .expect("open");

    let _ = client.take_published().await;

    let params = ExecuteCommandParams {
        command: "masm-lsp.decompileProcedureAtCursor".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
            "position": {
                "line": 1,
                "character": 3,
            }
        })],
        work_done_progress_params: Default::default(),
    };

    let err = backend
        .execute_command(params)
        .await
        .expect_err("expected decompilation failure");
    assert_eq!(err.code, tower_lsp::jsonrpc::ErrorCode::InternalError);

    let diagnostic = command_error_diagnostic(&err).expect("diagnostic in error data");
    assert_eq!(diagnostic.source.as_deref(), Some("masm-lsp/decompilation"));
    assert_eq!(
        diagnostic.severity,
        Some(tower_lsp::lsp_types::DiagnosticSeverity::WARNING)
    );
    assert!(
        diagnostic
            .message
            .contains("Could not decompile procedure `foo`"),
        "unexpected diagnostic message: {}",
        diagnostic.message
    );

    let published = client.take_published().await;
    assert!(
        published.iter().any(|(_, diags, _)| diags
            .iter()
            .any(|diag| diag.source.as_deref() == Some("masm-lsp/decompilation"))),
        "expected decompilation diagnostic to be published, got: {:?}",
        published
    );
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

fn command_error_diagnostic(error: &tower_lsp::jsonrpc::Error) -> Option<Diagnostic> {
    let diagnostic = error.data.as_ref()?.get("diagnostic")?;
    serde_json::from_value(diagnostic.clone()).ok()
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
