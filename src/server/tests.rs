use super::*;
use crate::LibraryPath;
use crate::client::PublishDiagnostics;
use crate::core_lib::{
    DEFAULT_CORE_LIBRARY_PREFIX, core_library_root_from_repo_root, default_core_library_path,
    default_core_library_symbol_path,
};
use crate::util::to_miden_uri;
use miden_debug_types::SourceManager;
use serde_json::json;
use std::path::Path;
use std::sync::Arc;
use tower_lsp::LanguageServer;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::{
    Diagnostic, ExecuteCommandParams, Position, TextDocumentContentChangeEvent, Url,
    VersionedTextDocumentIdentifier,
};

fn unique_temp_dir(name: &str) -> std::path::PathBuf {
    let stamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("time should move forward")
        .as_nanos();
    std::env::temp_dir().join(format!(
        "masm-lsp-server-{name}-{}-{stamp}",
        std::process::id()
    ))
}

fn copy_dir_recursive(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("create destination directory");
    let entries = std::fs::read_dir(src).expect("read source directory");
    for entry in entries {
        let entry = entry.expect("read directory entry");
        let path = entry.path();
        let target = dst.join(entry.file_name());
        if entry
            .file_type()
            .expect("read directory entry type")
            .is_dir()
        {
            copy_dir_recursive(&path, &target);
        } else {
            if let Some(parent) = target.parent() {
                std::fs::create_dir_all(parent).expect("create destination parent");
            }
            std::fs::copy(&path, &target).expect("copy fixture file");
        }
    }
}

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
    let _ = backend.publish_diagnostics(uri.clone()).await.len();

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
async fn execute_command_sets_core_library_root() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let base = std::env::temp_dir().join(format!(
        "masm-lsp-set-core-lib-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos()
    ));
    let repo = base.join("miden-vm");
    let asm = core_library_root_from_repo_root(&repo);
    std::fs::create_dir_all(&asm).expect("create core library asm");
    let expected = asm
        .canonicalize()
        .expect("canonicalize expected core library path");

    let params = ExecuteCommandParams {
        command: "masm-lsp.setCorePath".to_string(),
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
    assert_eq!(cfg.library_paths[0].prefix, DEFAULT_CORE_LIBRARY_PREFIX);
    assert_eq!(cfg.library_paths[0].root, expected);

    let _ = std::fs::remove_dir_all(base);
}

#[tokio::test]
async fn execute_command_set_core_library_root_requires_argument() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let params = ExecuteCommandParams {
        command: "masm-lsp.setCorePath".to_string(),
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
            "use miden::core::math::u64\nuse miden::core::crypto::hashes\n\nproc foo\n  push.1\nend\n\nproc bar\n  push.2\nend\n".to_string(),
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
            json!("use miden::core::math::u64"),
            json!("use miden::core::crypto::hashes"),
        ]
    );
}

#[tokio::test]
async fn execute_command_group_advice_diagnostics_by_origin_returns_grouped_payload() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/group_advice_success.masm").expect("valid URI");

    backend
        .handle_open(
            uri.clone(),
            1,
            "@locals(1)\nproc bad\n    adv_push\n    loc_store.0\n    loc_load.0\n    push.1\n    u32wrapping_add\n    drop\n    loc_load.0\n    push.1\n    u32wrapping_add\nend\n".to_string(),
        )
        .await
        .expect("open");

    let params = ExecuteCommandParams {
        command: "masm-lsp.groupAdviceDiagnosticsByOrigin".to_string(),
        arguments: vec![json!({
            "uri": uri.as_str(),
        })],
        work_done_progress_params: Default::default(),
    };

    let result = backend
        .execute_command(params)
        .await
        .expect("execute command")
        .expect("grouped analysis payload");

    let groups = result
        .get("groups")
        .and_then(serde_json::Value::as_array)
        .expect("groups array");
    assert_eq!(groups.len(), 1, "expected one origin group");

    let group = groups[0].as_object().expect("group object");
    assert_eq!(
        group
            .get("sinkCount")
            .and_then(serde_json::Value::as_u64)
            .expect("sink count"),
        2
    );
    assert!(
        group
            .get("message")
            .and_then(serde_json::Value::as_str)
            .expect("group message")
            .contains("downstream sink"),
        "expected grouped message to summarize downstream sinks"
    );
    let procedures = group
        .get("procedures")
        .and_then(serde_json::Value::as_array)
        .expect("procedures array");
    assert_eq!(procedures.len(), 1);
    assert!(
        procedures[0]
            .as_str()
            .expect("procedure name")
            .ends_with("::bad"),
        "expected grouped payload to retain procedure names: {procedures:?}"
    );
    let sinks = group
        .get("sinks")
        .and_then(serde_json::Value::as_array)
        .expect("sinks array");
    assert_eq!(sinks.len(), 2);
}

#[tokio::test]
async fn execute_command_group_advice_diagnostics_by_origin_rejects_extra_arguments() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let params = ExecuteCommandParams {
        command: "masm-lsp.groupAdviceDiagnosticsByOrigin".to_string(),
        arguments: vec![json!({ "uri": "file:///tmp/one.masm" }), json!({})],
        work_done_progress_params: Default::default(),
    };

    let result = backend.execute_command(params).await;
    assert!(result.is_err(), "expected invalid params error");
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
            .is_some_and(|msg| msg.contains("could not decompile procedure `foo`")),
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
    assert!(failures.iter().any(|entry| {
        entry
            .get("message")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|msg| msg.contains("could not decompile procedure `foo`"))
    }));
    assert!(failures.iter().any(|entry| {
        entry
            .get("message")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|msg| msg.contains("could not decompile procedure `bar`"))
    }));

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
            .contains("could not decompile procedure `foo`"),
        "unexpected diagnostic message: {}",
        diagnostic.message
    );
    assert!(
        diagnostic
            .message
            .contains("unsupported instruction `dynexec` found")
            || diagnostic.message.contains("unknown inferred signature"),
        "expected underlying decompilation error in diagnostic message, got: {}",
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
async fn publish_diagnostics_includes_decompilation_failures_when_hints_are_enabled() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/decompile_failure.masm").expect("valid URI");

    let mut cfg = backend.snapshot_config().await;
    cfg.inlay_hint_type = crate::InlayHintType::Decompilation;
    cfg.taint_analysis_enabled = false;
    backend.update_config(cfg).await;

    backend
        .handle_open(uri.clone(), 1, "proc foo\n    dynexec\nend\n".to_string())
        .await
        .expect("open");

    let published = client.take_published().await;
    let diagnostics: Vec<_> = published
        .iter()
        .filter(|(published_uri, _, _)| published_uri == &uri)
        .flat_map(|(_, diags, _)| diags.iter())
        .collect();

    assert!(
        diagnostics.iter().any(|diag| {
            diag.source.as_deref() == Some(crate::masm::diagnostics::SOURCE_DECOMPILATION)
                && diag.message.contains("could not decompile procedure `foo`")
                && (diag
                    .message
                    .contains("unsupported instruction `dynexec` found")
                    || diag.message.contains("unknown inferred signature"))
        }),
        "expected detailed decompilation diagnostic for foo, got: {:?}",
        diagnostics
            .iter()
            .map(|diag| (diag.source.clone(), diag.message.clone()))
            .collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn pipe_words_to_memory_still_has_an_inferred_signature() {
    let client = RecordingClient::default();
    let backend = Backend::new(client);
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let core_root = repo_root.join("examples").join("core");
    let mem_path = core_root.join("mem.masm");
    let mem_uri = Url::from_file_path(&mem_path).expect("mem URI");
    let mem_text = std::fs::read_to_string(&mem_path).expect("read core library mem.masm");

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![default_core_library_path(core_root)];
    cfg.taint_analysis_enabled = true;
    backend.update_config(cfg).await;

    backend
        .handle_open(mem_uri.clone(), 1, mem_text)
        .await
        .expect("open core library mem.masm");

    let signature = backend
        .inferred_signature_line(
            backend
                .snapshot_document_symbols(&mem_uri)
                .await
                .expect("mem symbols")
                .module
                .as_ref(),
            &mem_uri,
            &default_core_library_symbol_path("mem::pipe_words_to_memory"),
            &[default_core_library_path(
                mem_path
                    .parent()
                    .expect("core library module parent")
                    .to_path_buf(),
            )],
        )
        .await;

    assert!(
        signature.is_some(),
        "expected inferred signature for pipe_words_to_memory"
    );
}

#[tokio::test]
async fn reloading_libraries_removes_old_core_library_entries() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let base = std::env::temp_dir().join(format!(
        "masm-lsp-reload-core-lib-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos()
    ));
    let old_root = base
        .join("old")
        .join("crates")
        .join("lib")
        .join("core")
        .join("asm");
    let new_root = base
        .join("new")
        .join("crates")
        .join("lib")
        .join("core")
        .join("asm");
    std::fs::create_dir_all(&old_root).expect("create old core library root");
    std::fs::create_dir_all(&new_root).expect("create new core library root");
    std::fs::write(old_root.join("old.masm"), "proc old_proc\n  nop\nend\n")
        .expect("write old core library file");
    std::fs::write(new_root.join("new.masm"), "proc new_proc\n  nop\nend\n")
        .expect("write new core library file");

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![default_core_library_path(old_root.clone())];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition("::miden::core::old::old_proc").is_some());
    drop(ws);

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![default_core_library_path(new_root.clone())];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    let ws = backend.snapshot_workspace().await;
    assert!(
        ws.definition("::miden::core::old::old_proc").is_none(),
        "old core library definition should be cleared after root switch"
    );
    assert!(ws.definition("::miden::core::new::new_proc").is_some());

    let _ = std::fs::remove_dir_all(base);
}

#[tokio::test]
async fn handle_close_restores_disk_backed_symbols() {
    let client = RecordingClient::default();
    let backend = Backend::new(client);

    let root = unique_temp_dir("close-restores-disk");
    std::fs::create_dir_all(&root).expect("create root");
    let path = root.join("utils.masm");
    std::fs::write(&path, "proc old_proc\n  nop\nend\n").expect("write disk file");
    let uri = Url::from_file_path(&path).expect("path URI");

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![LibraryPath {
        root: root.clone(),
        prefix: String::new(),
    }];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("old_proc").is_some());
    drop(ws);

    backend
        .handle_open(uri.clone(), 1, "proc new_proc\n  nop\nend\n".to_string())
        .await
        .expect("open override");

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("new_proc").is_some());
    assert!(ws.definition_by_name("old_proc").is_none());
    drop(ws);

    backend.handle_close(uri.clone()).await;

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("old_proc").is_some());
    assert!(ws.definition_by_name("new_proc").is_none());

    let symbols = backend
        .snapshot_document_symbols(&uri)
        .await
        .expect("restored symbols");
    let proc_name = symbols
        .module
        .procedures()
        .next()
        .expect("procedure")
        .name()
        .as_str()
        .to_string();
    assert_eq!(proc_name, "old_proc");

    let _ = std::fs::remove_dir_all(root);
}

#[tokio::test]
async fn library_reload_does_not_clobber_open_buffer_same_uri() {
    let client = RecordingClient::default();
    let backend = Backend::new(client);

    let root = unique_temp_dir("reload-open-buffer");
    std::fs::create_dir_all(&root).expect("create root");
    let path = root.join("utils.masm");
    std::fs::write(&path, "proc old_proc\n  nop\nend\n").expect("write disk file");
    let uri = Url::from_file_path(&path).expect("path URI");

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![LibraryPath {
        root: root.clone(),
        prefix: String::new(),
    }];
    backend.update_config(cfg).await;
    backend.load_configured_libraries().await;

    backend
        .handle_open(uri.clone(), 1, "proc new_proc\n  nop\nend\n".to_string())
        .await
        .expect("open override");

    backend.load_configured_libraries().await;

    let source = backend
        .sources
        .get_by_uri(&to_miden_uri(&uri))
        .expect("source present");
    assert!(source.as_str().contains("new_proc"));
    assert!(!source.as_str().contains("old_proc"));

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("new_proc").is_some());
    assert!(ws.definition_by_name("old_proc").is_none());

    let _ = std::fs::remove_dir_all(root);
}

#[tokio::test]
async fn parse_error_preserves_last_known_good_open_state() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/parse_error_preserves_state.masm").expect("valid URI");

    backend
        .handle_open(uri.clone(), 1, "proc foo\n  nop\nend\n".to_string())
        .await
        .expect("open");
    let _ = client.take_published().await;

    backend
        .handle_change(
            uri.clone(),
            2,
            vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "proc foo\n  nop\n".to_string(),
            }],
        )
        .await;

    let published = client.take_published().await;
    assert!(
        published.iter().any(|(_, diags, _)| !diags.is_empty()),
        "expected parse diagnostics after invalid edit"
    );

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("foo").is_some());

    let symbols = backend
        .snapshot_document_symbols(&uri)
        .await
        .expect("cached symbols");
    let proc_name = symbols
        .module
        .procedures()
        .next()
        .expect("procedure")
        .name()
        .as_str()
        .to_string();
    assert_eq!(proc_name, "foo");

    let active = backend
        .tracked_workspace
        .read()
        .await
        .active_program_for_uri(&uri)
        .expect("active tracked program");
    assert_eq!(
        active.origin,
        super::tracked_workspace::ProgramOrigin::OpenDocument
    );
}

#[tokio::test]
async fn parse_error_without_last_known_good_removes_active_open_state() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/parse_error_without_last_good.masm").expect("valid URI");

    backend
        .handle_open(uri.clone(), 1, "proc\n".to_string())
        .await
        .expect("open invalid");

    let published = client.take_published().await;
    assert!(
        published.iter().any(|(_, diags, _)| !diags.is_empty()),
        "expected diagnostics for invalid initial open"
    );

    let ws = backend.snapshot_workspace().await;
    assert!(ws.definition_by_name("proc").is_none());
    assert!(backend.snapshot_document_symbols(&uri).await.is_none());
    assert!(
        backend
            .tracked_workspace
            .read()
            .await
            .active_program_for_uri(&uri)
            .is_none()
    );
}

#[tokio::test]
async fn workspace_folder_noise_does_not_publish_analysis_warning_for_core_library_inlay_hints() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());

    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = unique_temp_dir("workspace-folder-noise");
    let core_root = workspace_root
        .join("crates")
        .join("lib")
        .join("core")
        .join("asm");
    let mem_fixture = repo_root.join("examples").join("core").join("mem.masm");
    let poseidon2_fixture = repo_root
        .join("examples")
        .join("core")
        .join("crypto")
        .join("hashes")
        .join("poseidon2.masm");

    std::fs::create_dir_all(core_root.join("crypto").join("hashes"))
        .expect("create core library fixture directories");
    std::fs::copy(&mem_fixture, core_root.join("mem.masm")).expect("copy mem fixture");
    std::fs::copy(
        &poseidon2_fixture,
        core_root
            .join("crypto")
            .join("hashes")
            .join("poseidon2.masm"),
    )
    .expect("copy poseidon2 fixture");
    copy_dir_recursive(
        &repo_root
            .join("tests")
            .join("fixtures")
            .join("workspace_noise"),
        &workspace_root,
    );

    let mut cfg = backend.snapshot_config().await;
    cfg.library_paths = vec![default_core_library_path(core_root.clone())];
    cfg.inlay_hint_type = crate::InlayHintType::Decompilation;
    backend.update_config(cfg).await;

    backend
        .initialize(lsp_types::InitializeParams {
            workspace_folders: Some(vec![lsp_types::WorkspaceFolder {
                uri: Url::from_file_path(&workspace_root).expect("workspace URI"),
                name: "miden-vm".to_string(),
            }]),
            ..Default::default()
        })
        .await
        .expect("initialize");
    backend.initialized(lsp_types::InitializedParams {}).await;

    let mem_path = core_root.join("mem.masm");
    let mem_uri = Url::from_file_path(&mem_path).expect("mem URI");
    let mem_text = std::fs::read_to_string(&mem_path).expect("read core library mem.masm");

    backend
        .did_open(lsp_types::DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: mem_uri.clone(),
                language_id: "masm".to_string(),
                version: 1,
                text: mem_text.clone(),
            },
        })
        .await;

    let open_published = client.take_published().await;
    let open_mem_diags: Vec<_> = open_published
        .iter()
        .filter(|(uri, _, _)| uri == &mem_uri)
        .flat_map(|(_, diags, _)| diags.iter())
        .collect();
    assert!(
        open_mem_diags.iter().all(|diag| {
            diag.source.as_deref() != Some(crate::masm::diagnostics::SOURCE_ANALYSIS)
                && !diag
                    .message
                    .contains("unresolved transitive module dependencies")
        }),
        "expected core library mem.masm open diagnostics to be free of cross-workspace analysis warnings, got: {:?}",
        open_mem_diags
            .iter()
            .map(|diag| (diag.source.clone(), diag.message.clone()))
            .collect::<Vec<_>>()
    );

    let doc = backend
        .snapshot_document_symbols(&mem_uri)
        .await
        .expect("mem symbols");
    assert!(
        doc.module.path().to_string().ends_with("miden::core::mem"),
        "expected core library module path, got {}",
        doc.module.path()
    );

    let last_line_index = mem_text.lines().count().saturating_sub(1) as u32;
    let last_line_len = mem_text.lines().last().map(|line| line.len()).unwrap_or(0) as u32;
    let _ = tokio::time::timeout(
        std::time::Duration::from_secs(15),
        backend.inlay_hint(lsp_types::InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier {
                uri: mem_uri.clone(),
            },
            range: lsp_types::Range::new(
                Position::new(0, 0),
                Position::new(last_line_index, last_line_len),
            ),
            work_done_progress_params: Default::default(),
        }),
    )
    .await
    .expect("inlay hint request timed out")
    .expect("inlay hints request");

    let published = client.take_published().await;
    assert!(
        published.is_empty(),
        "inlayHint should not publish diagnostics, got: {:?}",
        published
    );

    let _ = std::fs::remove_dir_all(workspace_root);
}

#[tokio::test]
async fn inlay_hint_request_does_not_publish_diagnostics() {
    let client = RecordingClient::default();
    let backend = Backend::new(client.clone());
    let uri = Url::parse("file:///tmp/inlay_hint_no_publish.masm").expect("valid URI");
    let text = "proc foo\n  push.1\nend\n".to_string();

    let mut cfg = backend.snapshot_config().await;
    cfg.inlay_hint_type = crate::InlayHintType::Decompilation;
    cfg.taint_analysis_enabled = false;
    backend.update_config(cfg).await;

    backend
        .did_open(lsp_types::DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: uri.clone(),
                language_id: "masm".to_string(),
                version: 1,
                text,
            },
        })
        .await;
    let _ = client.take_published().await;

    let _ = backend
        .inlay_hint(lsp_types::InlayHintParams {
            text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
            range: lsp_types::Range::new(Position::new(0, 0), Position::new(2, 3)),
            work_done_progress_params: Default::default(),
        })
        .await
        .expect("inlay hint request");

    let published = client.take_published().await;
    assert!(
        published.is_empty(),
        "inlayHint should not publish diagnostics, got: {:?}",
        published
    );
}

fn command_error_diagnostic(error: &tower_lsp::jsonrpc::Error) -> Option<Diagnostic> {
    let diagnostic = error.data.as_ref()?.get("diagnostic")?;
    serde_json::from_value(diagnostic.clone()).ok()
}

#[derive(Clone, Default)]
struct RecordingClient {
    published: Arc<tokio::sync::Mutex<PublishedDiagnostics>>,
}

impl RecordingClient {
    async fn take_published(&self) -> PublishedDiagnostics {
        let mut guard = self.published.lock().await;
        guard.drain(..).collect()
    }
}

type PublishedDiagnostic = (Url, Vec<Diagnostic>, Option<i32>);
type PublishedDiagnostics = Vec<PublishedDiagnostic>;

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
