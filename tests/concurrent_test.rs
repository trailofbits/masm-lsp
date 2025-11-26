//! Tests for concurrent access safety.
//!
//! These tests verify that concurrent document updates, reads, and workspace
//! operations don't cause data races, deadlocks, or panics.

mod common;

use std::sync::Arc;

use common::harness::{RecordingClient, TestHarness};
use masm_lsp::server::Backend;
use tokio::task::JoinSet;
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams, HoverParams,
    Position, TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams, Url, VersionedTextDocumentIdentifier, WorkspaceSymbolParams,
};
use tower_lsp::LanguageServer;

/// Generate a simple MASM procedure with the given name and version.
fn gen_proc(name: &str, version: u32) -> String {
    format!(
        r#"#! Version {version}
proc {name}
    push.{version}
end
"#
    )
}

/// Generate a module with multiple procedures.
fn gen_module(prefix: &str, proc_count: usize) -> String {
    let mut content = String::new();
    for i in 0..proc_count {
        content.push_str(&format!(
            "proc {prefix}_{i}\n    push.{i}\nend\n\n",
            prefix = prefix,
            i = i
        ));
    }
    content
}

#[tokio::test]
async fn concurrent_document_opens_no_panic() {
    let client = RecordingClient::new();
    let backend = Arc::new(Backend::new(client));

    let mut set = JoinSet::new();

    // Open 20 documents concurrently
    for i in 0..20 {
        let backend = backend.clone();
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/doc{}.masm", i)).unwrap();
            let content = gen_module(&format!("module{}", i), 5);

            let params = DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id: "masm".into(),
                    version: 1,
                    text: content,
                },
            };
            backend.did_open(params).await;
        });
    }

    // All tasks should complete without panic
    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn concurrent_rapid_changes_no_panic() {
    let client = RecordingClient::new();
    let backend = Arc::new(Backend::new(client));

    let uri = Url::parse("file:///tmp/concurrent/rapid.masm").unwrap();

    // First open the document
    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "masm".into(),
            version: 1,
            text: gen_proc("foo", 1),
        },
    };
    backend.did_open(params).await;

    let mut set = JoinSet::new();

    // Send 50 rapid changes concurrently
    for v in 2..52 {
        let backend = backend.clone();
        let uri = uri.clone();
        set.spawn(async move {
            let params = DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri,
                    version: v,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: gen_proc("foo", v as u32),
                }],
            };
            backend.did_change(params).await;
        });
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn concurrent_reads_during_writes_no_panic() {
    let client = RecordingClient::new();
    let backend = Arc::new(Backend::new(client));

    // Open initial documents
    for i in 0..5 {
        let uri = Url::parse(&format!("file:///tmp/concurrent/rw{}.masm", i)).unwrap();
        let content = gen_module(&format!("mod{}", i), 3);
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "masm".into(),
                version: 1,
                text: content,
            },
        };
        backend.did_open(params).await;
    }

    tokio::task::yield_now().await;

    let mut set = JoinSet::new();

    // Spawn concurrent writers
    for i in 0..5 {
        let backend = backend.clone();
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/rw{}.masm", i)).unwrap();
            for v in 2..20 {
                let params = DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: uri.clone(),
                        version: v,
                    },
                    content_changes: vec![TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: gen_module(&format!("mod{}v{}", i, v), 3),
                    }],
                };
                backend.did_change(params).await;
                tokio::task::yield_now().await;
            }
        });
    }

    // Spawn concurrent readers (goto_definition)
    for i in 0..5 {
        let backend = backend.clone();
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/rw{}.masm", i)).unwrap();
            for _ in 0..20 {
                let params = GotoDefinitionParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                        position: Position::new(1, 5),
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                // Ignore the result - we're testing for panics/deadlocks
                let _ = backend.goto_definition(params).await;
                tokio::task::yield_now().await;
            }
        });
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn concurrent_hover_requests_no_panic() {
    let harness = TestHarness::new().await;

    // Open a document with multiple procedures
    let content = r#"proc alpha
    push.1 push.2 add
end

proc beta
    push.3 push.4 mul
end

proc gamma
    exec.alpha exec.beta
end
"#;
    let uri = harness.open_inline("hover_test.masm", content).await;

    tokio::task::yield_now().await;

    let backend = Arc::new(harness.backend);
    let mut set = JoinSet::new();

    // Send many concurrent hover requests at different positions
    let positions = [
        Position::new(0, 5),  // proc alpha
        Position::new(1, 5),  // push.1
        Position::new(4, 5),  // proc beta
        Position::new(5, 5),  // push.3
        Position::new(8, 5),  // proc gamma
        Position::new(9, 10), // exec.alpha
    ];

    for _ in 0..10 {
        for &pos in &positions {
            let backend = backend.clone();
            let uri = uri.clone();
            set.spawn(async move {
                let params = HoverParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri },
                        position: pos,
                    },
                    work_done_progress_params: Default::default(),
                };
                let _ = backend.hover(params).await;
            });
        }
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn concurrent_workspace_symbol_queries_no_panic() {
    let harness = TestHarness::new().await;

    // Open multiple documents to populate the workspace
    for i in 0..5 {
        let content = gen_module(&format!("module{}", i), 10);
        harness
            .open_inline(&format!("ws{}.masm", i), &content)
            .await;
    }

    tokio::task::yield_now().await;

    let backend = Arc::new(harness.backend);
    let mut set = JoinSet::new();

    // Send concurrent workspace symbol queries
    let queries = ["module", "0", "1", "proc", ""];
    for _ in 0..20 {
        for query in &queries {
            let backend = backend.clone();
            let query = query.to_string();
            set.spawn(async move {
                let params = WorkspaceSymbolParams {
                    query,
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                let _ = backend.symbol(params).await;
            });
        }
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn concurrent_open_close_cycles_no_panic() {
    let client = RecordingClient::new();
    let backend = Arc::new(Backend::new(client));

    let mut set = JoinSet::new();

    // Multiple documents going through open/close cycles concurrently
    for i in 0..10 {
        let backend = backend.clone();
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/cycle{}.masm", i)).unwrap();

            for cycle in 0..5 {
                // Open
                let params = DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: uri.clone(),
                        language_id: "masm".into(),
                        version: cycle * 10 + 1,
                        text: gen_proc(&format!("cycle{}", cycle), cycle as u32),
                    },
                };
                backend.did_open(params).await;

                tokio::task::yield_now().await;

                // Close
                let params = tower_lsp::lsp_types::DidCloseTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                };
                backend.did_close(params).await;

                tokio::task::yield_now().await;
            }
        });
    }

    while let Some(result) = set.join_next().await {
        result.expect("task should not panic");
    }
}

#[tokio::test]
async fn mixed_concurrent_operations_no_deadlock() {
    let client = RecordingClient::new();
    let backend = Arc::new(Backend::new(client));

    // Pre-populate with some documents
    for i in 0..3 {
        let uri = Url::parse(&format!("file:///tmp/concurrent/mixed{}.masm", i)).unwrap();
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "masm".into(),
                version: 1,
                text: gen_module(&format!("mixed{}", i), 5),
            },
        };
        backend.did_open(params).await;
    }

    tokio::task::yield_now().await;

    let mut set = JoinSet::new();

    // Spawn mixed operations: opens, changes, hovers, goto_definitions, workspace_symbols
    for i in 0..5 {
        // Clone backend for each task at the start of each iteration
        let backend_writer = backend.clone();
        let backend_hover = backend.clone();
        let backend_goto = backend.clone();
        let backend_symbol = backend.clone();

        // Writer task
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/mixed{}.masm", i % 3)).unwrap();
            for v in 2..10 {
                let params = DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: uri.clone(),
                        version: v,
                    },
                    content_changes: vec![TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: gen_module(&format!("mixed{}v{}", i, v), 5),
                    }],
                };
                backend_writer.did_change(params).await;
                tokio::task::yield_now().await;
            }
        });

        // Hover task
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/mixed{}.masm", i % 3)).unwrap();
            for line in 0..10 {
                let params = HoverParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                        position: Position::new(line, 5),
                    },
                    work_done_progress_params: Default::default(),
                };
                let _ = backend_hover.hover(params).await;
                tokio::task::yield_now().await;
            }
        });

        // Goto definition task
        set.spawn(async move {
            let uri = Url::parse(&format!("file:///tmp/concurrent/mixed{}.masm", i % 3)).unwrap();
            for line in 0..10 {
                let params = GotoDefinitionParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                        position: Position::new(line, 5),
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                let _ = backend_goto.goto_definition(params).await;
                tokio::task::yield_now().await;
            }
        });

        // Workspace symbol task
        set.spawn(async move {
            for _ in 0..10 {
                let params = WorkspaceSymbolParams {
                    query: format!("mixed{}", i % 3),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                let _ = backend_symbol.symbol(params).await;
                tokio::task::yield_now().await;
            }
        });
    }

    // Use timeout to detect deadlocks
    let result = tokio::time::timeout(std::time::Duration::from_secs(30), async {
        while let Some(result) = set.join_next().await {
            result.expect("task should not panic");
        }
    })
    .await;

    assert!(result.is_ok(), "operations timed out - possible deadlock");
}
