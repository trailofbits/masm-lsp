use masm_lsp::{client::PublishDiagnostics, server::Backend};
use tokio::sync::Mutex;
use tower_lsp::{
    lsp_types::{
        request::{GotoImplementationParams, GotoImplementationResponse},
        DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, Location,
        Position, ReferenceContext, ReferenceParams, TextDocumentIdentifier, TextDocumentItem,
        TextDocumentPositionParams, Url, WorkspaceSymbolParams,
    },
    LanguageServer,
};

#[tokio::test]
async fn goto_definition_and_implementation_find_proc() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/sample.masm").unwrap();
    let text = sample_program();
    harness.open_doc(uri.clone(), text).await;

    let pos = find_position(&sample_program(), "foo");
    let params = goto_params(uri.clone(), pos);
    let def = harness
        .backend
        .goto_definition(params.clone())
        .await
        .unwrap();
    let imp = harness
        .backend
        .goto_implementation(impl_params(uri.clone(), pos))
        .await
        .unwrap();

    let def_loc = expect_scalar(def);
    let imp_loc = expect_scalar(map_impl_to_def(imp));

    assert_eq!(def_loc.uri, uri);
    assert_eq!(imp_loc.uri, uri);
    // Definition should be on the `proc foo` line (line 0)
    assert_eq!(def_loc.range.start.line, 0);
}

#[tokio::test]
async fn find_references_returns_call_and_definition() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/sample.masm").unwrap();
    let text = sample_program();
    harness.open_doc(uri.clone(), text).await;

    let pos = find_position(&sample_program(), "foo");
    let params = ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: pos,
        },
        context: ReferenceContext {
            include_declaration: true,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let refs = harness.backend.references(params).await.unwrap().unwrap();

    // Expect at least declaration and one call.
    assert!(
        refs.len() >= 2,
        "expected at least 2 references, got {}",
        refs.len()
    );
    assert!(
        refs.iter().any(|loc| loc.range.start.line == 0),
        "declaration missing"
    );
    assert!(
        refs.iter().any(|loc| loc.range.start.line != 0),
        "call site missing"
    );
}

#[tokio::test]
async fn references_do_not_cross_modules_with_same_name() {
    let harness = TestHarness::new().await;

    let uri_a = Url::parse("file:///tmp/foo_a.masm").unwrap();
    let uri_b = Url::parse("file:///tmp/foo_b.masm").unwrap();

    let text_a = r#"proc foo
  push.1
end

proc call_a
  exec.foo
end
"#
    .to_string();

    let text_b = r#"proc foo
  push.2
end

proc call_b
  exec.foo
end
"#
    .to_string();

    harness.open_doc(uri_a.clone(), text_a.clone()).await;
    harness.open_doc(uri_b.clone(), text_b.clone()).await;

    let pos = find_position(&text_a, "foo");
    let params = ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri_a.clone() },
            position: pos,
        },
        context: ReferenceContext {
            include_declaration: true,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let refs = harness.backend.references(params).await.unwrap().unwrap();

    // Only the declaration and call inside module A should be returned.
    assert!(
        refs.iter().all(|loc| loc.uri == uri_a),
        "references from other modules should not be included"
    );
}

#[tokio::test]
async fn references_across_files_are_aggregated() {
    let harness = TestHarness::new().await;

    let def_uri = Url::parse("file:///tmp/defs.masm").unwrap();
    let call_uri_1 = Url::parse("file:///tmp/call1.masm").unwrap();
    let call_uri_2 = Url::parse("file:///tmp/call2.masm").unwrap();

    let def_text = r#"proc foo
  push.1
end
"#
    .to_string();

    let call_text_1 = r#"proc bar
  exec.::defs::foo
end
"#
    .to_string();

    let call_text_2 = r#"proc baz
  exec.::defs::foo
end
"#
    .to_string();

    harness.open_doc(def_uri.clone(), def_text.clone()).await;
    harness
        .open_doc(call_uri_1.clone(), call_text_1.clone())
        .await;
    harness
        .open_doc(call_uri_2.clone(), call_text_2.clone())
        .await;

    let def_doc = harness
        .backend
        .snapshot_document_symbols(&def_uri)
        .await
        .expect("def document symbols");
    let def_path = def_doc
        .definitions
        .iter()
        .find(|d| d.path.as_str().contains("foo"))
        .map(|d| d.path.clone())
        .expect("def path");

    // Ensure we are targeting the expected fully-qualified path.
    assert_eq!(def_path.as_str(), "::defs::foo");

    let pos = find_position(&def_text, "foo");
    let params = ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: def_uri.clone(),
            },
            position: pos,
        },
        context: ReferenceContext {
            include_declaration: true,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let refs = harness.backend.references(params).await.unwrap().unwrap();

    assert_eq!(
        refs.len(),
        3,
        "expected 3 references (def + 2 calls), got {}",
        refs.len()
    );
    assert!(
        refs.iter()
            .any(|loc| loc.uri == def_uri && loc.range.start.line == 0),
        "definition missing from references"
    );
    assert!(
        refs.iter()
            .any(|loc| loc.uri == call_uri_1 && loc.range.start.line == 1),
        "call site 1 missing"
    );
    assert!(
        refs.iter()
            .any(|loc| loc.uri == call_uri_2 && loc.range.start.line == 1),
        "call site 2 missing"
    );
}

#[tokio::test]
async fn references_include_multiple_calls_in_same_proc() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/multi_refs_same_proc.masm").unwrap();
    let text = r#"proc f
    nop
end

proc g
    exec.f
    exec.f
    exec.f
end
"#
    .to_string();

    harness.open_doc(uri.clone(), text.clone()).await;

    let pos_call = find_position(&text, "exec.f");
    let refs = harness
        .backend
        .references(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: pos_call,
            },
            context: ReferenceContext {
                include_declaration: true,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .await
        .expect("references")
        .expect("some references");

    let call_lines: Vec<_> = refs
        .iter()
        .filter(|loc| loc.uri == uri && loc.range.start.line != 0)
        .map(|loc| loc.range.start.line)
        .collect();

    assert!(
        call_lines.len() >= 3,
        "expected at least three call lines, got {:?}",
        call_lines
    );
}

#[tokio::test]
async fn workspace_symbol_finds_definition() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/sample.masm").unwrap();
    let text = sample_program();
    harness.open_doc(uri.clone(), text).await;

    let syms = harness
        .backend
        .symbol(WorkspaceSymbolParams {
            query: "foo".into(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
        .await
        .unwrap()
        .unwrap();

    assert!(
        syms.iter().any(|s| s.name == "foo"),
        "workspace symbol should include foo"
    );
}

#[tokio::test]
async fn goto_handles_aliases_and_imports() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/aliases.masm").unwrap();
    let text = fixture("module_with_aliases.masm");
    harness.open_doc(uri.clone(), text).await;

    let diags = harness.client.take_published().await;
    assert!(
        diags.iter().all(|(_, ds, _)| ds.is_empty()),
        "unexpected diagnostics in alias fixture: {:?}",
        diags
    );

    // Ensure index contains the alias definition
    {
        let ws = harness.backend.snapshot_workspace().await;
        let has_plus = ws
            .workspace_symbols("plus")
            .into_iter()
            .any(|(name, _)| name.contains("plus"));
        assert!(has_plus, "workspace index missing alias symbol");
        let doc = harness.backend.snapshot_document_symbols(&uri).await;
        assert!(doc.is_some(), "document symbols missing");
    }

    // resolve aliased add
    let pos = find_position(&fixture("module_with_aliases.masm"), "plus");
    let def = harness
        .backend
        .goto_definition(goto_params(uri.clone(), pos))
        .await
        .unwrap();
    let loc = expect_scalar(def);
    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 4); // alias declaration line
}

#[tokio::test]
async fn goto_definition_handles_procref() {
    let harness = TestHarness::new().await;
    let uri = Url::parse("file:///tmp/procref.masm").unwrap();
    let text = r#"proc foo
    nop
end

proc g
    procref.foo
end
"#
    .to_string();

    harness.open_doc(uri.clone(), text.clone()).await;

    let pos = find_position(&text, "foo");
    let def = harness
        .backend
        .goto_definition(goto_params(uri.clone(), pos))
        .await
        .unwrap();
    let loc = expect_scalar(def);
    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 0);
}

struct TestHarness {
    backend: Backend<RecordingClient>,
    client: RecordingClient,
}

impl TestHarness {
    async fn new() -> Self {
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        Self { backend, client }
    }

    async fn open_doc(&self, uri: Url, text: String) {
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "masm".into(),
                version: 1,
                text,
            },
        };
        self.backend.did_open(params).await;
    }
}

#[derive(Clone, Default)]
struct RecordingClient {
    published:
        std::sync::Arc<Mutex<Vec<(Url, Vec<tower_lsp::lsp_types::Diagnostic>, Option<i32>)>>>,
}

#[async_trait::async_trait]
impl PublishDiagnostics for RecordingClient {
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diagnostics: Vec<tower_lsp::lsp_types::Diagnostic>,
        version: Option<i32>,
    ) {
        let mut guard = self.published.lock().await;
        guard.push((uri, diagnostics, version));
    }
}

impl RecordingClient {
    async fn take_published(
        &self,
    ) -> Vec<(Url, Vec<tower_lsp::lsp_types::Diagnostic>, Option<i32>)> {
        let mut guard = self.published.lock().await;
        guard.drain(..).collect()
    }
}

fn sample_program() -> String {
    r#"proc foo
  push.1
end

begin
  exec.foo
end
"#
    .to_string()
}

fn fixture(name: &str) -> String {
    std::fs::read_to_string(format!("tests/fixtures/{name}")).expect("fixture")
}

fn goto_params(uri: Url, position: Position) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    }
}

fn impl_params(uri: Url, position: Position) -> GotoImplementationParams {
    GotoImplementationParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    }
}

fn find_position(text: &str, needle: &str) -> Position {
    let offset = text
        .rfind(needle)
        .unwrap_or_else(|| panic!("needle '{needle}' not found in text"));
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in text.char_indices() {
        if i == offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position::new(line, col)
}

fn expect_scalar(resp: Option<GotoDefinitionResponse>) -> Location {
    match resp {
        Some(GotoDefinitionResponse::Scalar(loc)) => loc,
        other => panic!("expected scalar definition, got {:?}", other),
    }
}

fn map_impl_to_def(resp: Option<GotoImplementationResponse>) -> Option<GotoDefinitionResponse> {
    resp.map(|r| match r {
        GotoImplementationResponse::Scalar(loc) => GotoDefinitionResponse::Scalar(loc),
        GotoImplementationResponse::Array(v) => GotoDefinitionResponse::Array(v),
        GotoImplementationResponse::Link(v) => GotoDefinitionResponse::Link(v),
    })
}
