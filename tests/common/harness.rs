//! Enhanced test harness for integration testing.

#![allow(dead_code)]

use std::sync::Arc;

use async_trait::async_trait;
use masm_lsp::client::PublishDiagnostics;
use masm_lsp::server::Backend;
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{
    Diagnostic, DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverParams, Location, Position, ReferenceContext, ReferenceParams, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentPositionParams, Url, WorkspaceSymbolParams,
};
use tower_lsp::LanguageServer;

use super::fixtures::{find_position, FixtureKind};

/// A recording LSP client that captures published diagnostics.
#[derive(Clone, Default)]
pub struct RecordingClient {
    published: Arc<Mutex<Vec<(Url, Vec<Diagnostic>, Option<i32>)>>>,
}

#[async_trait]
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

impl RecordingClient {
    /// Create a new recording client.
    pub fn new() -> Self {
        Self::default()
    }

    /// Take all published diagnostics, clearing the internal buffer.
    pub async fn take_published(&self) -> Vec<(Url, Vec<Diagnostic>, Option<i32>)> {
        let mut guard = self.published.lock().await;
        guard.drain(..).collect()
    }

    /// Get diagnostics for a specific URI.
    pub async fn diagnostics_for(&self, uri: &Url) -> Vec<Diagnostic> {
        let guard = self.published.lock().await;
        guard
            .iter()
            .filter(|(u, _, _)| u == uri)
            .flat_map(|(_, d, _)| d.clone())
            .collect()
    }
}

/// Test harness that wraps a Backend with a RecordingClient.
pub struct TestHarness {
    pub backend: Backend<RecordingClient>,
    pub client: RecordingClient,
}

impl TestHarness {
    /// Create a new test harness.
    pub async fn new() -> Self {
        let client = RecordingClient::new();
        let backend = Backend::new(client.clone());
        Self { backend, client }
    }

    /// Open a document in the backend.
    pub async fn open_doc(&self, uri: Url, text: String) {
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

    /// Open a fixture by kind.
    pub async fn open_fixture(&self, kind: FixtureKind) -> Url {
        let uri = kind.url();
        let content = kind.content();
        self.open_doc(uri.clone(), content).await;
        uri
    }

    /// Open inline content with a generated URI.
    pub async fn open_inline(&self, name: &str, content: &str) -> Url {
        let uri = Url::parse(&format!("file:///tmp/test/{}", name)).expect("valid URL");
        self.open_doc(uri.clone(), content.to_string()).await;
        uri
    }

    /// Assert that no diagnostics were published for a URI.
    pub async fn assert_no_diagnostics(&self, uri: &Url) {
        let diags = self.client.diagnostics_for(uri).await;
        assert!(
            diags.is_empty(),
            "expected no diagnostics for {}, got: {:?}",
            uri,
            diags
        );
    }

    /// Assert that diagnostics were published for a URI.
    pub async fn assert_has_diagnostics(&self, uri: &Url) {
        let diags = self.client.diagnostics_for(uri).await;
        assert!(
            !diags.is_empty(),
            "expected diagnostics for {}, got none",
            uri
        );
    }

    /// Assert that a diagnostic message contains specific text.
    pub async fn assert_diagnostic_contains(&self, uri: &Url, text: &str) {
        let diags = self.client.diagnostics_for(uri).await;
        let found = diags.iter().any(|d| d.message.contains(text));
        assert!(
            found,
            "expected diagnostic containing '{}' for {}, got: {:?}",
            text, uri, diags
        );
    }

    /// Perform goto definition and return the result.
    pub async fn goto_definition(&self, uri: &Url, position: Position) -> Option<Location> {
        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let result = self.backend.goto_definition(params).await.ok()?;
        match result {
            Some(GotoDefinitionResponse::Scalar(loc)) => Some(loc),
            Some(GotoDefinitionResponse::Array(locs)) if !locs.is_empty() => {
                Some(locs.into_iter().next().unwrap())
            }
            _ => None,
        }
    }

    /// Perform goto definition at a text needle.
    pub async fn goto_definition_at(
        &self,
        uri: &Url,
        text: &str,
        needle: &str,
    ) -> Option<Location> {
        let position = find_position(text, needle);
        self.goto_definition(uri, position).await
    }

    /// Find all references to a symbol at a position.
    pub async fn find_references(
        &self,
        uri: &Url,
        position: Position,
        include_declaration: bool,
    ) -> Vec<Location> {
        let params = ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position,
            },
            context: ReferenceContext {
                include_declaration,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        self.backend
            .references(params)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    /// Find references at a text needle.
    pub async fn find_references_at(
        &self,
        uri: &Url,
        text: &str,
        needle: &str,
        include_declaration: bool,
    ) -> Vec<Location> {
        let position = find_position(text, needle);
        self.find_references(uri, position, include_declaration)
            .await
    }

    /// Search for workspace symbols.
    pub async fn workspace_symbols(
        &self,
        query: &str,
    ) -> Vec<tower_lsp::lsp_types::SymbolInformation> {
        let params = WorkspaceSymbolParams {
            query: query.to_string(),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        self.backend
            .symbol(params)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    /// Assert that a workspace symbol exists with the given name.
    pub async fn assert_workspace_symbol_exists(&self, name: &str) {
        let symbols = self.workspace_symbols(name).await;
        let found = symbols.iter().any(|s| s.name.contains(name));
        assert!(
            found,
            "expected workspace symbol '{}', found: {:?}",
            name,
            symbols.iter().map(|s| &s.name).collect::<Vec<_>>()
        );
    }

    /// Get hover information at a position.
    pub async fn hover(&self, uri: &Url, position: Position) -> Option<Hover> {
        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position,
            },
            work_done_progress_params: Default::default(),
        };
        self.backend.hover(params).await.ok().flatten()
    }

    /// Get hover information at a text needle.
    pub async fn hover_at(&self, uri: &Url, text: &str, needle: &str) -> Option<Hover> {
        let position = find_position(text, needle);
        self.hover(uri, position).await
    }

    /// Get inlay hints for a document within a range.
    pub async fn inlay_hints(
        &self,
        uri: &Url,
        range: tower_lsp::lsp_types::Range,
    ) -> Vec<tower_lsp::lsp_types::InlayHint> {
        use tower_lsp::lsp_types::{InlayHintParams, TextDocumentIdentifier};
        use tower_lsp::LanguageServer;

        let params = InlayHintParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            range,
            work_done_progress_params: Default::default(),
        };
        self.backend
            .inlay_hint(params)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    /// Enable decompilation inlay hints.
    pub async fn enable_decompilation_hints(&self) {
        let mut config = self.backend.snapshot_config().await;
        config.inlay_hint_type = masm_lsp::InlayHintType::Decompilation;
        self.backend.update_config(config).await;
    }

    /// Disable inlay hints.
    pub async fn disable_inlay_hints(&self) {
        let mut config = self.backend.snapshot_config().await;
        config.inlay_hint_type = masm_lsp::InlayHintType::None;
        self.backend.update_config(config).await;
    }
}
