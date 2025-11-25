use async_trait::async_trait;
use tower_lsp::{
    lsp_types::{Diagnostic, Url},
    Client,
};

/// Minimal abstraction over publishing diagnostics so the backend can be tested without a real LSP client.
#[async_trait]
pub trait PublishDiagnostics: Clone + Send + Sync + 'static {
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diagnostics: Vec<Diagnostic>,
        version: Option<i32>,
    );
}

#[async_trait]
impl PublishDiagnostics for Client {
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diagnostics: Vec<Diagnostic>,
        version: Option<i32>,
    ) {
        Client::publish_diagnostics(self, uri, diagnostics, version).await;
    }
}
