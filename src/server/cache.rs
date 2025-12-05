use std::collections::HashMap;
use std::sync::Arc;

use tower_lsp::lsp_types::Url;

use crate::index::DocumentSymbols;

#[derive(Debug, Default, Clone)]
struct DocumentState {
    version: i32,
}

/// Cached document symbols with version tracking.
#[derive(Debug, Clone)]
struct CachedSymbols {
    symbols: DocumentSymbols,
    version: i32,
}

/// Cache for document state and parsed symbols.
///
/// Encapsulates tracking of open documents and their parsed symbol information.
#[derive(Debug, Default, Clone)]
pub struct DocumentCache {
    state: Arc<tokio::sync::RwLock<HashMap<Url, DocumentState>>>,
    symbols: Arc<tokio::sync::RwLock<HashMap<Url, CachedSymbols>>>,
}

impl DocumentCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn get_version(&self, uri: &Url) -> Option<i32> {
        let state = self.state.read().await;
        state.get(uri).map(|doc| doc.version)
    }

    pub async fn set_version(&self, uri: Url, version: i32) {
        let mut state = self.state.write().await;
        state.insert(uri, DocumentState { version });
    }

    pub async fn remove(&self, uri: &Url) {
        let mut state = self.state.write().await;
        state.remove(uri);
    }

    pub async fn get_symbols_if_current(&self, uri: &Url) -> Option<DocumentSymbols> {
        let current_version = self.get_version(uri).await?;
        let symbols = self.symbols.read().await;
        let cached = symbols.get(uri)?;
        (cached.version == current_version).then(|| cached.symbols.clone())
    }

    pub async fn get_symbols(&self, uri: &Url) -> Option<DocumentSymbols> {
        let symbols = self.symbols.read().await;
        symbols.get(uri).map(|c| c.symbols.clone())
    }

    pub async fn set_symbols(&self, uri: Url, doc_symbols: DocumentSymbols, version: i32) {
        let mut symbols = self.symbols.write().await;
        symbols.insert(
            uri,
            CachedSymbols {
                symbols: doc_symbols,
                version,
            },
        );
    }
}
