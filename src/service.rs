//! Service traits for testing LSP functionality without full transport setup.
//!
//! These traits abstract the core document and workspace operations, allowing
//! unit tests to use mock implementations instead of the full `Backend`.

use async_trait::async_trait;
use tower_lsp::lsp_types::{Location, Position, Url};

use crate::cursor_resolution::{ResolutionError, ResolvedSymbol};
use crate::index::DocumentSymbols;
use crate::symbol_path::SymbolPath;

/// Service for document-level operations.
///
/// This trait abstracts operations on individual documents, such as
/// retrieving parsed symbols and resolving symbols at cursor positions.
#[async_trait]
pub trait DocumentService: Send + Sync {
    /// Get the parsed symbols for a document.
    async fn get_document_symbols(&self, uri: &Url) -> Option<DocumentSymbols>;

    /// Resolve the symbol at a given position in a document.
    async fn resolve_at_position(
        &self,
        uri: &Url,
        position: Position,
    ) -> Result<ResolvedSymbol, ResolutionError>;
}

/// Service for workspace-level operations.
///
/// This trait abstracts cross-file operations such as finding definitions
/// and references across the entire workspace.
#[async_trait]
pub trait WorkspaceService: Send + Sync {
    /// Find the definition of a symbol by its path.
    ///
    /// Tries exact match first, then suffix match, then name match.
    fn find_definition(&self, path: &SymbolPath) -> Option<Location>;

    /// Find all references to a symbol by its path.
    fn find_references(&self, path: &SymbolPath) -> Vec<Location>;

    /// Search for workspace symbols matching a query string.
    fn search_symbols(&self, query: &str) -> Vec<(String, Location)>;
}

/// Handle a goto definition request using service traits.
///
/// This function contains the core logic for goto definition, decoupled from
/// the LSP transport layer.
pub async fn handle_goto_definition<D, W>(
    doc_service: &D,
    workspace_service: &W,
    uri: &Url,
    position: Position,
) -> Option<Location>
where
    D: DocumentService,
    W: WorkspaceService,
{
    // Try to resolve the symbol at the cursor position
    let resolved = doc_service.resolve_at_position(uri, position).await.ok()?;

    // Look up the definition in the workspace
    workspace_service.find_definition(&resolved.path)
}

/// Handle a find references request using service traits.
///
/// This function contains the core logic for find references, decoupled from
/// the LSP transport layer.
pub async fn handle_find_references<D, W>(
    doc_service: &D,
    workspace_service: &W,
    uri: &Url,
    position: Position,
    include_declaration: bool,
) -> Option<Vec<Location>>
where
    D: DocumentService,
    W: WorkspaceService,
{
    // Try to resolve the symbol at the cursor position
    let resolved = doc_service.resolve_at_position(uri, position).await.ok()?;

    // Find all references to this symbol
    let mut results = workspace_service.find_references(&resolved.path);

    // Include the definition if requested
    if include_declaration {
        if let Some(def) = workspace_service.find_definition(&resolved.path) {
            results.push(def);
        }
    }

    if results.is_empty() {
        None
    } else {
        Some(results)
    }
}

/// Handle a workspace symbol search using service traits.
pub fn handle_workspace_symbols<W>(workspace_service: &W, query: &str) -> Vec<(String, Location)>
where
    W: WorkspaceService,
{
    workspace_service.search_symbols(query)
}

#[cfg(test)]
pub mod mocks {
    //! Mock implementations for testing.

    use super::*;
    use std::collections::HashMap;
    use std::collections::HashSet;

    /// A mock document service for testing.
    #[derive(Default)]
    pub struct MockDocumentService {
        symbols: HashMap<Url, DocumentSymbols>,
        /// Successful resolutions stored by position.
        successful_resolutions: HashMap<(Url, u32, u32), ResolvedSymbol>,
        /// Positions that should return errors (stores error message).
        error_positions: HashSet<(Url, u32, u32)>,
    }

    impl MockDocumentService {
        /// Create a new empty mock service.
        pub fn new() -> Self {
            Self::default()
        }

        /// Add document symbols for a URI.
        pub fn with_symbols(mut self, uri: Url, symbols: DocumentSymbols) -> Self {
            self.symbols.insert(uri, symbols);
            self
        }

        /// Add a successful resolution result for a specific position.
        pub fn with_resolution(
            mut self,
            uri: Url,
            line: u32,
            column: u32,
            result: Result<ResolvedSymbol, ResolutionError>,
        ) -> Self {
            match result {
                Ok(symbol) => {
                    self.successful_resolutions
                        .insert((uri, line, column), symbol);
                }
                Err(_) => {
                    self.error_positions.insert((uri, line, column));
                }
            }
            self
        }
    }

    #[async_trait]
    impl DocumentService for MockDocumentService {
        async fn get_document_symbols(&self, uri: &Url) -> Option<DocumentSymbols> {
            self.symbols.get(uri).cloned()
        }

        async fn resolve_at_position(
            &self,
            uri: &Url,
            position: Position,
        ) -> Result<ResolvedSymbol, ResolutionError> {
            let key = (uri.clone(), position.line, position.character);

            if self.error_positions.contains(&key) {
                return Err(ResolutionError::SymbolNotFound("mock error".to_string()));
            }

            self.successful_resolutions
                .get(&key)
                .cloned()
                .ok_or_else(|| ResolutionError::NoTokenAtPosition {
                    line: position.line,
                    column: position.character,
                })
        }
    }

    /// A mock workspace service for testing.
    #[derive(Default)]
    pub struct MockWorkspaceService {
        definitions: HashMap<SymbolPath, Location>,
        references: HashMap<SymbolPath, Vec<Location>>,
    }

    impl MockWorkspaceService {
        /// Create a new empty mock service.
        pub fn new() -> Self {
            Self::default()
        }

        /// Add a definition for a symbol path.
        pub fn with_definition(mut self, path: SymbolPath, location: Location) -> Self {
            self.definitions.insert(path, location);
            self
        }

        /// Add references for a symbol path.
        pub fn with_references(mut self, path: SymbolPath, locations: Vec<Location>) -> Self {
            self.references.insert(path, locations);
            self
        }
    }

    impl WorkspaceService for MockWorkspaceService {
        fn find_definition(&self, path: &SymbolPath) -> Option<Location> {
            // Try exact match first
            if let Some(loc) = self.definitions.get(path) {
                return Some(loc.clone());
            }

            // Try suffix match
            for (def_path, loc) in &self.definitions {
                if def_path.ends_with(path.as_str()) || path.ends_with(def_path.as_str()) {
                    return Some(loc.clone());
                }
            }

            // Try name match
            let name = path.name();
            for (def_path, loc) in &self.definitions {
                if def_path.name_matches(name) {
                    return Some(loc.clone());
                }
            }

            None
        }

        fn find_references(&self, path: &SymbolPath) -> Vec<Location> {
            // Try exact match first
            if let Some(locs) = self.references.get(path) {
                return locs.clone();
            }

            // Try suffix match
            let mut results = Vec::new();
            for (ref_path, locs) in &self.references {
                if ref_path.ends_with(path.as_str()) || path.ends_with(ref_path.as_str()) {
                    results.extend(locs.clone());
                }
            }

            results
        }

        fn search_symbols(&self, query: &str) -> Vec<(String, Location)> {
            self.definitions
                .iter()
                .filter(|(path, _)| path.as_str().contains(query) || path.name().contains(query))
                .map(|(path, loc)| (path.to_string(), loc.clone()))
                .collect()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::mocks::*;
    use super::*;
    use tower_lsp::lsp_types::Range;

    fn test_uri() -> Url {
        Url::parse("file:///tmp/test.masm").unwrap()
    }

    fn test_location(line: u32) -> Location {
        Location::new(
            test_uri(),
            Range::new(Position::new(line, 0), Position::new(line, 10)),
        )
    }

    #[tokio::test]
    async fn test_goto_definition_finds_exact_match() {
        let uri = test_uri();
        let path = SymbolPath::new("::module::foo");
        let def_location = test_location(5);

        let doc_service = MockDocumentService::new().with_resolution(
            uri.clone(),
            0,
            0,
            Ok(ResolvedSymbol {
                path: path.clone(),
                name: "foo".to_string(),
            }),
        );

        let workspace_service =
            MockWorkspaceService::new().with_definition(path.clone(), def_location.clone());

        let result =
            handle_goto_definition(&doc_service, &workspace_service, &uri, Position::new(0, 0))
                .await;

        assert_eq!(result, Some(def_location));
    }

    #[tokio::test]
    async fn test_goto_definition_returns_none_when_symbol_not_found() {
        let uri = test_uri();

        let doc_service = MockDocumentService::new().with_resolution(
            uri.clone(),
            0,
            0,
            Err(ResolutionError::SymbolNotFound("unknown".to_string())),
        );

        let workspace_service = MockWorkspaceService::new();

        let result =
            handle_goto_definition(&doc_service, &workspace_service, &uri, Position::new(0, 0))
                .await;

        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn test_find_references_returns_all_references() {
        let uri = test_uri();
        let path = SymbolPath::new("::module::bar");
        let ref_locations = vec![test_location(10), test_location(20)];

        let doc_service = MockDocumentService::new().with_resolution(
            uri.clone(),
            0,
            0,
            Ok(ResolvedSymbol {
                path: path.clone(),
                name: "bar".to_string(),
            }),
        );

        let workspace_service =
            MockWorkspaceService::new().with_references(path.clone(), ref_locations.clone());

        let result = handle_find_references(
            &doc_service,
            &workspace_service,
            &uri,
            Position::new(0, 0),
            false,
        )
        .await;

        assert_eq!(result, Some(ref_locations));
    }

    #[tokio::test]
    async fn test_find_references_includes_declaration() {
        let uri = test_uri();
        let path = SymbolPath::new("::module::baz");
        let def_location = test_location(5);
        let ref_locations = vec![test_location(10)];

        let doc_service = MockDocumentService::new().with_resolution(
            uri.clone(),
            0,
            0,
            Ok(ResolvedSymbol {
                path: path.clone(),
                name: "baz".to_string(),
            }),
        );

        let workspace_service = MockWorkspaceService::new()
            .with_definition(path.clone(), def_location.clone())
            .with_references(path.clone(), ref_locations.clone());

        let result = handle_find_references(
            &doc_service,
            &workspace_service,
            &uri,
            Position::new(0, 0),
            true,
        )
        .await;

        let expected = vec![test_location(10), def_location];
        assert_eq!(result, Some(expected));
    }

    #[tokio::test]
    async fn test_find_references_returns_none_when_empty() {
        let uri = test_uri();
        let path = SymbolPath::new("::module::empty");

        let doc_service = MockDocumentService::new().with_resolution(
            uri.clone(),
            0,
            0,
            Ok(ResolvedSymbol {
                path: path.clone(),
                name: "empty".to_string(),
            }),
        );

        let workspace_service = MockWorkspaceService::new();

        let result = handle_find_references(
            &doc_service,
            &workspace_service,
            &uri,
            Position::new(0, 0),
            false,
        )
        .await;

        assert_eq!(result, None);
    }

    #[test]
    fn test_workspace_symbols_filters_by_query() {
        let workspace_service = MockWorkspaceService::new()
            .with_definition(SymbolPath::new("::std::crypto::hash"), test_location(1))
            .with_definition(SymbolPath::new("::std::math::add"), test_location(2))
            .with_definition(SymbolPath::new("::app::crypto_utils"), test_location(3));

        let results = handle_workspace_symbols(&workspace_service, "crypto");

        assert_eq!(results.len(), 2);
        assert!(results.iter().any(|(name, _)| name.contains("crypto")));
    }

    #[test]
    fn test_mock_workspace_finds_by_suffix() {
        let workspace_service = MockWorkspaceService::new().with_definition(
            SymbolPath::new("::std::crypto::sha256::hash"),
            test_location(1),
        );

        // Should find by suffix
        let result = workspace_service.find_definition(&SymbolPath::new("sha256::hash"));
        assert!(result.is_some());

        // Should find by name
        let result = workspace_service.find_definition(&SymbolPath::new("hash"));
        assert!(result.is_some());
    }
}
