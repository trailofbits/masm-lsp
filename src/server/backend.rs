use std::{fs, path::Path, sync::Arc};

use miden_assembly_syntax::{Parse, ParseOptions, SemanticAnalysisError};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use miden_utils_diagnostics as diagnostics;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    Diagnostic, Location, Position, Range, TextDocumentContentChangeEvent, Url,
};
use tracing::error;

use crate::{
    client::PublishDiagnostics,
    cursor_resolution::{resolve_symbol_at_position, ResolutionError, ResolvedSymbol},
    diagnostics::{diagnostics_from_report, unresolved_to_diagnostics},
    index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    module_path::ModulePathResolver,
    service::{DocumentService, WorkspaceService},
    symbol_path::SymbolPath,
    util::{lsp_range_to_selection, to_miden_uri},
    InlayHintType, LibraryPath, ServerConfig,
};

use super::cache::DocumentCache;
use super::helpers::determine_module_kind_from_ast;

#[derive(Debug, Clone)]
pub struct Backend<C = tower_lsp::Client> {
    pub(crate) client: C,
    pub(crate) sources: Arc<DefaultSourceManager>,
    pub(crate) documents: DocumentCache,
    pub(crate) workspace: Arc<RwLock<WorkspaceIndex>>,
    pub(crate) config: Arc<RwLock<ServerConfig>>,
}

impl<C> Backend<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            sources: Arc::new(DefaultSourceManager::default()),
            documents: DocumentCache::new(),
            workspace: Arc::new(RwLock::new(WorkspaceIndex::default())),
            config: Arc::new(RwLock::new(ServerConfig::default())),
        }
    }

    pub fn new_with_config(client: C, config: ServerConfig) -> Self {
        Self {
            config: Arc::new(RwLock::new(config)),
            ..Self::new(client)
        }
    }
}

impl<C> Backend<C> {
    pub async fn snapshot_workspace(&self) -> WorkspaceIndex {
        self.workspace.read().await.clone()
    }

    pub async fn snapshot_document_symbols(&self, uri: &Url) -> Option<DocumentSymbols> {
        self.documents.get_symbols(uri).await
    }

    pub async fn update_config(&self, cfg: ServerConfig) {
        let mut guard = self.config.write().await;
        *guard = cfg;
    }

    pub async fn snapshot_config(&self) -> ServerConfig {
        self.config.read().await.clone()
    }
}

impl<C> Backend<C>
where
    C: PublishDiagnostics,
{
    pub async fn load_configured_libraries(&self) {
        let cfg = self.snapshot_config().await;
        for lib in &cfg.library_paths {
            if let Err(err) = self.load_library_root(lib).await {
                error!("failed to load library root {}: {err}", lib.root.display());
            }
        }
    }

    pub async fn handle_open(
        &self,
        uri: Url,
        version: i32,
        text: String,
    ) -> std::result::Result<Vec<Diagnostic>, ()> {
        let miden_uri = to_miden_uri(&uri);
        self.sources.load(SourceLanguage::Masm, miden_uri, text);
        self.set_document_version(uri.clone(), version).await;
        let _ = self.parse_and_index(&uri).await;
        Ok(self.publish_diagnostics(uri).await)
    }

    pub async fn handle_change(
        &self,
        uri: Url,
        version: i32,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) {
        let miden_uri = to_miden_uri(&uri);
        let source_id = self.sources.find(&miden_uri);

        if let Some(id) = source_id {
            for change in changes {
                let selection = change.range.map(lsp_range_to_selection);
                if let Err(err) = self.sources.update(id, change.text, selection, version) {
                    error!("failed to apply change to {}: {err}", uri);
                }
            }
        } else if let Some(latest) = changes.last() {
            self.sources
                .load(SourceLanguage::Masm, miden_uri, latest.text.clone());
        }

        self.set_document_version(uri.clone(), version).await;
        let _ = self.parse_and_index(&uri).await;
        self.publish_diagnostics(uri).await;
    }

    pub async fn handle_close(&self, uri: Url) {
        self.documents.remove(&uri).await;
    }

    pub async fn set_document_version(&self, uri: Url, version: i32) {
        self.documents.set_version(uri, version).await;
    }

    pub async fn publish_diagnostics(&self, uri: Url) -> Vec<Diagnostic> {
        let version = self.documents.get_version(&uri).await;
        let parse_result = self.parse_and_index(&uri).await;

        let fallback_doc = if parse_result.is_err() {
            self.documents.get_symbols(&uri).await
        } else {
            None
        };

        let config = self.config.read().await;
        let taint_enabled = config.taint_analysis_enabled;
        let decompilation_enabled = config.inlay_hint_type == InlayHintType::Decompilation;
        drop(config);

        let workspace = self.workspace.read().await;
        let diagnostics = match parse_result {
            Ok(doc) => {
                let mut diags = Vec::new();
                diags.extend(unresolved_to_diagnostics(&uri, &doc, &workspace));

                if taint_enabled {
                    let taint_diags = crate::analysis::analyze_module(
                        &doc.module,
                        self.sources.as_ref(),
                        &uri,
                        Some(workspace.contracts()),
                    );
                    diags.extend(taint_diags);
                }

                if decompilation_enabled {
                    if let Some(source) = self.sources.get_by_uri(&to_miden_uri(&uri)) {
                        let full_range = Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: u32::MAX,
                                character: 0,
                            },
                        };
                        let decompilation_result = crate::decompiler::collect_decompilation_hints(
                            &doc.module,
                            self.sources.as_ref(),
                            &uri,
                            &full_range,
                            0,
                            source.as_str(),
                            Some(workspace.contracts()),
                        );
                        diags.extend(decompilation_result.diagnostics);
                    }
                }

                diags
            }
            Err(report) => {
                let mut diags = diagnostics_from_report(&self.sources, &uri, report);
                if let Some(doc) = fallback_doc {
                    diags.extend(unresolved_to_diagnostics(&uri, &doc, &workspace));

                    if taint_enabled {
                        let taint_diags = crate::analysis::analyze_module(
                            &doc.module,
                            self.sources.as_ref(),
                            &uri,
                            Some(workspace.contracts()),
                        );
                        diags.extend(taint_diags);
                    }

                    if decompilation_enabled {
                        if let Some(source) = self.sources.get_by_uri(&to_miden_uri(&uri)) {
                            let full_range = Range {
                                start: Position {
                                    line: 0,
                                    character: 0,
                                },
                                end: Position {
                                    line: u32::MAX,
                                    character: 0,
                                },
                            };
                            let decompilation_result =
                                crate::decompiler::collect_decompilation_hints(
                                    &doc.module,
                                    self.sources.as_ref(),
                                    &uri,
                                    &full_range,
                                    0,
                                    source.as_str(),
                                    Some(workspace.contracts()),
                                );
                            diags.extend(decompilation_result.diagnostics);
                        }
                    }
                }

                diags
            }
        };
        drop(workspace);

        self.client
            .publish_diagnostics(uri, diagnostics.clone(), version)
            .await;

        diagnostics
    }

    async fn parse_module(
        &self,
        uri: &Url,
    ) -> std::result::Result<Box<miden_assembly_syntax::ast::Module>, miden_assembly_syntax::Report>
    {
        use miden_assembly_syntax::ast::ModuleKind;

        let miden_uri = to_miden_uri(uri);
        let source_file = self
            .sources
            .get_by_uri(&miden_uri)
            .ok_or_else(|| diagnostics::report!("file not loaded in source manager"))?;

        let module_path = self.module_path_from_uri(uri).await;

        let opts = ParseOptions {
            kind: ModuleKind::Library,
            path: module_path.clone().map(Into::into),
            ..Default::default()
        };

        match source_file
            .clone()
            .parse_with_options(self.sources.as_ref(), opts)
        {
            Ok(mut module) => {
                let detected_kind = determine_module_kind_from_ast(&module);
                if module.kind() != detected_kind {
                    module.set_kind(detected_kind);
                }
                Ok(module)
            }
            Err(library_err) => {
                let should_try_executable =
                    if let Some(err) = library_err.downcast_ref::<SemanticAnalysisError>() {
                        matches!(err, SemanticAnalysisError::UnexpectedEntrypoint { .. })
                    } else {
                        source_file
                            .as_str()
                            .lines()
                            .any(|line| line.trim_start().starts_with("begin"))
                    };

                if should_try_executable {
                    match self
                        .try_parse_with_kind(uri, &source_file, ModuleKind::Executable)
                        .await
                    {
                        Ok(module) => return Ok(module),
                        Err(exec_err) => {
                            if library_err
                                .downcast_ref::<SemanticAnalysisError>()
                                .is_some()
                            {
                                return Err(exec_err);
                            }
                            return Err(library_err);
                        }
                    }
                }
                Err(library_err)
            }
        }
    }

    async fn try_parse_with_kind(
        &self,
        uri: &Url,
        source_file: &std::sync::Arc<miden_debug_types::SourceFile>,
        kind: miden_assembly_syntax::ast::ModuleKind,
    ) -> std::result::Result<Box<miden_assembly_syntax::ast::Module>, miden_assembly_syntax::Report>
    {
        let opts = ParseOptions {
            kind,
            path: self.module_path_from_uri(uri).await.map(Into::into),
            ..Default::default()
        };
        source_file
            .clone()
            .parse_with_options(self.sources.as_ref(), opts)
    }

    async fn module_path_from_uri(&self, uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
        let cfg = self.config.read().await;
        let resolver = ModulePathResolver::new(&cfg.library_paths);
        resolver.resolve(uri)
    }

    pub(crate) async fn parse_and_index(
        &self,
        uri: &Url,
    ) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
        let module = self.parse_module(uri).await?;
        let doc_symbols = build_document_symbols(module, self.sources.as_ref());

        let version = self.documents.get_version(uri).await.unwrap_or(0);
        self.documents
            .set_symbols(uri.clone(), doc_symbols.clone(), version)
            .await;

        {
            let mut ws = self.workspace.write().await;
            ws.update_document(
                uri.clone(),
                &doc_symbols.definitions,
                &doc_symbols.references,
            );
            ws.update_contracts(&doc_symbols.module, self.sources.as_ref());
        }

        Ok(doc_symbols)
    }

    pub(crate) async fn get_or_parse_document(&self, uri: &Url) -> Option<DocumentSymbols> {
        if let Some(doc) = self.documents.get_symbols_if_current(uri).await {
            return Some(doc);
        }
        self.parse_and_index(uri).await.ok()
    }

    async fn load_library_root(&self, lib: &LibraryPath) -> std::io::Result<()> {
        if !lib.root.is_dir() {
            return Ok(());
        }
        let mut stack = vec![lib.root.clone()];
        while let Some(dir) = stack.pop() {
            for entry in fs::read_dir(&dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext.eq_ignore_ascii_case("masm"))
                    .unwrap_or(false)
                {
                    self.load_library_file(&path).await?;
                }
            }
        }
        Ok(())
    }

    async fn load_library_file(&self, path: &Path) -> std::io::Result<()> {
        let text = fs::read_to_string(path)?;
        let url = Url::from_file_path(path).map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "invalid file path for Url",
            )
        })?;
        let miden_uri = to_miden_uri(&url);
        self.sources.load(SourceLanguage::Masm, miden_uri, text);
        let _ = self.parse_and_index(&url).await;
        Ok(())
    }
}

// ----------------------------------------------------------------------------
// Service trait implementations
// ----------------------------------------------------------------------------

#[async_trait::async_trait]
impl<C> DocumentService for Backend<C>
where
    C: PublishDiagnostics,
{
    async fn get_document_symbols(&self, uri: &Url) -> Option<DocumentSymbols> {
        self.get_or_parse_document(uri).await
    }

    async fn resolve_at_position(
        &self,
        uri: &Url,
        position: Position,
    ) -> std::result::Result<ResolvedSymbol, ResolutionError> {
        let doc = self
            .get_or_parse_document(uri)
            .await
            .ok_or_else(|| ResolutionError::SourceNotFound(uri.clone()))?;
        resolve_symbol_at_position(uri, &doc.module, &self.sources, position)
    }
}

impl<C> WorkspaceService for Backend<C>
where
    C: PublishDiagnostics,
{
    fn find_definition(&self, _path: &SymbolPath) -> Option<Location> {
        None
    }

    fn find_references(&self, _path: &SymbolPath) -> Vec<Location> {
        Vec::new()
    }

    fn search_symbols(&self, _query: &str) -> Vec<(String, Location)> {
        Vec::new()
    }
}

/// A snapshot wrapper around WorkspaceIndex that implements WorkspaceService.
#[allow(dead_code)]
pub struct WorkspaceIndexWrapper(pub WorkspaceIndex);

impl WorkspaceService for WorkspaceIndexWrapper {
    fn find_definition(&self, path: &SymbolPath) -> Option<Location> {
        self.0
            .definition(path.as_str())
            .or_else(|| self.0.definition_by_name(path.name()))
    }

    fn find_references(&self, path: &SymbolPath) -> Vec<Location> {
        self.0.references(path.as_str())
    }

    fn search_symbols(&self, query: &str) -> Vec<(String, Location)> {
        self.0.workspace_symbols(query)
    }
}
