use std::{
    collections::HashMap,
    fs,
    path::Path,
    sync::Arc,
};

use miden_assembly_syntax::{Parse, ParseOptions, SemanticAnalysisError};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use miden_utils_diagnostics as diagnostics;
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        self,
        request::{GotoImplementationParams, GotoImplementationResponse},
        Diagnostic, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
        HoverParams, InitializeParams, InitializeResult, InitializedParams, InlayHint,
        InlayHintParams, Location, MarkupContent, MarkupKind, ReferenceParams, ServerCapabilities,
        SymbolInformation, SymbolKind, TextDocumentContentChangeEvent, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkspaceSymbolParams,
    },
    LanguageServer,
};
use tracing::{debug, error, info};

use crate::{
    client::PublishDiagnostics,
    diagnostics::{diagnostics_from_report, unresolved_to_diagnostics},
    index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    inlay_hints::{collect_inlay_hints, get_instruction_hover},
    resolution::{resolve_symbol_at_position, ResolvedSymbol, ResolutionError},
    service::{DocumentService, WorkspaceService},
    symbol_path::SymbolPath,
    util::{extract_token_at_position, extract_word_at_position, lsp_range_to_selection, to_miden_uri},
    LibraryPath, ServerConfig,
};

#[derive(Debug, Default, Clone)]
struct DocumentState {
    version: i32,
}

#[derive(Debug, Clone)]
pub struct Backend<C = tower_lsp::Client> {
    client: C,
    sources: Arc<DefaultSourceManager>,
    documents: Arc<RwLock<HashMap<Url, DocumentState>>>,
    symbols: Arc<RwLock<HashMap<Url, DocumentSymbols>>>,
    workspace: Arc<RwLock<WorkspaceIndex>>,
    config: Arc<RwLock<ServerConfig>>,
}

impl<C> Backend<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            sources: Arc::new(DefaultSourceManager::default()),
            documents: Arc::new(RwLock::new(HashMap::new())),
            symbols: Arc::new(RwLock::new(HashMap::new())),
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
        let docs = self.symbols.read().await;
        docs.get(uri).cloned()
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

    async fn handle_open(
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

    async fn handle_change(
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

    async fn handle_close(&self, uri: Url) {
        let mut docs = self.documents.write().await;
        docs.remove(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn set_document_version(&self, uri: Url, version: i32) {
        let mut docs = self.documents.write().await;
        docs.insert(uri, DocumentState { version });
    }

    async fn publish_diagnostics(&self, uri: Url) -> Vec<Diagnostic> {
        let version = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|doc| doc.version)
        };

        // Parse and index first (this may update the workspace)
        let parse_result = self.parse_and_index(&uri).await;

        // Get document symbols if parse failed (need this before acquiring workspace lock)
        let fallback_doc = if parse_result.is_err() {
            self.symbols.read().await.get(&uri).cloned()
        } else {
            None
        };

        // Then acquire a read lock on the workspace after parsing is done
        let workspace = self.workspace.read().await;
        let diagnostics = match parse_result {
            Ok(doc) => {
                let mut diags = Vec::new();
                diags.extend(unresolved_to_diagnostics(&uri, &doc, &workspace));
                diags
            }
            Err(report) => {
                let mut diags = diagnostics_from_report(&self.sources, &uri, report);
                if let Some(doc) = fallback_doc {
                    diags.extend(unresolved_to_diagnostics(&uri, &doc, &workspace));
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

        // Build parse options with module path
        let module_path = self.module_path_from_uri(uri).await;

        // Try parsing with Library kind first (most permissive for many constructs)
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
                // Successfully parsed - now determine the correct kind from AST
                let detected_kind = determine_module_kind_from_ast(&module);
                if module.kind() != detected_kind {
                    module.set_kind(detected_kind);
                }
                Ok(module)
            }
            Err(library_err) => {
                // Library parsing failed - check if it's an entrypoint-related error
                let should_try_executable =
                    if let Some(err) = library_err.downcast_ref::<SemanticAnalysisError>() {
                        matches!(err, SemanticAnalysisError::UnexpectedEntrypoint { .. })
                    } else {
                        // For syntax errors, check if the source contains a begin block
                        // This handles cases where begin causes a syntax error in Library mode
                        source_file
                            .as_str()
                            .lines()
                            .any(|line| line.trim_start().starts_with("begin"))
                    };

                if should_try_executable {
                    // Try parsing as Executable
                    match self
                        .try_parse_with_kind(uri, &source_file, ModuleKind::Executable)
                        .await
                    {
                        Ok(module) => return Ok(module),
                        Err(exec_err) => {
                            // If Executable also failed, check if Library was a better error
                            // Return the error from the kind that seemed more appropriate
                            if library_err.downcast_ref::<SemanticAnalysisError>().is_some() {
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
        if let Ok(fs_path) = uri.to_file_path() {
            let cfg = self.config.read().await;
            for lib in &cfg.library_paths {
                if fs_path.starts_with(&lib.root) {
                    if let Some(buf) = build_path_from_root(&fs_path, &lib.root, &lib.prefix) {
                        return Some(buf);
                    }
                }
            }
        }
        module_path_from_uri_fallback(uri)
    }

    async fn parse_and_index(
        &self,
        uri: &Url,
    ) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
        let module = self.parse_module(uri).await?;
        let doc_symbols = build_document_symbols(module, self.sources.as_ref());

        {
            let mut docs = self.symbols.write().await;
            docs.insert(uri.clone(), doc_symbols.clone());
        }

        {
            let mut ws = self.workspace.write().await;
            ws.update_document(
                uri.clone(),
                &doc_symbols.definitions,
                &doc_symbols.references,
            );
        }

        Ok(doc_symbols)
    }

    async fn get_or_parse_document(&self, uri: &Url) -> Option<DocumentSymbols> {
        {
            let docs = self.symbols.read().await;
            if let Some(doc) = docs.get(uri) {
                return Some(doc.clone());
            }
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
        position: lsp_types::Position,
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
        // Note: This is a synchronous method that needs workspace access.
        // In async contexts, use snapshot_workspace() first, then call methods on the snapshot.
        // For the trait implementation, we provide a blocking version.
        // The LSP handlers should continue to use the async pattern.
        None // Placeholder - see WorkspaceIndexWrapper below
    }

    fn find_references(&self, _path: &SymbolPath) -> Vec<Location> {
        Vec::new() // Placeholder - see WorkspaceIndexWrapper below
    }

    fn search_symbols(&self, _query: &str) -> Vec<(String, Location)> {
        Vec::new() // Placeholder - see WorkspaceIndexWrapper below
    }
}

/// A snapshot wrapper around WorkspaceIndex that implements WorkspaceService.
///
/// This allows using a workspace snapshot with the service trait functions
/// without needing async access.
pub struct WorkspaceIndexWrapper(pub WorkspaceIndex);

impl WorkspaceService for WorkspaceIndexWrapper {
    fn find_definition(&self, path: &SymbolPath) -> Option<Location> {
        self.0
            .definition(path.as_str())
            .or_else(|| self.0.definition_by_suffix(path.as_str()))
            .or_else(|| self.0.definition_by_name(path.name()))
    }

    fn find_references(&self, path: &SymbolPath) -> Vec<Location> {
        let mut results = self.0.references(path.as_str());
        if results.is_empty() {
            results = self.0.references_by_suffix(path.as_str());
        }
        results
    }

    fn search_symbols(&self, query: &str) -> Vec<(String, Location)> {
        self.0.workspace_symbols(query)
    }
}

#[tower_lsp::async_trait]
impl<C> LanguageServer for Backend<C>
where
    C: PublishDiagnostics,
{
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            implementation_provider: Some(lsp_types::ImplementationProviderCapability::Simple(
                true,
            )),
            references_provider: Some(lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("MASM LSP initialized");
        self.load_configured_libraries().await;
    }

    async fn did_change_configuration(&self, params: lsp_types::DidChangeConfigurationParams) {
        let mut cfg = self.config.write().await;
        if let Some(tab_count) = extract_tab_count(&params.settings) {
            cfg.inlay_hint_tabs = tab_count;
            info!("updated inlay hint tab padding: {}", tab_count);
        }
        if let Some(paths) = extract_library_paths(&params.settings) {
            cfg.library_paths = paths;
            info!("updated library search paths");
        }
        drop(cfg);
        self.load_configured_libraries().await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let text = params.text_document.text;
        let _ = self.handle_open(uri, version, text).await;
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        self.handle_change(uri, version, params.content_changes)
            .await;
        // Re-run diagnostics on the updated document.
        let _ = self.publish_diagnostics(params.text_document.uri).await;
    }

    async fn did_close(&self, params: lsp_types::DidCloseTextDocumentParams) {
        self.handle_close(params.text_document.uri).await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let miden_uri = to_miden_uri(&uri);

        // Check if cursor is on a `use` statement - resolve as module path lookup
        if let Some(source) = self.sources.get_by_uri(&miden_uri) {
            if is_on_use_statement(source.as_str(), pos) {
                if let Some(token) = extract_token_at_position(&source, pos) {
                    let normalized = token.trim_start_matches(':');
                    let workspace = self.workspace.read().await;

                    if let Some(loc) = workspace
                        .definition(&format!("::{}", normalized))
                        .or_else(|| workspace.definition_by_suffix(normalized))
                        .or_else(|| {
                            // Try matching by the last component (module name)
                            normalized
                                .rsplit("::")
                                .next()
                                .and_then(|name| workspace.definition_by_name(name))
                        })
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
                    }
                }
            }
        }

        // Standard symbol resolution for procedure calls, etc.
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let symbol = match resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos) {
            Ok(s) => s,
            Err(e) => {
                debug!("goto_definition: {e}");
                return Ok(None);
            }
        };

        let workspace = self.workspace.read().await;
        if let Some(loc) = workspace
            .definition(symbol.path.as_str())
            .or_else(|| workspace.definition_by_suffix(symbol.path.as_str()))
            .or_else(|| workspace.definition_by_name(&symbol.name))
        {
            return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        // Check if instruction hovers are enabled
        let instruction_hovers_enabled = self.config.read().await.instruction_hovers_enabled;

        // Extract tokens at position for hover checks
        let source = self.sources.get_by_uri(&to_miden_uri(&uri));
        let token = source.as_ref().and_then(|s| extract_token_at_position(s, pos));
        let word = source.as_ref().and_then(|s| extract_word_at_position(s, pos));

        // Check if cursor is directly on an invocation instruction keyword (exec, call, syscall, procref)
        // Only show instruction hover when cursor is on the keyword itself, not the target name
        if instruction_hovers_enabled {
            if let Some(ref w) = word {
                if matches!(w.as_str(), "exec" | "call" | "syscall" | "procref") {
                    if let Some(hover_text) = get_instruction_hover(w) {
                        return Ok(Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: hover_text,
                            }),
                            range: None,
                        }));
                    }
                }
            }
        }

        // Try to resolve as a symbol (procedure, constant, etc.)
        match resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos) {
            Ok(symbol) => {
                // Find the definition location
                let workspace = self.workspace.read().await;
                let def_loc = workspace
                    .definition(symbol.path.as_str())
                    .or_else(|| workspace.definition_by_suffix(symbol.path.as_str()))
                    .or_else(|| workspace.definition_by_name(&symbol.name));
                drop(workspace);

                if let Some(loc) = def_loc {
                    // Get the source file for the definition
                    let def_uri = to_miden_uri(&loc.uri);
                    if let Some(source) = self.sources.get_by_uri(&def_uri) {
                        // Extract signature and comments above the definition
                        let content = source.as_str();
                        let def_line = loc.range.start.line as usize;
                        let signature = extract_procedure_signature(content, def_line);
                        let comment = extract_doc_comment(content, def_line);

                        // Build hover content: signature code block + separator + doc comment
                        let hover_text = match (signature, comment) {
                            (Some(sig), Some(doc)) => {
                                format!("```masm\n{sig}\n```\n\n---\n\n{doc}")
                            }
                            (Some(sig), None) => format!("```masm\n{sig}\n```"),
                            (None, Some(doc)) => doc,
                            (None, None) => return Ok(None),
                        };

                        return Ok(Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: hover_text,
                            }),
                            range: None,
                        }));
                    }
                }
            }
            Err(e) => {
                debug!("hover: symbol resolution failed: {e}");
            }
        }

        // Fall back to instruction hover for other instructions (if enabled)
        if instruction_hovers_enabled {
            if let Some(t) = token {
                if let Some(hover_text) = get_instruction_hover(&t) {
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: None,
                    }));
                }
            }
        }

        Ok(None)
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        let res = self
            .goto_definition(GotoDefinitionParams {
                text_document_position_params: params.text_document_position_params.clone(),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await?;
        Ok(res)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let symbol = match resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos) {
            Ok(s) => s,
            Err(e) => {
                debug!("references: {e}");
                return Ok(None);
            }
        };

        let workspace = self.workspace.read().await;

        let mut results = workspace.references(symbol.path.as_str());
        if results.is_empty() {
            results.extend(workspace.references_by_suffix(symbol.path.as_str()));
        }

        // Always include the definition - users expect to see where a symbol is defined
        // alongside its usages. This matches common IDE behavior.
        if let Some(def) = workspace
            .definition(symbol.path.as_str())
            .or_else(|| workspace.definition_by_suffix(symbol.path.as_str()))
        {
            results.push(def);
        }

        if results.is_empty() {
            Ok(None)
        } else {
            Ok(Some(results))
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let workspace = self.workspace.read().await;
        let syms = workspace
            .workspace_symbols(&params.query)
            .into_iter()
            .map(|(name, loc)| {
                #[allow(deprecated)]
                {
                    SymbolInformation {
                        name,
                        location: loc,
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        container_name: None,
                        deprecated: None,
                    }
                }
            })
            .collect();
        Ok(Some(syms))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let miden_uri = to_miden_uri(&uri);
        let Some(source) = self.sources.get_by_uri(&miden_uri) else {
            return Ok(None);
        };

        let tab_count = self.snapshot_config().await.inlay_hint_tabs;
        let hints = collect_inlay_hints(
            &doc.module,
            self.sources.as_ref(),
            &params.range,
            tab_count,
            source.as_str(),
        );
        Ok(Some(hints))
    }
}

fn extract_tab_count(settings: &serde_json::Value) -> Option<usize> {
    // Expect settings like { "masm": { "inlayHint": { "tabPadding": 2 } } }
    settings
        .get("masm")
        .and_then(|v| v.get("inlayHint"))
        .and_then(|v| v.get("tabPadding"))
        .and_then(|v| v.as_u64())
        .and_then(|v| usize::try_from(v).ok())
        .filter(|v| *v > 0)
}

fn extract_library_paths(settings: &serde_json::Value) -> Option<Vec<LibraryPath>> {
    let arr = settings
        .get("masm")
        .and_then(|v| v.get("libraryPaths"))
        .and_then(|v| v.as_array())?;
    let mut out = Vec::new();
    for entry in arr {
        match entry {
            serde_json::Value::String(path) => {
                out.push(LibraryPath {
                    root: path.into(),
                    prefix: "std".to_string(),
                });
            }
            serde_json::Value::Object(map) => {
                let Some(path_val) = map.get("path").and_then(|v| v.as_str()) else {
                    continue;
                };
                let prefix = map.get("prefix").and_then(|v| v.as_str()).unwrap_or("std");
                out.push(LibraryPath {
                    root: path_val.into(),
                    prefix: prefix.to_string(),
                });
            }
            _ => {}
        }
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

/// Determine the appropriate module kind from the parsed AST.
///
/// This examines the module's structure to determine whether it should be:
/// - `Executable`: if it has an entrypoint (`begin..end` block)
/// - `Library`: otherwise (the default for modules without entrypoints)
///
/// Note: Kernel modules are not auto-detected since they require explicit
/// namespace configuration during parsing.
fn determine_module_kind_from_ast(
    module: &miden_assembly_syntax::ast::Module,
) -> miden_assembly_syntax::ast::ModuleKind {
    use miden_assembly_syntax::ast::ModuleKind;

    if module.has_entrypoint() {
        ModuleKind::Executable
    } else {
        ModuleKind::Library
    }
}

fn module_path_from_uri_fallback(uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let path = uri.path();
    let stem = Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())?;
    let mut buf = miden_assembly_syntax::ast::PathBuf::default();
    buf.push(stem);
    Some(buf)
}

fn build_path_from_root(
    path: &Path,
    root: &Path,
    prefix: &str,
) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let rel = path.strip_prefix(root).ok()?;
    let mut buf = miden_assembly_syntax::ast::PathBuf::default();
    if !prefix.is_empty() {
        buf.push(prefix);
    }
    let parts: Vec<_> = rel.iter().collect();
    for (i, comp) in parts.iter().enumerate() {
        let mut seg = comp.to_string_lossy();
        if i == parts.len().saturating_sub(1) {
            if let Some(stripped) = seg.split('.').next() {
                seg = std::borrow::Cow::Owned(stripped.to_string());
            }
        }
        if seg.is_empty() {
            continue;
        }
        buf.push(seg.as_ref());
    }
    Some(buf)
}

/// Check if the cursor position is on a `use` statement line.
fn is_on_use_statement(source: &str, pos: lsp_types::Position) -> bool {
    if let Some(line) = source.lines().nth(pos.line as usize) {
        let trimmed = line.trim_start();
        trimmed.starts_with("use ")
    } else {
        false
    }
}

/// Extract the procedure signature (attributes + definition line) at the given line.
/// Returns lines like "@locals(48)" and "export.double" as a multi-line string.
fn extract_procedure_signature(source: &str, def_line: usize) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let def = lines.get(def_line)?.trim();

    // The definition line should be the procedure declaration
    if !def.starts_with("proc")
        && !def.starts_with("pub")
        && !def.starts_with("export.")
        && !def.starts_with("begin")
        && !def.starts_with("const.")
    {
        return None;
    }

    let mut signature_lines = Vec::new();

    // Collect attribute lines above the definition (walking backwards)
    let mut line_idx = def_line.saturating_sub(1);
    while line_idx < lines.len() {
        let line = lines.get(line_idx).map(|l| l.trim()).unwrap_or("");
        if line.starts_with('@') {
            signature_lines.push(line.to_string());
        } else if line.is_empty() || line.starts_with("#!") || line.starts_with('#') {
            // Skip blank lines and comments, keep looking for attributes
            if !signature_lines.is_empty() {
                break; // Found attributes, stop at non-attribute
            }
        } else {
            break;
        }
        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    // Reverse attributes since we collected bottom-up
    signature_lines.reverse();
    // Add the definition line
    signature_lines.push(def.to_string());

    Some(signature_lines.join("\n"))
}

/// Extract doc comments above a definition at the given line.
/// Doc comments are lines starting with "#!" immediately before the definition.
/// Skips over attributes (lines starting with "@") between comments and definition.
/// Returns the comment text with "#!" prefixes stripped.
fn extract_doc_comment(source: &str, def_line: usize) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    if def_line == 0 || def_line > lines.len() {
        return None;
    }

    let mut comment_lines = Vec::new();
    let mut line_idx = def_line.saturating_sub(1);

    // Walk backwards collecting comment lines
    loop {
        let line = lines.get(line_idx)?;
        let trimmed = line.trim();
        if trimmed.starts_with("#!") {
            // Strip the "#!" and optional space
            let comment_text = trimmed.strip_prefix("#!").unwrap_or(trimmed);
            let comment_text = comment_text.strip_prefix(' ').unwrap_or(comment_text);
            comment_lines.push(comment_text.to_string());
        } else if trimmed.is_empty() {
            // Skip blank lines between comments and definition
            if !comment_lines.is_empty() {
                break;
            }
        } else if trimmed.starts_with('@') {
            // Skip attribute lines (e.g., @locals(48), @extern(foo))
            // Continue looking for doc comments above the attribute
        } else {
            // Non-comment, non-empty, non-attribute line - stop
            break;
        }

        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    if comment_lines.is_empty() {
        return None;
    }

    // Reverse since we collected bottom-up
    comment_lines.reverse();
    Some(comment_lines.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use tower_lsp::lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
        GotoDefinitionResponse, ReferenceContext, ReferenceParams, TextDocumentItem,
        TextDocumentPositionParams, VersionedTextDocumentIdentifier,
    };

    #[tokio::test]
    async fn open_valid_file_produces_no_diagnostics() {
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        let uri = Url::parse("file:///tmp/program.masm").unwrap();
        let text = "begin\n  push.1\nend\n".to_string();

        backend
            .handle_open(uri.clone(), 1, text)
            .await
            .expect("open");

        let published = client.take_published().await;
        assert_eq!(published.len(), 1);
        let (_, diags, version) = &published[0];
        assert!(
            diags.is_empty(),
            "expected no diagnostics, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
        assert_eq!(*version, Some(1));
    }

    #[tokio::test]
    async fn change_to_invalid_source_emits_diagnostics() {
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        let uri = Url::parse("file:///tmp/program.masm").unwrap();

        let open_params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "masm".into(),
                version: 1,
                text: "begin\n  push.1\nend\n".into(),
            },
        };
        backend.did_open(open_params).await;

        let change_params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "this is not valid masm".into(),
            }],
        };
        backend.did_change(change_params).await;

        let published = client.take_published().await;
        assert!(
            published.len() >= 2,
            "expected at least two publish calls, got {}",
            published.len()
        );
        let (_, diags_after_change, version) = published.last().unwrap();
        assert!(!diags_after_change.is_empty());
        assert_eq!(*version, Some(2));
    }

    #[tokio::test]
    async fn goto_definition_resolves_stdlib_sha256() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let stdlib_root = manifest_dir
            .join(".codex")
            .join("miden-vm")
            .join("stdlib")
            .join("asm");
        let example_path = manifest_dir
            .join(".codex")
            .join("miden-vm")
            .join("miden-vm")
            .join("masm-examples")
            .join("hashing")
            .join("sha256")
            .join("sha256.masm");

        if !stdlib_root.is_dir() || !example_path.is_file() {
            // Skip when test data is unavailable in the sandbox.
            return;
        }

        let mut cfg = ServerConfig::default();
        cfg.library_paths = vec![LibraryPath {
            root: stdlib_root.clone(),
            prefix: "std".to_string(),
        }];

        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        backend.update_config(cfg).await;
        backend.load_configured_libraries().await;

        let text = std::fs::read_to_string(&example_path).unwrap();
        let uri = Url::from_file_path(&example_path).unwrap();
        let open_params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "masm".into(),
                version: 1,
                text: text.clone(),
            },
        };
        backend.did_open(open_params).await;

        let pos_hash = position_of(&text, "exec.sha256::hash_2to1");
        let def = backend
            .goto_definition(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: pos_hash,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .expect("goto def");

        let Some(GotoDefinitionResponse::Scalar(loc)) = def else {
            panic!("expected scalar goto definition");
        };
        let expected_uri = Url::from_file_path(
            stdlib_root
                .join("crypto")
                .join("hashes")
                .join("sha256.masm"),
        )
        .unwrap();
        assert_eq!(loc.uri, expected_uri);
        assert_eq!(
            loc.range.start.line,
            position_of_stdlib(&expected_uri, "pub proc hash_2to1").line
        );

        // Cursor on the module name should still resolve to the module file.
        let pos_module = position_of(&text, "sha256::");
        let def_module = backend
            .goto_definition(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: pos_module,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .await
            .expect("goto def module");
        let Some(GotoDefinitionResponse::Scalar(module_loc)) = def_module else {
            panic!("expected scalar goto definition");
        };
        assert_eq!(module_loc.uri, expected_uri);
    }

    #[tokio::test]
    async fn references_across_files_include_definition_and_callsite() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let stdlib_root = manifest_dir
            .join(".codex")
            .join("miden-vm")
            .join("stdlib")
            .join("asm");
        let example_path = manifest_dir
            .join(".codex")
            .join("miden-vm")
            .join("miden-vm")
            .join("masm-examples")
            .join("hashing")
            .join("sha256")
            .join("sha256.masm");

        if !stdlib_root.is_dir() || !example_path.is_file() {
            return;
        }

        let mut cfg = ServerConfig::default();
        cfg.library_paths = vec![LibraryPath {
            root: stdlib_root.clone(),
            prefix: "std".to_string(),
        }];

        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        backend.update_config(cfg).await;
        backend.load_configured_libraries().await;

        let text = std::fs::read_to_string(&example_path).unwrap();
        let uri = Url::from_file_path(&example_path).unwrap();
        let open_params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "masm".into(),
                version: 1,
                text: text.clone(),
            },
        };
        backend.did_open(open_params).await;

        let pos_hash = position_of(&text, "exec.sha256::hash_2to1");
        let refs = backend
            .references(ReferenceParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: pos_hash,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .await
            .expect("references");
        let Some(list) = refs else {
            panic!("expected references");
        };
        eprintln!("refs debug: {:?}", list);
        let expected_def_uri = Url::from_file_path(
            stdlib_root
                .join("crypto")
                .join("hashes")
                .join("sha256.masm"),
        )
        .unwrap();

        assert!(
            list.iter().any(|loc| loc.uri == expected_def_uri),
            "should include definition in stdlib"
        );
        assert!(
            list.iter().any(|loc| loc.uri == uri),
            "should include call site"
        );
        assert!(
            list.len() >= 2,
            "expected at least callsite and definition, got {}",
            list.len()
        );
    }

    #[tokio::test]
    async fn references_include_multiple_calls_in_same_file() {
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        let uri = Url::parse("file:///tmp/multi_refs.masm").unwrap();
        let text = r#"proc f
    nop
end

proc g
    exec.f
    exec.f
end
"#
        .to_string();

        backend
            .handle_open(uri.clone(), 1, text.clone())
            .await
            .expect("open");

        let pos_call = position_of(&text, "exec.f");
        let refs = backend
            .references(ReferenceParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: pos_call,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .await
            .expect("references");
        let Some(list) = refs else {
            panic!("expected references");
        };
        let calls = list.iter().filter(|loc| loc.uri == uri).count();
        assert!(
            calls >= 2,
            "expected at least two callsites in same file, got {}",
            calls
        );
    }

    #[tokio::test]
    async fn unresolved_invocation_reports_diagnostic_after_change() {
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());
        let uri = Url::parse("file:///tmp/unresolved.masm").unwrap();
        let valid = r#"proc foo
    nop
end

proc g
    exec.foo
end
"#
        .to_string();

        backend
            .handle_open(uri.clone(), 1, valid.clone())
            .await
            .expect("open");
        let _ = client.take_published().await;

        let invalid = r#"proc bar
    nop
end

proc g
    exec.foo
end
"#
        .to_string();

        backend
            .did_change(DidChangeTextDocumentParams {
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

        // When parsing fails due to undefined symbols, diagnostics are generated from
        // the parse error rather than from doc.unresolved. The MASM parser validates
        // symbol references during parsing, so undefined local symbols cause parse errors.
        let diags = backend.publish_diagnostics(uri.clone()).await;
        assert!(
            !diags.is_empty(),
            "expected diagnostics for undefined symbol"
        );
        // The diagnostic should indicate a syntax/semantic error
        let has_error_diag = diags.iter().any(|d| {
            d.message.contains("syntax error")
                || d.message.contains("undefined")
                || d.message.contains("foo")
        });
        assert!(
            has_error_diag,
            "expected error diagnostic, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn determine_module_kind_detects_executable_from_entrypoint() {
        use miden_assembly_syntax::ast::ModuleKind;

        // Parse a module with an entrypoint (begin..end block)
        // The function should detect it as Executable based on has_entrypoint()
        let client = RecordingClient::default();
        let backend = Backend::new(client.clone());

        // Module with begin..end should be detected as Executable
        let exec_uri = Url::parse("file:///tmp/exec_test.masm").unwrap();
        let exec_text = "begin\n  push.1\nend\n".to_string();
        backend
            .handle_open(exec_uri.clone(), 1, exec_text)
            .await
            .expect("open");

        let exec_doc = backend.snapshot_document_symbols(&exec_uri).await.unwrap();
        assert_eq!(
            determine_module_kind_from_ast(&exec_doc.module),
            ModuleKind::Executable,
            "module with begin..end should be detected as Executable"
        );

        // Module without begin..end should be detected as Library
        let lib_uri = Url::parse("file:///tmp/lib_test.masm").unwrap();
        let lib_text = "proc foo\n  nop\nend\n".to_string();
        backend
            .handle_open(lib_uri.clone(), 1, lib_text)
            .await
            .expect("open");

        let lib_doc = backend.snapshot_document_symbols(&lib_uri).await.unwrap();
        assert_eq!(
            determine_module_kind_from_ast(&lib_doc.module),
            ModuleKind::Library,
            "module without begin..end should be detected as Library"
        );
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

    fn position_of(text: &str, needle: &str) -> lsp_types::Position {
        for (i, line) in text.lines().enumerate() {
            if let Some(col) = line.find(needle) {
                return lsp_types::Position::new(i as u32, col as u32);
            }
        }
        panic!("needle not found");
    }

    fn position_of_stdlib(uri: &Url, needle: &str) -> lsp_types::Position {
        let path = uri.to_file_path().unwrap();
        let text = std::fs::read_to_string(path).unwrap();
        position_of(&text, needle)
    }

    #[test]
    fn extract_doc_comment_single_line() {
        let source = "#! This is a comment\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 1);
        assert_eq!(comment, Some("This is a comment".to_string()));
    }

    #[test]
    fn extract_doc_comment_multi_line() {
        let source = "#! Line one\n#! Line two\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Line one\nLine two".to_string()));
    }

    #[test]
    fn extract_doc_comment_no_comment() {
        let source = "proc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 0);
        assert_eq!(comment, None);
    }

    #[test]
    fn extract_doc_comment_with_blank_line() {
        let source = "#! Comment\n\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Comment".to_string()));
    }

    #[test]
    fn extract_doc_comment_ignores_regular_comments() {
        // Regular comments starting with just "#" should be ignored
        let source = "# Regular comment\nproc foo\n  nop\nend\n";
        let comment = extract_doc_comment(source, 1);
        assert_eq!(comment, None);
    }

    #[test]
    fn extract_doc_comment_skips_attributes() {
        // Doc comments should be found even with attributes between comment and definition
        let source = "#! Comment here!\n@locals(48)\nexport.double\n";
        let comment = extract_doc_comment(source, 2);
        assert_eq!(comment, Some("Comment here!".to_string()));
    }

    #[test]
    fn extract_doc_comment_skips_multiple_attributes() {
        // Multiple attributes should all be skipped
        let source = "#! Multi-line\n#! doc comment\n@locals(16)\n@extern(foo)\nproc bar\nend\n";
        let comment = extract_doc_comment(source, 4);
        assert_eq!(comment, Some("Multi-line\ndoc comment".to_string()));
    }

    #[test]
    fn extract_signature_simple_proc() {
        let source = "proc foo\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("proc foo".to_string()));
    }

    #[test]
    fn extract_signature_export() {
        let source = "export.double\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("export.double".to_string()));
    }

    #[test]
    fn extract_signature_with_locals() {
        let source = "@locals(48)\nexport.double\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 1);
        assert_eq!(sig, Some("@locals(48)\nexport.double".to_string()));
    }

    #[test]
    fn extract_signature_with_multiple_attributes() {
        let source = "@locals(16)\n@extern(foo)\nproc bar\nend\n";
        let sig = extract_procedure_signature(source, 2);
        assert_eq!(sig, Some("@locals(16)\n@extern(foo)\nproc bar".to_string()));
    }

    #[test]
    fn extract_signature_with_comment_above() {
        let source = "#! Doc comment\n@locals(48)\nexport.double\n";
        let sig = extract_procedure_signature(source, 2);
        assert_eq!(sig, Some("@locals(48)\nexport.double".to_string()));
    }

    #[test]
    fn extract_signature_pub_proc() {
        let source = "pub proc foo\n  nop\nend\n";
        let sig = extract_procedure_signature(source, 0);
        assert_eq!(sig, Some("pub proc foo".to_string()));
    }

    #[test]
    fn extract_signature_pub_proc_with_locals() {
        let source = "@locals(16)\npub proc bar\nend\n";
        let sig = extract_procedure_signature(source, 1);
        assert_eq!(sig, Some("@locals(16)\npub proc bar".to_string()));
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
}
