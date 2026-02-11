use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

use masm_analysis::signature_mismatches;
use masm_decompiler::{
    Decompiler,
    frontend::{LibraryRoot, Program, Workspace},
    signature::ProcSignature,
};

use miden_assembly_syntax::{Parse, ParseOptions, SemanticAnalysisError};
use miden_assembly_syntax::ast::Module;
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager};
use miden_utils_diagnostics as diagnostics;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, TextDocumentContentChangeEvent, Url,
};
use tracing::error;

use crate::{
    client::PublishDiagnostics,
    cursor_resolution::{resolve_symbol_at_position, ResolutionError, ResolvedSymbol},
    diagnostics::{SOURCE_ANALYSIS, diagnostics_from_report, normalize_message, span_to_range, unresolved_to_diagnostics},
    index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    module_path::ModulePathResolver,
    service::DocumentService,
    util::{lsp_range_to_selection, to_miden_uri},
    LibraryPath, ServerConfig, SymbolPath,
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
    pub(crate) signature_cache: Arc<SignatureCache>,
}

impl<C> Backend<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            sources: Arc::new(DefaultSourceManager::default()),
            documents: DocumentCache::new(),
            workspace: Arc::new(RwLock::new(WorkspaceIndex::default())),
            config: Arc::new(RwLock::new(ServerConfig::default())),
            signature_cache: Arc::new(SignatureCache::new()),
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

    pub(crate) async fn invalidate_signature_cache(&self) {
        self.signature_cache.invalidate().await;
    }

    pub(crate) async fn inferred_signature_line(
        &self,
        module: &Module,
        uri: &Url,
        symbol_path: &SymbolPath,
        library_paths: &[LibraryPath],
    ) -> Option<String> {
        if let Some(value) = self.signature_cache.get(symbol_path).await {
            return value;
        }
        let generation = self.signature_cache.generation();
        let inferred = infer_signature_line(
            module,
            self.sources.clone(),
            uri,
            symbol_path,
            library_paths,
        );
        self.signature_cache
            .insert_if_fresh(symbol_path.clone(), inferred.clone(), generation)
            .await;
        inferred
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
        self.invalidate_signature_cache().await;
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
        self.invalidate_signature_cache().await;
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
        self.publish_diagnostics_inner(uri, Vec::new()).await
    }

    pub async fn publish_diagnostics_with_extra(
        &self,
        uri: Url,
        extra: Vec<Diagnostic>,
    ) -> Vec<Diagnostic> {
        self.publish_diagnostics_inner(uri, extra).await
    }

    async fn publish_diagnostics_inner(
        &self,
        uri: Url,
        extra: Vec<Diagnostic>,
    ) -> Vec<Diagnostic> {
        let version = self.documents.get_version(&uri).await;
        let parse_result = self.parse_and_index(&uri).await;
        let config = self.snapshot_config().await;

        let analysis_diags = if let Ok(doc) = &parse_result {
            if config.taint_analysis_enabled {
                let source_path =
                    uri.to_file_path().unwrap_or_else(|_| PathBuf::from("in-memory.masm"));
                let roots: Vec<LibraryRoot> = config
                    .library_paths
                    .iter()
                    .map(|lib| LibraryRoot::new(lib.prefix.clone(), lib.root.clone()))
                    .collect();
                let mismatches = signature_mismatches(
                    &doc.module,
                    self.sources.clone(),
                    source_path,
                    &roots,
                );
                signature_mismatch_diagnostics(mismatches, self.sources.as_ref())
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        let fallback_doc = if parse_result.is_err() {
            self.documents.get_symbols(&uri).await
        } else {
            None
        };

        let workspace = self.workspace.read().await;
        let mut diagnostics = match parse_result {
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

        if !analysis_diags.is_empty() {
            diagnostics.extend(analysis_diags);
        }

        if !extra.is_empty() {
            diagnostics.extend(extra);
        }

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
            .parse_with_options(self.sources.clone(), opts)
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
            .parse_with_options(self.sources.clone(), opts)
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
        let doc_symbols = build_document_symbols(module, self.sources.clone());

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
        resolve_symbol_at_position(uri, &doc.module, self.sources.clone(), position)
    }
}

fn infer_signature_line(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    uri: &Url,
    symbol_path: &SymbolPath,
    library_paths: &[LibraryPath],
) -> Option<String> {
    let source_path = uri
        .to_file_path()
        .unwrap_or_else(|_| std::path::PathBuf::from("in-memory.masm"));
    let roots: Vec<LibraryRoot> = library_paths
        .iter()
        .map(|lib| LibraryRoot::new(lib.prefix.clone(), lib.root.clone()))
        .collect();

    let mut workspace = Workspace::with_source_manager(roots, sources);
    let program = Program::from_parts(
        Box::new(module.clone()),
        source_path,
        module.path().to_path_buf(),
    );
    workspace.add_program(program);
    workspace.load_dependencies();

    let decompiler = Decompiler::new(&workspace);
    let signature = decompiler.signatures().get(symbol_path)?;
    format_inferred_signature(symbol_path.name(), signature)
}

fn format_inferred_signature(name: &str, signature: &ProcSignature) -> Option<String> {
    match signature {
        ProcSignature::Known { inputs, outputs, .. } => {
            let args = (0..*inputs)
                .map(|idx| format!("v_{idx}"))
                .collect::<Vec<_>>()
                .join(", ");
            let ret = match *outputs {
                0 => String::new(),
                1 => " -> Felt".to_string(),
                n => {
                    let types = (0..n)
                        .map(|_| "Felt")
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!(" -> ({types})")
                }
            };
            Some(format!("proc {name}({args}){ret}"))
        }
        ProcSignature::Unknown => None,
    }
}

fn signature_mismatch_diagnostics(
    mismatches: Vec<masm_analysis::SignatureMismatch>,
    sources: &DefaultSourceManager,
) -> Vec<Diagnostic> {
    mismatches
        .into_iter()
        .map(|mismatch| {
            let range = span_to_range(sources, mismatch.span).unwrap_or_else(|| {
                Range::new(Position::new(0, 0), Position::new(0, 0))
            });
            let message = normalize_message(&signature_mismatch_message(&mismatch));
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some(SOURCE_ANALYSIS.to_string()),
                message,
                ..Default::default()
            }
        })
        .collect()
}

fn signature_mismatch_message(mismatch: &masm_analysis::SignatureMismatch) -> String {
    let inputs_diff = mismatch.declared.inputs != mismatch.inferred.inputs;
    let outputs_diff = mismatch.declared.outputs != mismatch.inferred.outputs;
    match (inputs_diff, outputs_diff) {
        (true, true) => format!(
            "the definition declares {} inputs and {} outputs, but the inferred counts are {} and {} respectively",
            mismatch.declared.inputs,
            mismatch.declared.outputs,
            mismatch.inferred.inputs,
            mismatch.inferred.outputs
        ),
        (true, false) => format!(
            "the definition declares {} inputs, but the inferred input count is {}",
            mismatch.declared.inputs,
            mismatch.inferred.inputs
        ),
        (false, true) => format!(
            "the definition declares {} outputs, but the inferred output count is {}",
            mismatch.declared.outputs,
            mismatch.inferred.outputs
        ),
        (false, false) => String::new(),
    }
}

#[derive(Debug)]
pub(crate) struct SignatureCache {
    generation: AtomicU64,
    entries: RwLock<HashMap<SymbolPath, Option<String>>>,
}

impl SignatureCache {
    pub(crate) fn new() -> Self {
        Self {
            generation: AtomicU64::new(0),
            entries: RwLock::new(HashMap::new()),
        }
    }

    pub(crate) fn generation(&self) -> u64 {
        self.generation.load(Ordering::Relaxed)
    }

    pub(crate) async fn get(&self, path: &SymbolPath) -> Option<Option<String>> {
        let entries = self.entries.read().await;
        entries.get(path).cloned()
    }

    pub(crate) async fn insert_if_fresh(
        &self,
        path: SymbolPath,
        value: Option<String>,
        generation: u64,
    ) {
        if self.generation.load(Ordering::Relaxed) != generation {
            return;
        }
        let mut entries = self.entries.write().await;
        if self.generation.load(Ordering::Relaxed) != generation {
            return;
        }
        entries.insert(path, value);
    }

    pub(crate) async fn invalidate(&self) {
        self.generation.fetch_add(1, Ordering::Relaxed);
        let mut entries = self.entries.write().await;
        entries.clear();
    }
}
