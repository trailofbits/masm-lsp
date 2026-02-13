use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};

use masm_analysis::signature_mismatches;
use masm_decompiler::{
    callgraph::CallGraph,
    fmt::{CodeWriter, FormattingConfig},
    frontend::{LibraryRoot, Program, Workspace},
    signature::{infer_signatures, ProcSignature, SignatureMap},
    symbol::resolution::{resolve_constant_path, resolve_constant_symbol},
    types::{
        infer_type_summaries, InferredType, TypeDiagnosticsMap, TypeRequirement, TypeSummary,
        TypeSummaryMap,
    },
    Decompiler,
};

use miden_assembly_syntax::ast::{Module, Procedure};
use miden_assembly_syntax::{Parse, ParseOptions, SemanticAnalysisError};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Spanned};
use miden_utils_diagnostics as diagnostics;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range,
    TextDocumentContentChangeEvent, Url,
};
use tracing::error;

use crate::{
    client::PublishDiagnostics,
    cursor_resolution::{
        find_constant_candidate_at_position, position_to_offset, resolve_symbol_at_position,
        ResolutionError, ResolvedSymbol,
    },
    diagnostics::{
        diagnostics_from_report, normalize_message, span_to_range, unresolved_to_diagnostics,
        SOURCE_ANALYSIS, SOURCE_DECOMPILATION,
    },
    index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    inlay_hints::decompilation_error_diagnostic,
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
    pub(crate) workspace_library_paths: Arc<RwLock<Vec<LibraryPath>>>,
    pub(crate) signature_cache: Arc<SignatureCache>,
    pub(crate) loaded_library_documents: Arc<RwLock<HashSet<Url>>>,
}

#[derive(Debug, Clone)]
pub(crate) struct ProcedureDecompilation {
    pub(crate) name: String,
    pub(crate) path: String,
    pub(crate) range: Range,
    pub(crate) decompiled: String,
}

#[derive(Debug, Clone)]
pub(crate) struct FileDecompilation {
    pub(crate) module_path: String,
    pub(crate) use_statements: Vec<String>,
    pub(crate) procedures: Vec<ProcedureDecompilation>,
    pub(crate) failures: Vec<ProcedureDecompilationFailure>,
}

#[derive(Debug, Clone)]
pub(crate) struct ProcedureDecompilationFailure {
    pub(crate) name: String,
    pub(crate) path: String,
    pub(crate) range: Range,
    pub(crate) code: String,
    pub(crate) message: String,
}

#[derive(Debug, Clone)]
pub(crate) enum ProcedureDecompilationError {
    DocumentUnavailable(String),
    InvalidCursorPosition {
        line: u32,
        character: u32,
    },
    CursorOutsideProcedure {
        diagnostic: Diagnostic,
    },
    DecompilationFailed {
        message: String,
        diagnostic: Diagnostic,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum FileDecompilationError {
    DocumentUnavailable(String),
}

impl<C> Backend<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            sources: Arc::new(DefaultSourceManager::default()),
            documents: DocumentCache::new(),
            workspace: Arc::new(RwLock::new(WorkspaceIndex::default())),
            config: Arc::new(RwLock::new(ServerConfig::default())),
            workspace_library_paths: Arc::new(RwLock::new(Vec::new())),
            signature_cache: Arc::new(SignatureCache::new()),
            loaded_library_documents: Arc::new(RwLock::new(HashSet::new())),
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

    pub(crate) async fn set_workspace_library_paths(&self, paths: Vec<LibraryPath>) {
        let mut guard = self.workspace_library_paths.write().await;
        *guard = deduplicate_library_paths(paths);
    }

    pub(crate) async fn effective_library_paths(&self) -> Vec<LibraryPath> {
        let cfg = self.snapshot_config().await;
        self.effective_library_paths_from(&cfg.library_paths).await
    }

    pub(crate) async fn effective_library_paths_from(
        &self,
        configured: &[LibraryPath],
    ) -> Vec<LibraryPath> {
        let workspace_paths = self.workspace_library_paths.read().await.clone();
        let mut combined = configured.to_vec();
        combined.extend(workspace_paths);
        deduplicate_library_paths(combined)
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

    pub(crate) async fn resolve_constant_symbol_at_position(
        &self,
        uri: &Url,
        module: &Module,
        position: Position,
        library_paths: &[LibraryPath],
    ) -> Result<ResolvedSymbol, ResolutionError> {
        let candidate =
            find_constant_candidate_at_position(uri, module, self.sources.clone(), position)?;
        let display_name = candidate
            .rsplit("::")
            .next()
            .unwrap_or(candidate.as_str())
            .to_string();

        let roots = build_library_roots(library_paths);
        let source_path = uri
            .to_file_path()
            .unwrap_or_else(|_| std::path::PathBuf::from("in-memory.masm"));
        let mut workspace = Workspace::with_source_manager(roots, self.sources.clone());
        let program = Program::from_parts(
            Box::new(module.clone()),
            source_path,
            module.path().to_path_buf(),
        );
        workspace.add_program(program);
        workspace.load_dependencies();

        let module_path = SymbolPath::new(module.path().to_string());
        let resolved = if candidate.contains("::") {
            resolve_constant_path(&workspace, &module_path, candidate.as_str())
        } else {
            resolve_constant_symbol(&workspace, &module_path, candidate.as_str())
        };

        if let Ok(path) = resolved {
            // Mirror masm-decompiler's constant lookup flow:
            // 1) resolve constant symbol/path
            // 2) fetch constant definition entry from the workspace
            if workspace.lookup_constant_entry(&path).is_none() {
                return Err(ResolutionError::SymbolNotFound(candidate));
            }
            return Ok(ResolvedSymbol {
                path,
                name: display_name,
            });
        }

        // Fallback for module contexts that are not loaded in the temporary decompiler workspace.
        // We still resolve via the module-local resolver so open in-memory files remain navigable.
        let resolver = crate::symbol_resolution::create_resolver(module, self.sources.clone());
        let fallback = if candidate.contains("::") {
            resolver.resolve_path(candidate.as_str())
        } else {
            resolver.resolve_symbol(candidate.as_str())
        };
        match fallback {
            Ok(path) => Ok(ResolvedSymbol {
                path,
                name: display_name,
            }),
            Err(_) => Err(ResolutionError::SymbolNotFound(candidate)),
        }
    }
}

impl<C> Backend<C>
where
    C: PublishDiagnostics,
{
    pub async fn load_configured_libraries(&self) {
        self.invalidate_signature_cache().await;
        self.clear_loaded_libraries().await;
        let libraries = self.effective_library_paths().await;
        for lib in &libraries {
            if let Err(err) = self.load_library_root(lib).await {
                error!("failed to load library root {}: {err}", lib.root.display());
            }
        }
    }

    pub(crate) async fn decompile_procedure_at_position(
        &self,
        uri: &Url,
        position: Position,
        library_paths: &[LibraryPath],
    ) -> Result<ProcedureDecompilation, ProcedureDecompilationError> {
        let Some(doc) = self.get_or_parse_document(uri).await else {
            return Err(ProcedureDecompilationError::DocumentUnavailable(format!(
                "document not loaded or failed to parse: {uri}"
            )));
        };

        let Some(source) = self.sources.get_by_uri(&to_miden_uri(uri)) else {
            return Err(ProcedureDecompilationError::DocumentUnavailable(format!(
                "source file not found for URI: {uri}"
            )));
        };

        let Some(offset) = position_to_offset(source.as_ref(), position) else {
            return Err(ProcedureDecompilationError::InvalidCursorPosition {
                line: position.line,
                character: position.character,
            });
        };

        let (proc_name, proc_range) = {
            let Some(procedure) = find_procedure_containing_offset(&doc.module, offset) else {
                return Err(ProcedureDecompilationError::CursorOutsideProcedure {
                    diagnostic: cursor_outside_procedure_diagnostic(position),
                });
            };

            let name = procedure.name().as_str().to_string();
            let range = span_to_range(self.sources.as_ref(), procedure.span())
                .or_else(|| span_to_range(self.sources.as_ref(), procedure.name().span()))
                .unwrap_or_else(|| Range::new(position, position));

            (name, range)
        };

        let module = doc.module.as_ref().clone();
        let module_path = module.path().to_string();
        let source_path = uri
            .to_file_path()
            .unwrap_or_else(|_| PathBuf::from("in-memory.masm"));
        let roots = build_library_roots(library_paths);
        let mut workspace = Workspace::with_source_manager(roots, self.sources.clone());
        let program = Program::from_parts(
            Box::new(module.clone()),
            source_path,
            module.path().to_path_buf(),
        );
        workspace.add_program(program);
        workspace.load_dependencies();

        let fq_name = if module_path.is_empty() {
            proc_name.clone()
        } else {
            format!("{module_path}::{proc_name}")
        };

        let decompiler = Decompiler::new(&workspace);
        let decompiled = match decompiler.decompile_proc(&fq_name) {
            Ok(value) => value,
            Err(error) => {
                let diagnostic = decompilation_error_diagnostic(
                    self.sources.as_ref(),
                    proc_range.clone(),
                    &proc_name,
                    error,
                )
                .unwrap_or_else(|| Diagnostic {
                    range: proc_range.clone(),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(SOURCE_DECOMPILATION.to_string()),
                    code: Some(NumberOrString::String("decompilation-failed".to_string())),
                    message: normalize_message(&format!(
                        "could not decompile procedure `{proc_name}`"
                    )),
                    ..Default::default()
                });
                return Err(ProcedureDecompilationError::DecompilationFailed {
                    message: format!("failed to decompile procedure `{fq_name}`"),
                    diagnostic,
                });
            }
        };

        let mut writer = CodeWriter::with_config(FormattingConfig::default().with_color(false));
        writer.write(&decompiled);

        Ok(ProcedureDecompilation {
            name: proc_name,
            path: fq_name,
            range: proc_range,
            decompiled: writer.finish(),
        })
    }

    pub(crate) async fn decompile_file(
        &self,
        uri: &Url,
        library_paths: &[LibraryPath],
    ) -> Result<FileDecompilation, FileDecompilationError> {
        let Some(doc) = self.get_or_parse_document(uri).await else {
            return Err(FileDecompilationError::DocumentUnavailable(format!(
                "document not loaded or failed to parse: {uri}"
            )));
        };

        let module = doc.module.as_ref().clone();
        let module_path = module.path().to_string();
        let use_statements = self
            .sources
            .get_by_uri(&to_miden_uri(uri))
            .map(|source| collect_use_statements(source.as_str()))
            .unwrap_or_default();
        let source_path = uri
            .to_file_path()
            .unwrap_or_else(|_| PathBuf::from("in-memory.masm"));
        let roots = build_library_roots(library_paths);
        let mut workspace = Workspace::with_source_manager(roots, self.sources.clone());
        let program = Program::from_parts(
            Box::new(module.clone()),
            source_path,
            module.path().to_path_buf(),
        );
        workspace.add_program(program);
        workspace.load_dependencies();
        let decompiler = Decompiler::new(&workspace);

        let mut procedures = Vec::new();
        let mut failures = Vec::new();
        for proc in module.procedures() {
            let proc_name = proc.name().as_str().to_string();
            let proc_range = span_to_range(self.sources.as_ref(), proc.span())
                .or_else(|| span_to_range(self.sources.as_ref(), proc.name().span()))
                .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
            let fq_name = if module_path.is_empty() {
                proc_name.clone()
            } else {
                format!("{module_path}::{proc_name}")
            };

            let decompiled = match decompiler.decompile_proc(&fq_name) {
                Ok(value) => value,
                Err(error) => {
                    let diagnostic = decompilation_error_diagnostic(
                        self.sources.as_ref(),
                        proc_range.clone(),
                        &proc_name,
                        error,
                    )
                    .unwrap_or_else(|| Diagnostic {
                        range: proc_range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        source: Some(SOURCE_DECOMPILATION.to_string()),
                        code: Some(NumberOrString::String("decompilation-failed".to_string())),
                        message: normalize_message(&format!(
                            "could not decompile procedure `{proc_name}`"
                        )),
                        ..Default::default()
                    });
                    failures.push(ProcedureDecompilationFailure {
                        name: proc_name,
                        path: fq_name,
                        range: diagnostic.range,
                        code: diagnostic_code(&diagnostic),
                        message: diagnostic.message,
                    });
                    continue;
                }
            };

            let mut writer = CodeWriter::with_config(FormattingConfig::default().with_color(false));
            writer.write(&decompiled);
            procedures.push(ProcedureDecompilation {
                name: proc_name,
                path: fq_name,
                range: proc_range,
                decompiled: writer.finish(),
            });
        }

        Ok(FileDecompilation {
            module_path,
            use_statements,
            procedures,
            failures,
        })
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

    async fn publish_diagnostics_inner(&self, uri: Url, extra: Vec<Diagnostic>) -> Vec<Diagnostic> {
        let version = self.documents.get_version(&uri).await;
        let parse_result = self.parse_and_index(&uri).await;
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;

        let analysis_diags = if let Ok(doc) = &parse_result {
            if config.taint_analysis_enabled {
                let analysis = infer_analysis_snapshot(
                    &doc.module,
                    self.sources.clone(),
                    &uri,
                    &effective_library_paths,
                );
                let mut diags = Vec::new();
                if analysis.unresolved_modules.is_empty() {
                    let source_path = uri
                        .to_file_path()
                        .unwrap_or_else(|_| PathBuf::from("in-memory.masm"));
                    let roots = build_library_roots(&effective_library_paths);
                    let mismatches = signature_mismatches(
                        &doc.module,
                        self.sources.clone(),
                        source_path,
                        &roots,
                    );
                    diags.extend(signature_mismatch_diagnostics(
                        mismatches,
                        self.sources.as_ref(),
                    ));
                    diags.extend(type_inconsistency_diagnostics(
                        &uri,
                        &analysis.type_diagnostics,
                        self.sources.as_ref(),
                    ));
                } else {
                    diags.push(unresolved_dependency_diagnostic(
                        &doc.module,
                        self.sources.as_ref(),
                        &analysis.unresolved_modules,
                    ));
                }
                diags
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
        let paths = self.effective_library_paths().await;
        let resolver = ModulePathResolver::new(&paths);
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
        self.loaded_library_documents.write().await.insert(url);
        Ok(())
    }

    async fn clear_loaded_libraries(&self) {
        let uris: Vec<Url> = {
            let mut guard = self.loaded_library_documents.write().await;
            guard.drain().collect()
        };
        if uris.is_empty() {
            return;
        }

        let mut removable = Vec::new();
        for uri in uris {
            if self.documents.get_version(&uri).await.is_some() {
                // Keep currently-open documents in the workspace cache.
                continue;
            }
            removable.push(uri);
        }
        if removable.is_empty() {
            return;
        }

        {
            let mut ws = self.workspace.write().await;
            for uri in &removable {
                ws.update_document(uri.clone(), &[], &[]);
            }
        }

        for uri in removable {
            self.documents.remove_symbols(&uri).await;
            self.documents.remove(&uri).await;
        }
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
    let analysis = infer_analysis_snapshot(module, sources, uri, library_paths);
    if !analysis.unresolved_modules.is_empty() {
        return None;
    }
    let signature = analysis.signatures.get(symbol_path)?;
    let summary = analysis.type_summaries.get(symbol_path);
    format_inferred_signature(symbol_path.name(), signature, summary)
}

fn build_library_roots(library_paths: &[LibraryPath]) -> Vec<LibraryRoot> {
    library_paths
        .iter()
        .map(|lib| LibraryRoot::new(lib.prefix.clone(), lib.root.clone()))
        .collect()
}

#[derive(Debug)]
struct AnalysisSnapshot {
    signatures: SignatureMap,
    type_summaries: TypeSummaryMap,
    type_diagnostics: TypeDiagnosticsMap,
    unresolved_modules: Vec<SymbolPath>,
}

fn infer_analysis_snapshot(
    module: &Module,
    sources: Arc<DefaultSourceManager>,
    uri: &Url,
    library_paths: &[LibraryPath],
) -> AnalysisSnapshot {
    let source_path = uri
        .to_file_path()
        .unwrap_or_else(|_| std::path::PathBuf::from("in-memory.masm"));
    let roots = build_library_roots(library_paths);

    let mut workspace = Workspace::with_source_manager(roots, sources);
    let program = Program::from_parts(
        Box::new(module.clone()),
        source_path,
        module.path().to_path_buf(),
    );
    workspace.add_program(program);
    workspace.load_dependencies();
    let unresolved_modules = workspace.unresolved_module_paths();

    let callgraph = CallGraph::from(&workspace);
    let signatures = infer_signatures(&workspace, &callgraph);
    let (type_summaries, type_diagnostics) =
        infer_type_summaries(&workspace, &callgraph, &signatures);

    AnalysisSnapshot {
        signatures,
        type_summaries,
        type_diagnostics,
        unresolved_modules,
    }
}

fn format_inferred_signature(
    name: &str,
    signature: &ProcSignature,
    summary: Option<&TypeSummary>,
) -> Option<String> {
    match signature {
        ProcSignature::Known {
            inputs, outputs, ..
        } => {
            let input_types = summary
                .map(|summary| normalized_input_types(&summary.inputs, *inputs))
                .unwrap_or_else(|| vec![TypeRequirement::Unknown; *inputs]);

            let output_types = summary
                .map(|summary| normalized_output_types(&summary.outputs, *outputs))
                .unwrap_or_else(|| vec![InferredType::Unknown; *outputs]);

            let args = (0..*inputs)
                .map(|idx| {
                    let ty = input_types
                        .get(idx)
                        .copied()
                        .unwrap_or(TypeRequirement::Unknown);
                    format!("v_{idx}: {}", type_requirement_for_display(ty))
                })
                .collect::<Vec<_>>()
                .join(", ");

            let ret = match *outputs {
                0 => String::new(),
                1 => {
                    let ty = output_types
                        .first()
                        .copied()
                        .unwrap_or(InferredType::Unknown);
                    format!(" -> {}", inferred_type_for_display(ty))
                }
                n => {
                    let types = (0..n)
                        .map(|idx| {
                            let ty = output_types
                                .get(idx)
                                .copied()
                                .unwrap_or(InferredType::Unknown);
                            inferred_type_for_display(ty)
                        })
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

fn deduplicate_library_paths(paths: Vec<LibraryPath>) -> Vec<LibraryPath> {
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for path in paths {
        let canonical_root = path
            .root
            .canonicalize()
            .unwrap_or_else(|_| path.root.clone());
        let key = (path.prefix.clone(), canonical_root);
        if seen.insert(key) {
            out.push(path);
        }
    }
    out
}

fn diagnostic_code(diag: &Diagnostic) -> String {
    match &diag.code {
        Some(NumberOrString::String(code)) => code.clone(),
        Some(NumberOrString::Number(code)) => code.to_string(),
        None => "decompilation-failed".to_string(),
    }
}

fn find_procedure_containing_offset(module: &Module, offset: u32) -> Option<&Procedure> {
    module.procedures().find(|proc| {
        let range = proc.span().into_range();
        offset >= range.start && offset < range.end
    })
}

fn collect_use_statements(source: &str) -> Vec<String> {
    source
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with("use "))
        .map(ToOwned::to_owned)
        .collect()
}

fn cursor_outside_procedure_diagnostic(position: Position) -> Diagnostic {
    Diagnostic {
        range: Range::new(position, position),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(SOURCE_DECOMPILATION.to_string()),
        code: Some(NumberOrString::String(
            "cursor-outside-procedure".to_string(),
        )),
        message: normalize_message("cursor is not inside a procedure"),
        ..Default::default()
    }
}

fn unresolved_dependency_diagnostic(
    module: &Module,
    sources: &DefaultSourceManager,
    unresolved: &[SymbolPath],
) -> Diagnostic {
    let summary = unresolved_dependencies_summary(unresolved);
    let range = span_to_range(sources, module.span())
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some(SOURCE_ANALYSIS.to_string()),
        message: normalize_message(&format!(
            "analysis incomplete: unresolved transitive module dependencies ({summary})"
        )),
        ..Default::default()
    }
}

fn unresolved_dependencies_summary(unresolved: &[SymbolPath]) -> String {
    const MAX_ITEMS: usize = 5;
    let mut modules: Vec<String> = unresolved.iter().map(ToString::to_string).collect();
    modules.sort();
    modules.dedup();
    if modules.len() <= MAX_ITEMS {
        return modules.join(", ");
    }
    let remaining = modules.len() - MAX_ITEMS;
    let shown = modules
        .into_iter()
        .take(MAX_ITEMS)
        .collect::<Vec<_>>()
        .join(", ");
    format!("{shown}, and {remaining} more")
}

fn normalized_input_types(types: &[TypeRequirement], expected_len: usize) -> Vec<TypeRequirement> {
    let mut normalized = vec![TypeRequirement::Unknown; expected_len];
    for (display_idx, slot) in normalized.iter_mut().enumerate() {
        let summary_idx = expected_len.saturating_sub(1).saturating_sub(display_idx);
        if let Some(ty) = types.get(summary_idx) {
            *slot = *ty;
        }
    }
    normalized
}

fn normalized_output_types(types: &[InferredType], expected_len: usize) -> Vec<InferredType> {
    let mut normalized = vec![InferredType::Unknown; expected_len];
    for (display_idx, slot) in normalized.iter_mut().enumerate() {
        let summary_idx = expected_len.saturating_sub(1).saturating_sub(display_idx);
        if let Some(ty) = types.get(summary_idx) {
            *slot = *ty;
        }
    }
    normalized
}

fn type_requirement_for_display(requirement: TypeRequirement) -> &'static str {
    match requirement {
        TypeRequirement::Unknown | TypeRequirement::Felt => "Felt",
        TypeRequirement::Bool => "Bool",
        TypeRequirement::U32 => "U32",
        TypeRequirement::Address => "Address",
    }
}

fn inferred_type_for_display(ty: InferredType) -> &'static str {
    match ty {
        InferredType::Unknown | InferredType::Felt => "Felt",
        InferredType::Bool => "Bool",
        InferredType::U32 => "U32",
        InferredType::Address => "Address",
    }
}

fn type_inconsistency_diagnostics(
    uri: &Url,
    diagnostics: &TypeDiagnosticsMap,
    sources: &DefaultSourceManager,
) -> Vec<Diagnostic> {
    let Some(source_id) = sources.find(&to_miden_uri(uri)) else {
        return Vec::new();
    };

    diagnostics
        .values()
        .flat_map(|proc_diags| proc_diags.iter())
        .filter(|diag| diag.span.source_id() == source_id)
        .map(|diag| {
            let range = span_to_range(sources, diag.span)
                .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some(SOURCE_ANALYSIS.to_string()),
                message: normalize_message(&diag.message),
                ..Default::default()
            }
        })
        .collect()
}

fn signature_mismatch_diagnostics(
    mismatches: Vec<masm_analysis::SignatureMismatch>,
    sources: &DefaultSourceManager,
) -> Vec<Diagnostic> {
    mismatches
        .into_iter()
        .map(|mismatch| {
            let range = span_to_range(sources, mismatch.span)
                .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 0)));
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
