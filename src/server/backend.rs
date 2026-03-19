use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};

use masm_analysis::signature_mismatches_in_workspace;
use masm_decompiler::{
    callgraph::CallGraph,
    fmt::{CodeWriter, FormattingConfig},
    frontend::{LibraryRoot, Workspace},
    signature::{infer_signatures, ProcSignature, SignatureMap},
    symbol::resolution::{resolve_constant_path, resolve_constant_symbol},
    types::{
        infer_type_summaries, InferredType, TypeDiagnosticsMap, TypeRequirement, TypeSummary,
        TypeSummaryMap,
    },
    DecompilationConfig, Decompiler,
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
    dmasm::parser::{parse_dmasm, DmasmDocument},
    masm::cursor_resolution::{
        find_constant_candidate_at_position, position_to_offset, resolve_symbol_at_position,
        ResolutionError, ResolvedSymbol,
    },
    masm::diagnostics::{
        diagnostics_from_report, normalize_message, span_to_range, unresolved_to_diagnostics,
        SOURCE_ANALYSIS, SOURCE_DECOMPILATION,
    },
    masm::index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    masm::inlay_hints::{collect_decompilation_diagnostics, decompilation_error_diagnostic},
    masm::module_path::ModulePathResolver,
    service::DocumentService,
    util::{lsp_range_to_selection, to_miden_uri},
    LibraryPath, ServerConfig, SymbolPath,
};

use super::cache::DocumentCache;
use super::helpers::determine_module_kind_from_ast;
use super::tracked_workspace::{ProgramOrigin, TrackedProgram, TrackedWorkspace};

#[derive(Debug, Clone)]
pub struct Backend<C = tower_lsp::Client> {
    pub(crate) client: C,
    pub(crate) sources: Arc<DefaultSourceManager>,
    pub(crate) documents: DocumentCache,
    pub(crate) workspace: Arc<RwLock<WorkspaceIndex>>,
    pub(crate) config: Arc<RwLock<ServerConfig>>,
    pub(crate) workspace_parse_paths: Arc<RwLock<Vec<LibraryPath>>>,
    pub(crate) signature_cache: Arc<SignatureCache>,
    pub(crate) tracked_workspace: Arc<RwLock<TrackedWorkspace>>,
    pub(crate) dmasm_documents: Arc<RwLock<HashMap<Url, DmasmDocument>>>,
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
            workspace_parse_paths: Arc::new(RwLock::new(Vec::new())),
            signature_cache: Arc::new(SignatureCache::new()),
            tracked_workspace: Arc::new(RwLock::new(TrackedWorkspace::default())),
            dmasm_documents: Arc::new(RwLock::new(HashMap::new())),
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

    /// Handle opening a DMASM document.
    ///
    /// Stores the text in the source manager (for incremental sync support)
    /// and parses variable information. No diagnostics are published for
    /// DMASM files.
    pub(crate) async fn handle_open_dmasm(&self, uri: Url, text: String) {
        let miden_uri = to_miden_uri(&uri);
        // NOTE: SourceLanguage::Masm is the only available variant. The source
        // manager uses it purely as metadata — text storage is unaffected.
        // DMASM text stored here will NOT be parsed by the MASM pipeline because
        // all MASM handlers check the document language before processing.
        self.sources
            .load(SourceLanguage::Masm, miden_uri, text.clone());
        let doc = parse_dmasm(&text);
        self.dmasm_documents.write().await.insert(uri, doc);
    }

    /// Handle a change to a DMASM document.
    pub(crate) async fn handle_change_dmasm(&self, uri: Url, text: &str) {
        let doc = parse_dmasm(text);
        self.dmasm_documents.write().await.insert(uri, doc);
    }

    /// Handle closing a DMASM document.
    pub(crate) async fn handle_close_dmasm(&self, uri: &Url) {
        self.dmasm_documents.write().await.remove(uri);
        // Note: the source manager does not support removal, so the text
        // may linger until the server shuts down. This is consistent with
        // how MASM files behave on close.
    }

    /// Get the parsed DMASM document for a URI, if it exists.
    pub(crate) async fn get_dmasm_document(&self, uri: &Url) -> Option<DmasmDocument> {
        self.dmasm_documents.read().await.get(uri).cloned()
    }

    pub(crate) async fn set_workspace_parse_paths(&self, paths: Vec<LibraryPath>) {
        let mut guard = self.workspace_parse_paths.write().await;
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
        deduplicate_library_paths(configured.to_vec())
    }

    pub(crate) async fn effective_parse_paths(&self) -> Vec<LibraryPath> {
        let cfg = self.snapshot_config().await;
        self.effective_parse_paths_from(&cfg.library_paths).await
    }

    pub(crate) async fn effective_parse_paths_from(
        &self,
        configured: &[LibraryPath],
    ) -> Vec<LibraryPath> {
        let workspace_paths = self.workspace_parse_paths.read().await.clone();
        let mut combined = configured.to_vec();
        combined.extend(workspace_paths);
        deduplicate_library_paths(combined)
    }

    pub(crate) async fn invalidate_signature_cache(&self) {
        self.signature_cache.invalidate().await;
    }

    pub(crate) async fn inferred_signature_line(
        &self,
        _module: &Module,
        uri: &Url,
        symbol_path: &SymbolPath,
        library_paths: &[LibraryPath],
    ) -> Option<String> {
        if let Some(value) = self.signature_cache.get(symbol_path).await {
            return value;
        }
        let generation = self.signature_cache.generation();
        let inferred = self
            .infer_signature_line(uri, symbol_path, library_paths)
            .await;
        self.signature_cache
            .insert_if_fresh(symbol_path.clone(), inferred.clone(), generation)
            .await;
        inferred
    }

    pub(crate) async fn build_analysis_workspace(
        &self,
        focus_uri: Option<&Url>,
        library_paths: &[LibraryPath],
    ) -> Workspace {
        let focus_program_from_cache = if let Some(uri) = focus_uri {
            self.documents.get_symbols_if_current(uri).await.map(|doc| {
                TrackedProgram::new(
                    uri.clone(),
                    source_path_from_uri(uri),
                    doc,
                    ProgramOrigin::OpenDocument,
                )
            })
        } else {
            None
        };
        let tracked = self.tracked_workspace.read().await;
        let focus_program = focus_uri
            .and_then(|uri| tracked.candidate_program_for_uri(uri))
            .or(focus_program_from_cache);
        let active_programs = tracked.active_programs();
        drop(tracked);

        let roots = build_library_roots(library_paths);
        let mut workspace = Workspace::with_source_manager(roots, self.sources.clone());
        let mut active_by_module: HashMap<String, TrackedProgram> = active_programs
            .into_iter()
            .map(|program| (normalized_symbol_path_key(&program.module_path()), program))
            .collect();

        if let Some(program) = focus_program {
            active_by_module.remove(&normalized_symbol_path_key(&program.module_path()));
            workspace.add_program(program.to_program());
            add_tracked_dependency_closure(&mut workspace, &mut active_by_module);
            return workspace;
        }

        for program in active_by_module.into_values() {
            workspace.add_program(program.to_program());
        }
        workspace.load_dependencies();
        workspace
    }

    async fn infer_signature_line(
        &self,
        uri: &Url,
        symbol_path: &SymbolPath,
        library_paths: &[LibraryPath],
    ) -> Option<String> {
        let workspace = self
            .build_analysis_workspace(Some(uri), library_paths)
            .await;
        let analysis = infer_analysis_snapshot(&workspace);
        if !analysis.unresolved_modules.is_empty() {
            return None;
        }
        let signature = analysis.signatures.get(symbol_path)?;
        let summary = analysis.type_summaries.get(symbol_path);
        format_inferred_signature(symbol_path.name(), signature, summary)
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

        let workspace = self
            .build_analysis_workspace(Some(uri), library_paths)
            .await;

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
        let libraries = self.effective_library_paths().await;
        let files = match collect_library_files(&libraries) {
            Ok(files) => files,
            Err(err) => {
                error!("failed to scan configured libraries: {err}");
                return;
            }
        };

        let next_uris: HashSet<Url> = files
            .iter()
            .filter_map(|path| Url::from_file_path(path).ok())
            .collect();
        let previous_uris = self.tracked_workspace.read().await.tracked_disk_uris();

        let mut affected = HashSet::new();
        for removed in previous_uris.difference(&next_uris) {
            let changed = self
                .tracked_workspace
                .write()
                .await
                .remove_tracked_disk_program(removed);
            affected.extend(changed);
            if self.documents.get_version(removed).await.is_none() {
                self.documents.remove_symbols(removed).await;
            }
        }

        for (disk_priority, path) in files.iter().enumerate() {
            match self
                .load_library_file(path, &libraries, disk_priority as u64)
                .await
            {
                Ok(changed) => affected.extend(changed),
                Err(err) => error!("failed to load library file {}: {err}", path.display()),
            }
        }

        self.sync_workspace_index_for_uris(affected).await;
    }

    pub(crate) async fn decompile_procedure_at_position(
        &self,
        uri: &Url,
        position: Position,
        library_paths: &[LibraryPath],
        decompilation_config: &DecompilationConfig,
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
        let workspace = self
            .build_analysis_workspace(Some(uri), library_paths)
            .await;

        let fq_name = if module_path.is_empty() {
            proc_name.clone()
        } else {
            format!("{module_path}::{proc_name}")
        };

        let decompiler = Decompiler::new(&workspace).with_config(decompilation_config.clone());
        let decompiled = match decompiler.decompile_proc(&fq_name) {
            Ok(value) => value,
            Err(error) => {
                let diagnostic = decompilation_error_diagnostic(
                    self.sources.as_ref(),
                    proc_range,
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
                    ))
                    .replacen("Could", "could", 1),
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
        decompilation_config: &DecompilationConfig,
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
        let workspace = self
            .build_analysis_workspace(Some(uri), library_paths)
            .await;
        let decompiler = Decompiler::new(&workspace).with_config(decompilation_config.clone());

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
                        proc_range,
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
                        ))
                        .replacen("Could", "could", 1),
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
        self.track_open_document_state(&uri).await;
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
        self.track_open_document_state(&uri).await;
        self.publish_diagnostics(uri).await;
    }

    pub async fn handle_close(&self, uri: Url) {
        self.invalidate_signature_cache().await;
        self.documents.remove(&uri).await;
        let changed = self
            .tracked_workspace
            .write()
            .await
            .close_open_document(&uri);

        if self
            .tracked_workspace
            .read()
            .await
            .active_program_for_uri(&uri)
            .is_some()
        {
            self.restore_active_symbols_for_uri(&uri).await;
        } else {
            self.documents.remove_symbols(&uri).await;
        }

        self.sync_workspace_index_for_uris(changed).await;
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
        let parse_result = self.parse_and_cache_document(&uri).await;
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let fallback_doc = if parse_result.is_err() {
            self.documents.get_symbols(&uri).await
        } else {
            None
        };

        let (analysis_diags, decompilation_diags) = {
            let analysis_workspace = if parse_result.is_ok()
                && (config.taint_analysis_enabled
                    || config.inlay_hint_type == crate::InlayHintType::Decompilation)
            {
                Some(
                    self.build_analysis_workspace(Some(&uri), &effective_library_paths)
                        .await,
                )
            } else {
                None
            };

            let analysis_diags = if config.taint_analysis_enabled {
                if let (Ok(doc), Some(workspace)) = (&parse_result, analysis_workspace.as_ref()) {
                    let analysis = infer_analysis_snapshot(workspace);
                    let mut diags = Vec::new();
                    if analysis.unresolved_modules.is_empty() {
                        let mismatches = signature_mismatches_in_workspace(
                            &doc.module,
                            self.sources.clone(),
                            workspace,
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
                    }
                    diags
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let decompilation_diags = if config.inlay_hint_type
                == crate::InlayHintType::Decompilation
            {
                if let (Ok(doc), Some(workspace)) = (&parse_result, analysis_workspace.as_ref()) {
                    collect_decompilation_diagnostics(
                        &doc.module,
                        self.sources.clone(),
                        workspace,
                        &config.decompilation_config,
                    )
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            (analysis_diags, decompilation_diags)
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

        if !decompilation_diags.is_empty() {
            diagnostics.extend(decompilation_diags);
        }

        if !extra.is_empty() {
            diagnostics.extend(extra);
        }

        self.client
            .publish_diagnostics(uri, diagnostics.clone(), version)
            .await;

        diagnostics
    }

    async fn track_open_document_state(&self, uri: &Url) {
        let changed = match self.parse_document_symbols(uri, self.sources.clone()).await {
            Ok(doc_symbols) => {
                let version = self.documents.get_version(uri).await.unwrap_or(0);
                self.documents
                    .set_symbols(uri.clone(), doc_symbols.clone(), version)
                    .await;
                let program = TrackedProgram::new(
                    uri.clone(),
                    source_path_from_uri(uri),
                    doc_symbols,
                    ProgramOrigin::OpenDocument,
                );
                self.tracked_workspace
                    .write()
                    .await
                    .upsert_open_program(program)
            }
            Err(_) => {
                let changed = self
                    .tracked_workspace
                    .write()
                    .await
                    .mark_open_parse_failed(uri);
                if self
                    .tracked_workspace
                    .read()
                    .await
                    .active_program_for_uri(uri)
                    .is_none()
                {
                    self.documents.remove_symbols(uri).await;
                }
                changed
            }
        };

        self.sync_workspace_index_for_uris(changed).await;
    }

    async fn restore_active_symbols_for_uri(&self, uri: &Url) {
        let active = self
            .tracked_workspace
            .read()
            .await
            .active_program_for_uri(uri);
        let Some(program) = active else {
            self.documents.remove_symbols(uri).await;
            return;
        };

        if matches!(program.origin, ProgramOrigin::DiskTracked)
            && let Ok(path) = uri.to_file_path()
            && let Ok(text) = fs::read_to_string(path)
        {
            self.sources
                .load(SourceLanguage::Masm, to_miden_uri(uri), text);
        }

        let version = self.documents.get_version(uri).await.unwrap_or(0);
        self.documents
            .set_symbols(uri.clone(), program.symbols, version)
            .await;
    }

    async fn sync_workspace_index_for_uris(&self, uris: HashSet<Url>) {
        if uris.is_empty() {
            return;
        }

        let tracked = self.tracked_workspace.read().await;
        let mut updates = Vec::new();
        for uri in uris {
            let active = tracked.active_program_for_uri(&uri);
            updates.push((uri, active.map(|program| program.symbols)));
        }
        drop(tracked);

        let mut workspace = self.workspace.write().await;
        for (uri, active) in updates {
            if let Some(symbols) = active {
                workspace.update_document(uri, &symbols.definitions, &symbols.references);
            } else {
                workspace.update_document(uri, &[], &[]);
            }
        }
    }

    async fn parse_and_cache_document(
        &self,
        uri: &Url,
    ) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
        let doc_symbols = self
            .parse_document_symbols(uri, self.sources.clone())
            .await?;
        let version = self.documents.get_version(uri).await.unwrap_or(0);
        self.documents
            .set_symbols(uri.clone(), doc_symbols.clone(), version)
            .await;
        Ok(doc_symbols)
    }

    async fn parse_document_symbols(
        &self,
        uri: &Url,
        sources: Arc<DefaultSourceManager>,
    ) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
        let library_paths = self.effective_parse_paths().await;
        parse_document_symbols_in_sources(uri, sources, &library_paths)
    }

    pub(crate) async fn get_or_parse_document(&self, uri: &Url) -> Option<DocumentSymbols> {
        if let Some(doc) = self.documents.get_symbols_if_current(uri).await {
            return Some(doc);
        }
        self.parse_and_cache_document(uri).await.ok()
    }

    async fn load_library_file(
        &self,
        path: &Path,
        library_paths: &[LibraryPath],
        disk_priority: u64,
    ) -> std::io::Result<HashSet<Url>> {
        let text = fs::read_to_string(path)?;
        let url = Url::from_file_path(path).map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "invalid file path for Url",
            )
        })?;
        let is_open = self.documents.get_version(&url).await.is_some();

        let doc_symbols = if is_open {
            parse_document_symbols_from_text(&url, &text, library_paths).map_err(|err| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, err.to_string())
            })?
        } else {
            self.sources
                .load(SourceLanguage::Masm, to_miden_uri(&url), text);
            let doc_symbols =
                parse_document_symbols_in_sources(&url, self.sources.clone(), library_paths)
                    .map_err(|err| {
                        std::io::Error::new(std::io::ErrorKind::InvalidData, err.to_string())
                    })?;
            self.documents
                .set_symbols(url.clone(), doc_symbols.clone(), 0)
                .await;
            doc_symbols
        };

        let program = TrackedProgram::new(
            url.clone(),
            path.to_path_buf(),
            doc_symbols,
            ProgramOrigin::DiskTracked,
        );
        Ok(self
            .tracked_workspace
            .write()
            .await
            .upsert_disk_program(program, disk_priority))
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

fn infer_analysis_snapshot(workspace: &Workspace) -> AnalysisSnapshot {
    let unresolved_modules = workspace.unresolved_module_paths();

    let callgraph = CallGraph::from(workspace);
    let signatures = infer_signatures(workspace, &callgraph);
    let (type_summaries, type_diagnostics) =
        infer_type_summaries(workspace, &callgraph, &signatures);

    AnalysisSnapshot {
        signatures,
        type_summaries,
        type_diagnostics,
        unresolved_modules,
    }
}

fn source_path_from_uri(uri: &Url) -> PathBuf {
    uri.to_file_path()
        .unwrap_or_else(|_| PathBuf::from("in-memory.masm"))
}

fn collect_library_files(library_paths: &[LibraryPath]) -> std::io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for lib in library_paths {
        if !lib.root.is_dir() {
            continue;
        }
        let mut stack = vec![lib.root.clone()];
        while let Some(dir) = stack.pop() {
            let mut entries: Vec<_> = fs::read_dir(&dir)?.collect::<Result<_, _>>()?;
            entries.sort_by_key(|entry| entry.path());
            for entry in entries {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext.eq_ignore_ascii_case("masm"))
                    .unwrap_or(false)
                {
                    files.push(path);
                }
            }
        }
    }
    files.sort();
    Ok(files)
}

fn parse_document_symbols_from_text(
    uri: &Url,
    text: &str,
    library_paths: &[LibraryPath],
) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
    let sources = Arc::new(DefaultSourceManager::default());
    sources.load(SourceLanguage::Masm, to_miden_uri(uri), text.to_owned());
    parse_document_symbols_in_sources(uri, sources, library_paths)
}

fn parse_document_symbols_in_sources(
    uri: &Url,
    sources: Arc<DefaultSourceManager>,
    library_paths: &[LibraryPath],
) -> std::result::Result<DocumentSymbols, miden_assembly_syntax::Report> {
    let module = parse_module_in_sources(uri, sources.clone(), library_paths)?;
    Ok(build_document_symbols(module, sources))
}

fn parse_module_in_sources(
    uri: &Url,
    sources: Arc<DefaultSourceManager>,
    library_paths: &[LibraryPath],
) -> std::result::Result<Box<miden_assembly_syntax::ast::Module>, miden_assembly_syntax::Report> {
    use miden_assembly_syntax::ast::ModuleKind;

    let miden_uri = to_miden_uri(uri);
    let source_file = sources
        .get_by_uri(&miden_uri)
        .ok_or_else(|| diagnostics::report!("file not loaded in source manager"))?;

    let opts = ParseOptions {
        kind: ModuleKind::Library,
        path: module_path_from_uri_with_paths(uri, library_paths).map(Into::into),
        ..Default::default()
    };

    match source_file
        .clone()
        .parse_with_options(sources.clone(), opts)
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
                let opts = ParseOptions {
                    kind: ModuleKind::Executable,
                    path: module_path_from_uri_with_paths(uri, library_paths).map(Into::into),
                    ..Default::default()
                };
                match source_file.clone().parse_with_options(sources, opts) {
                    Ok(mut module) => {
                        let detected_kind = determine_module_kind_from_ast(&module);
                        if module.kind() != detected_kind {
                            module.set_kind(detected_kind);
                        }
                        return Ok(module);
                    }
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

fn module_path_from_uri_with_paths(
    uri: &Url,
    library_paths: &[LibraryPath],
) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let resolver = ModulePathResolver::new(library_paths);
    resolver.resolve(uri)
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

fn normalized_symbol_path_key(path: &SymbolPath) -> String {
    path.to_string().trim_start_matches("::").to_string()
}

fn add_tracked_dependency_closure(
    workspace: &mut Workspace,
    active_by_module: &mut HashMap<String, TrackedProgram>,
) {
    loop {
        workspace.load_dependencies();
        let unresolved = workspace.unresolved_module_paths();
        let mut additions = Vec::new();
        for module in unresolved {
            let key = module.to_string().trim_start_matches("::").to_string();
            if let Some(program) = active_by_module.remove(&key) {
                additions.push(program);
            }
        }
        if additions.is_empty() {
            break;
        }
        for program in additions {
            workspace.add_program(program.to_program());
        }
    }
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
