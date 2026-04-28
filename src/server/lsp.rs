use crate::client::PublishDiagnostics;
use crate::core_lib::{default_core_library_path, normalize_core_library_path};
use crate::dmasm::{DocumentLanguage, detect_language};
use crate::masm::code_lens::collect_code_lenses;
use crate::masm::cursor_resolution::resolve_symbol_at_position;
use crate::masm::diagnostics::{normalize_message, span_to_range};
use crate::masm::inlay_hints::collect_inlay_hints;
use crate::util::{extract_token_at_position, to_miden_uri};
use crate::{InlayHintType, LibraryPath};
use miden_debug_types::SourceManager;
use std::{collections::HashSet, path::PathBuf};
use tower_lsp::{
    LanguageServer,
    jsonrpc::{ErrorCode, Result},
    lsp_types::{
        CodeLens, CodeLensOptions, CodeLensParams, ExecuteCommandOptions, ExecuteCommandParams,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
        InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintParams,
        Location, MarkupContent, MarkupKind, Position, ReferenceParams, RenameOptions,
        ServerCapabilities, SymbolInformation, SymbolKind, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkspaceEdit, WorkspaceSymbolParams,
        request::{GotoImplementationParams, GotoImplementationResponse},
    },
};
use tracing::{debug, info};

use super::backend::{Backend, FileDecompilationError, ProcedureDecompilationError};
use super::config::{
    extract_code_lens_stack_effects, extract_decompilation_config, extract_inlay_hint_type,
    extract_library_paths, format_decompilation_config_header,
};
use super::helpers::{
    extract_doc_comment, extract_procedure_attributes, extract_procedure_signature,
    is_on_use_statement,
};

const CMD_SET_CORE_PATH: &str = "masm-lsp.setCorePath";
const CMD_DECOMPILE_PROCEDURE_AT_CURSOR: &str = "masm-lsp.decompileProcedureAtCursor";
const CMD_DECOMPILE_FILE: &str = "masm-lsp.decompileFile";
const CMD_GROUP_ADVICE_DIAGNOSTICS_BY_ORIGIN: &str = "masm-lsp.groupAdviceDiagnosticsByOrigin";

#[tower_lsp::async_trait]
impl<C> LanguageServer for Backend<C>
where
    C: PublishDiagnostics,
{
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let workspace_paths = workspace_parse_paths_from_initialize(&params);
        self.set_workspace_parse_paths(workspace_paths).await;

        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            hover_provider: Some(tower_lsp::lsp_types::HoverProviderCapability::Simple(true)),
            definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            implementation_provider: Some(
                tower_lsp::lsp_types::ImplementationProviderCapability::Simple(true),
            ),
            references_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            inlay_hint_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            rename_provider: Some(tower_lsp::lsp_types::OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            document_symbol_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(false),
            }),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![
                    CMD_SET_CORE_PATH.to_string(),
                    CMD_DECOMPILE_PROCEDURE_AT_CURSOR.to_string(),
                    CMD_DECOMPILE_FILE.to_string(),
                    CMD_GROUP_ADVICE_DIAGNOSTICS_BY_ORIGIN.to_string(),
                ],
                work_done_progress_options: Default::default(),
            }),
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

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        match params.command.as_str() {
            CMD_SET_CORE_PATH => {
                let Some(raw_path) = parse_core_library_path_argument(&params.arguments) else {
                    return Err(tower_lsp::jsonrpc::Error::invalid_params(
                        "masm-lsp.setCorePath expects path as first argument",
                    ));
                };
                let root = normalize_core_library_path(&raw_path);

                let mut cfg = self.config.write().await;
                cfg.library_paths = vec![default_core_library_path(root.clone())];
                drop(cfg);

                info!("updated core library root via command: {}", root.display());
                self.load_configured_libraries().await;

                Ok(Some(serde_json::json!({
                    "root": root.to_string_lossy().to_string(),
                    "prefix": crate::core_lib::DEFAULT_CORE_LIBRARY_PREFIX,
                })))
            }
            CMD_DECOMPILE_PROCEDURE_AT_CURSOR => {
                self.execute_decompile_procedure_at_cursor(&params).await
            }
            CMD_DECOMPILE_FILE => self.execute_decompile_file(&params).await,
            CMD_GROUP_ADVICE_DIAGNOSTICS_BY_ORIGIN => {
                self.execute_group_advice_diagnostics_by_origin(&params)
                    .await
            }
            _ => Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "unknown command: {}",
                params.command
            ))),
        }
    }

    async fn did_change_configuration(
        &self,
        params: tower_lsp::lsp_types::DidChangeConfigurationParams,
    ) {
        let mut cfg = self.config.write().await;
        if let Some(paths) = extract_library_paths(&params.settings) {
            cfg.library_paths = paths;
            info!("updated library search paths");
        }
        if let Some(enabled) = extract_code_lens_stack_effects(&params.settings) {
            cfg.code_lens_stack_effects = enabled;
            info!("updated stack-effect code lens toggle: {}", enabled);
        }
        if let Some(mode) = extract_inlay_hint_type(&params.settings) {
            cfg.inlay_hint_type = mode;
            info!("updated inlay hint type: {:?}", mode);
        }
        if let Some(decompilation) = extract_decompilation_config(&params.settings) {
            cfg.decompilation_config = decompilation;
            info!("updated decompilation config");
        }
        drop(cfg);
        self.load_configured_libraries().await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: tower_lsp::lsp_types::DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let text = params.text_document.text;

        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                self.handle_open_dmasm(uri, text).await;
            }
            DocumentLanguage::Masm => {
                let _ = self.handle_open(uri, version, text).await;
            }
        }
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;

        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                let miden_uri = crate::util::to_miden_uri(&uri);
                if let Some(id) = self.sources.find(&miden_uri) {
                    // Apply incremental changes via the source manager.
                    for change in &params.content_changes {
                        let selection = change.range.map(crate::util::lsp_range_to_selection);
                        let _ = self
                            .sources
                            .update(id, change.text.clone(), selection, version);
                    }
                    // Re-read the full updated text and re-parse.
                    if let Some(updated) = self.sources.get_by_uri(&miden_uri) {
                        self.handle_change_dmasm(uri, updated.as_str()).await;
                    }
                } else {
                    // Source not loaded (should not happen if did_open was called).
                    // Only accept full-document replacements in this fallback.
                    if let Some(last) = params.content_changes.last() {
                        if last.range.is_none() {
                            self.handle_open_dmasm(uri, last.text.clone()).await;
                        } else {
                            tracing::warn!(
                                "DMASM incremental change received but source not loaded: {uri}"
                            );
                        }
                    }
                }
            }
            DocumentLanguage::Masm => {
                self.handle_change(uri, version, params.content_changes)
                    .await;
                let _ = self.publish_diagnostics(params.text_document.uri).await;
            }
        }
    }

    async fn did_close(&self, params: tower_lsp::lsp_types::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                self.handle_close_dmasm(&uri).await;
            }
            DocumentLanguage::Masm => {
                self.handle_close(uri).await;
            }
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        if detect_language(&params.text_document_position_params.text_document.uri)
            == DocumentLanguage::Dmasm
        {
            return Ok(None);
        }
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let miden_uri = to_miden_uri(&uri);

        if let Some(source) = self.sources.get_by_uri(&miden_uri)
            && is_on_use_statement(source.as_str(), pos)
            && let Some(token) = extract_token_at_position(&source, pos)
        {
            let normalized = token.trim_start_matches(':');
            let workspace = self.workspace.read().await;

            if let Some(loc) = workspace
                .definition(&format!("::{}", normalized))
                .or_else(|| {
                    normalized
                        .rsplit("::")
                        .next()
                        .and_then(|name| workspace.definition_by_name(name))
                })
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
            }
        }

        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let symbol = match resolve_symbol_at_position(&uri, &doc.module, self.sources.clone(), pos)
        {
            Ok(s) => s,
            Err(e) => {
                debug!("goto_definition: {e}");
                match self
                    .resolve_constant_symbol_at_position(
                        &uri,
                        &doc.module,
                        pos,
                        &effective_library_paths,
                    )
                    .await
                {
                    Ok(s) => s,
                    Err(err) => {
                        debug!("goto_definition (constant): {err}");
                        return Ok(None);
                    }
                }
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
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let pos = params.text_document_position_params.position;

        if detect_language(&uri) == DocumentLanguage::Dmasm {
            let Some(doc) = self.get_dmasm_document(&uri).await else {
                return Ok(None);
            };
            return Ok(crate::dmasm::services::hover(&doc, pos));
        }

        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let symbol = match resolve_symbol_at_position(&uri, &doc.module, self.sources.clone(), pos)
        {
            Ok(symbol) => symbol,
            Err(e) => {
                debug!("hover: symbol resolution failed: {e}");
                match self
                    .resolve_constant_symbol_at_position(
                        &uri,
                        &doc.module,
                        pos,
                        &effective_library_paths,
                    )
                    .await
                {
                    Ok(symbol) => symbol,
                    Err(err) => {
                        debug!("hover (constant): symbol resolution failed: {err}");
                        return Ok(None);
                    }
                }
            }
        };

        let workspace = self.workspace.read().await;
        let def_loc = workspace
            .definition(symbol.path.as_str())
            .or_else(|| workspace.definition_by_suffix(symbol.path.as_str()))
            .or_else(|| workspace.definition_by_name(&symbol.name));

        drop(workspace);

        if let Some(loc) = def_loc {
            let Some(def_doc) = self.get_or_parse_document(&loc.uri).await else {
                return Ok(None);
            };
            let def_uri = to_miden_uri(&loc.uri);
            if let Some(source) = self.sources.get_by_uri(&def_uri) {
                let content = source.as_str();
                let def_line = loc.range.start.line as usize;
                let attributes = extract_procedure_attributes(content, def_line);
                let inferred = self
                    .inferred_signature_line(
                        &def_doc.module,
                        &loc.uri,
                        &symbol.path,
                        &effective_library_paths,
                    )
                    .await;
                let signature = match inferred {
                    Some(line) => {
                        let mut lines = attributes;
                        lines.push(line);
                        Some(lines.join("\n"))
                    }
                    None => extract_procedure_signature(content, def_line).or_else(|| {
                        if attributes.is_empty() {
                            None
                        } else {
                            Some(attributes.join("\n"))
                        }
                    }),
                };
                let comment = extract_doc_comment(content, def_line);

                let hover_text = match (signature.as_ref(), comment) {
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
        let uri = params.text_document_position.text_document.uri.clone();
        let pos = params.text_document_position.position;

        if detect_language(&uri) == DocumentLanguage::Dmasm {
            let Some(doc) = self.get_dmasm_document(&uri).await else {
                return Ok(None);
            };
            return Ok(crate::dmasm::services::references(&doc, &uri, pos));
        }

        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let symbol = match resolve_symbol_at_position(&uri, &doc.module, self.sources.clone(), pos)
        {
            Ok(s) => s,
            Err(e) => {
                debug!("references: {e}");
                match self
                    .resolve_constant_symbol_at_position(
                        &uri,
                        &doc.module,
                        pos,
                        &effective_library_paths,
                    )
                    .await
                {
                    Ok(s) => s,
                    Err(err) => {
                        debug!("references (constant): {err}");
                        return Ok(None);
                    }
                }
            }
        };

        let workspace = self.workspace.read().await;

        let mut results = workspace.references(symbol.path.as_str());
        if results.is_empty() {
            results.extend(workspace.references_by_suffix(symbol.path.as_str()));
        }

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

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        if detect_language(&params.text_document.uri) == DocumentLanguage::Dmasm {
            return Ok(Some(vec![]));
        }
        let config = self.snapshot_config().await;
        if !config.code_lens_stack_effects {
            return Ok(Some(vec![]));
        }
        let uri = params.text_document.uri;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let lenses = collect_code_lenses(&doc.module, self.sources.as_ref());
        Ok(Some(lenses))
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        Ok(params)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        if detect_language(&params.text_document.uri) == DocumentLanguage::Dmasm {
            return Ok(Some(vec![]));
        }
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;

        if config.inlay_hint_type == InlayHintType::None {
            return Ok(Some(vec![]));
        }

        let uri = params.text_document.uri;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let miden_uri = to_miden_uri(&uri);
        let Some(source) = self.sources.get_by_uri(&miden_uri) else {
            return Ok(None);
        };

        let result = if config.inlay_hint_type == InlayHintType::Decompilation {
            let workspace = self
                .build_analysis_workspace(Some(&uri), &effective_library_paths)
                .await;
            collect_inlay_hints(
                &doc.module,
                self.sources.clone(),
                &params.range,
                source.as_str(),
                config.inlay_hint_type,
                Some(&workspace),
                &config.decompilation_config,
            )
        } else {
            collect_inlay_hints(
                &doc.module,
                self.sources.clone(),
                &params.range,
                source.as_str(),
                config.inlay_hint_type,
                None,
                &config.decompilation_config,
            )
        };
        Ok(Some(result.hints))
    }

    async fn rename(
        &self,
        params: tower_lsp::lsp_types::RenameParams,
    ) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                let Some(doc) = self.get_dmasm_document(&uri).await else {
                    return Ok(None);
                };
                Ok(crate::dmasm::services::rename(
                    &doc,
                    &uri,
                    pos,
                    &params.new_name,
                ))
            }
            DocumentLanguage::Masm => {
                // Rename is not supported for MASM files.
                Ok(None)
            }
        }
    }

    async fn prepare_rename(
        &self,
        params: tower_lsp::lsp_types::TextDocumentPositionParams,
    ) -> Result<Option<tower_lsp::lsp_types::PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let pos = params.position;

        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                let Some(doc) = self.get_dmasm_document(&uri).await else {
                    return Ok(None);
                };
                Ok(crate::dmasm::services::prepare_rename(&doc, pos)
                    .map(tower_lsp::lsp_types::PrepareRenameResponse::Range))
            }
            DocumentLanguage::Masm => Ok(None),
        }
    }

    async fn document_symbol(
        &self,
        params: tower_lsp::lsp_types::DocumentSymbolParams,
    ) -> Result<Option<tower_lsp::lsp_types::DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        match detect_language(&uri) {
            DocumentLanguage::Dmasm => {
                let Some(doc) = self.get_dmasm_document(&uri).await else {
                    return Ok(None);
                };
                let symbols = crate::dmasm::services::document_symbols(&doc);
                Ok(Some(tower_lsp::lsp_types::DocumentSymbolResponse::Nested(
                    symbols,
                )))
            }
            DocumentLanguage::Masm => {
                // Document symbols are not currently implemented for MASM.
                Ok(None)
            }
        }
    }
}

impl<C> Backend<C>
where
    C: PublishDiagnostics,
{
    async fn execute_decompile_file(
        &self,
        params: &ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let Some(uri) = parse_decompile_file_argument(&params.arguments) else {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "masm-lsp.decompileFile expects a single argument object: {\"uri\": \"file:///...\"}",
            ));
        };

        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;

        let decompilation_config = config.decompilation_config.clone();
        let result = self
            .decompile_file(&uri, &effective_library_paths, &decompilation_config)
            .await
            .map_err(decompile_file_command_error)?;
        let config_header = format_decompilation_config_header(&decompilation_config);
        let procedures = result
            .procedures
            .into_iter()
            .map(|proc| {
                serde_json::json!({
                    "name": proc.name,
                    "path": proc.path,
                    "range": proc.range,
                    "decompiled": proc.decompiled,
                })
            })
            .collect::<Vec<_>>();
        let failures = result
            .failures
            .into_iter()
            .map(|failure| {
                serde_json::json!({
                    "name": failure.name,
                    "path": failure.path,
                    "range": failure.range,
                    "code": failure.code,
                    "message": failure.message,
                })
            })
            .collect::<Vec<_>>();
        let decompiled_count = procedures.len();
        let failed_count = failures.len();
        let total_count = decompiled_count + failed_count;
        let status = if failed_count == 0 {
            "success"
        } else if decompiled_count == 0 {
            "failure"
        } else {
            "partial"
        };

        Ok(Some(serde_json::json!({
            "uri": uri,
            "modulePath": result.module_path,
            "useStatements": result.use_statements,
            "configHeader": config_header,
            "status": status,
            "summary": {
                "totalProcedures": total_count,
                "decompiledProcedures": decompiled_count,
                "failedProcedures": failed_count,
            },
            "procedures": procedures,
            "failures": failures,
        })))
    }

    async fn execute_decompile_procedure_at_cursor(
        &self,
        params: &ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let Some((uri, position)) = parse_decompile_cursor_argument(&params.arguments) else {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "masm-lsp.decompileProcedureAtCursor expects a single argument object: {\"uri\": \"file:///...\", \"position\": {\"line\": number, \"character\": number}}",
            ));
        };

        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;

        match self
            .decompile_procedure_at_position(
                &uri,
                position,
                &effective_library_paths,
                &config.decompilation_config,
            )
            .await
        {
            Ok(result) => Ok(Some(serde_json::json!({
                "uri": uri,
                "procedure": {
                    "name": result.name,
                    "path": result.path,
                    "range": result.range,
                },
                "decompiled": result.decompiled,
            }))),
            Err(err) => {
                if let Some(diagnostic) = decompilation_error_diagnostic_for_publish(&err) {
                    let _ = self
                        .publish_diagnostics_with_extra(uri.clone(), vec![diagnostic])
                        .await;
                }
                Err(decompile_command_error(err))
            }
        }
    }

    async fn execute_group_advice_diagnostics_by_origin(
        &self,
        params: &ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let Some(focus_uri) = parse_group_advice_argument(&params.arguments) else {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "masm-lsp.groupAdviceDiagnosticsByOrigin expects zero arguments or a single argument object: {\"uri\": \"file:///...\"}",
            ));
        };

        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let workspace = self
            .build_analysis_workspace(focus_uri.as_ref(), &effective_library_paths)
            .await;
        let analysis = masm_analysis::AnalysisSnapshot::from_workspace(&workspace);
        let focus_source_id = focus_uri
            .as_ref()
            .and_then(|uri| self.sources.find(&to_miden_uri(uri)));

        let groups =
            masm_analysis::group_advice_diagnostics_by_origin(&analysis.advice_diagnostics)
                .into_iter()
                .filter(|group| {
                    focus_source_id.is_none_or(|source_id| {
                        group.origin.source_id() == source_id
                            || group
                                .diagnostics
                                .iter()
                                .any(|diag| diag.span.source_id() == source_id)
                    })
                })
                .filter_map(|group| {
                    let origin = span_location_value(self.sources.as_ref(), group.origin)?;
                    let sinks = group
                        .diagnostics
                        .iter()
                        .filter_map(|diag| {
                            Some(serde_json::json!({
                                "location": span_location_value(self.sources.as_ref(), diag.span)?,
                                "procedure": diag.procedure.as_str(),
                                "message": normalize_message(&diag.message),
                            }))
                        })
                        .collect::<Vec<_>>();
                    let procedures = grouped_procedures(&group.diagnostics);
                    Some(serde_json::json!({
                        "origin": origin,
                        "message": normalize_message(&group.summary_message()),
                        "sinkCount": sinks.len(),
                        "procedures": procedures,
                        "sinks": sinks,
                    }))
                })
                .collect::<Vec<_>>();

        Ok(Some(serde_json::json!({
            "scopeUri": focus_uri,
            "unresolvedModules": analysis.unresolved_modules.iter().map(|module| module.as_str()).collect::<Vec<_>>(),
            "groups": groups,
        })))
    }
}

fn workspace_parse_paths_from_initialize(params: &InitializeParams) -> Vec<LibraryPath> {
    let mut roots = Vec::new();

    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }

    if roots.is_empty()
        && let Some(root_uri) = &params.root_uri
        && let Ok(path) = root_uri.to_file_path()
    {
        roots.push(path);
    }

    if roots.is_empty() {
        #[allow(deprecated)]
        if let Some(root_path) = &params.root_path {
            roots.push(PathBuf::from(root_path));
        }
    }

    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for root in roots {
        let canonical = root.canonicalize().unwrap_or(root.clone());
        if seen.insert(canonical) {
            out.push(LibraryPath {
                root,
                prefix: String::new(),
            });
        }
    }
    out
}

fn parse_core_library_path_argument(arguments: &[serde_json::Value]) -> Option<PathBuf> {
    let first = arguments.first()?;
    if let Some(path) = first.as_str() {
        return Some(PathBuf::from(path));
    }
    first
        .as_object()
        .and_then(|obj| obj.get("path"))
        .and_then(|value| value.as_str())
        .map(PathBuf::from)
}

fn parse_decompile_cursor_argument(arguments: &[serde_json::Value]) -> Option<(Url, Position)> {
    if arguments.len() != 1 {
        return None;
    }
    let value = arguments.first()?;
    let uri = value.get("uri").and_then(parse_uri_value)?;
    let position = value.get("position").and_then(parse_position_value)?;
    Some((uri, position))
}

fn parse_decompile_file_argument(arguments: &[serde_json::Value]) -> Option<Url> {
    if arguments.len() != 1 {
        return None;
    }
    let value = arguments.first()?;
    value.get("uri").and_then(parse_uri_value)
}

fn parse_group_advice_argument(arguments: &[serde_json::Value]) -> Option<Option<Url>> {
    match arguments {
        [] => Some(None),
        [value] => Some(Some(value.get("uri").and_then(parse_uri_value)?)),
        _ => None,
    }
}

fn parse_uri_value(value: &serde_json::Value) -> Option<Url> {
    value.as_str().and_then(|raw| Url::parse(raw).ok())
}

fn parse_position_value(value: &serde_json::Value) -> Option<Position> {
    serde_json::from_value(value.clone()).ok()
}

fn span_location_value(
    sources: &miden_debug_types::DefaultSourceManager,
    span: miden_debug_types::SourceSpan,
) -> Option<serde_json::Value> {
    let source = sources.get(span.source_id()).ok()?;
    let uri = Url::parse(source.uri().as_str()).ok()?;
    let range = span_to_range(sources, span)?;
    Some(serde_json::json!({
        "uri": uri,
        "range": range,
    }))
}

fn grouped_procedures(diagnostics: &[masm_analysis::AdviceDiagnostic]) -> Vec<String> {
    let mut procedures = diagnostics
        .iter()
        .map(|diag| diag.procedure.as_str().to_string())
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    procedures.sort();
    procedures
}

fn decompilation_error_diagnostic_for_publish(
    err: &ProcedureDecompilationError,
) -> Option<tower_lsp::lsp_types::Diagnostic> {
    match err {
        ProcedureDecompilationError::CursorOutsideProcedure { diagnostic }
        | ProcedureDecompilationError::DecompilationFailed { diagnostic, .. } => {
            Some(diagnostic.clone())
        }
        ProcedureDecompilationError::DocumentUnavailable(_)
        | ProcedureDecompilationError::InvalidCursorPosition { .. } => None,
    }
}

fn decompile_command_error(err: ProcedureDecompilationError) -> tower_lsp::jsonrpc::Error {
    match err {
        ProcedureDecompilationError::DocumentUnavailable(message) => {
            tower_lsp::jsonrpc::Error::invalid_params(message)
        }
        ProcedureDecompilationError::InvalidCursorPosition { line, character } => {
            tower_lsp::jsonrpc::Error::invalid_params(format!(
                "invalid cursor position {line}:{character}"
            ))
        }
        ProcedureDecompilationError::CursorOutsideProcedure { diagnostic } => {
            jsonrpc_error_with_diagnostic(
                ErrorCode::InvalidRequest,
                "cursor is not inside a procedure",
                diagnostic,
            )
        }
        ProcedureDecompilationError::DecompilationFailed {
            message,
            diagnostic,
        } => jsonrpc_error_with_diagnostic(ErrorCode::InternalError, message, diagnostic),
    }
}

fn decompile_file_command_error(err: FileDecompilationError) -> tower_lsp::jsonrpc::Error {
    match err {
        FileDecompilationError::DocumentUnavailable(message) => {
            tower_lsp::jsonrpc::Error::invalid_params(message)
        }
    }
}

fn jsonrpc_error_with_diagnostic(
    code: ErrorCode,
    message: impl Into<String>,
    diagnostic: tower_lsp::lsp_types::Diagnostic,
) -> tower_lsp::jsonrpc::Error {
    tower_lsp::jsonrpc::Error {
        code,
        message: message.into().into(),
        data: Some(serde_json::json!({
            "diagnostic": diagnostic,
        })),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{Url, WorkspaceFolder};

    fn unique_temp_dir(name: &str) -> PathBuf {
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should move forward")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "masm-lsp-lsp-{name}-{}-{stamp}",
            std::process::id()
        ))
    }

    #[test]
    fn workspace_paths_use_workspace_folders_when_present() {
        let base = unique_temp_dir("workspace-folders");
        let first = base.join("first");
        let second = base.join("second");
        std::fs::create_dir_all(&first).expect("create first workspace");
        std::fs::create_dir_all(&second).expect("create second workspace");

        let params = InitializeParams {
            workspace_folders: Some(vec![
                WorkspaceFolder {
                    uri: Url::from_file_path(&first).expect("first URI"),
                    name: "first".to_string(),
                },
                WorkspaceFolder {
                    uri: Url::from_file_path(&second).expect("second URI"),
                    name: "second".to_string(),
                },
            ]),
            root_uri: Some(Url::from_file_path(base.join("fallback")).expect("fallback URI")),
            ..Default::default()
        };

        let paths = workspace_parse_paths_from_initialize(&params);
        assert_eq!(paths.len(), 2);
        assert!(paths.iter().all(|p| p.prefix.is_empty()));
        assert!(paths.iter().any(|p| p.root == first));
        assert!(paths.iter().any(|p| p.root == second));

        let _ = std::fs::remove_dir_all(base);
    }

    #[test]
    fn workspace_paths_fall_back_to_root_uri() {
        let base = unique_temp_dir("root-uri");
        std::fs::create_dir_all(&base).expect("create workspace");

        let params = InitializeParams {
            root_uri: Some(Url::from_file_path(&base).expect("workspace URI")),
            ..Default::default()
        };

        let paths = workspace_parse_paths_from_initialize(&params);
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].root, base);
        assert!(paths[0].prefix.is_empty());
    }
}
