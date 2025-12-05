use crate::client::PublishDiagnostics;
use crate::code_lens::collect_code_lenses;
use crate::cursor_resolution::resolve_symbol_at_position;
use crate::inlay_hints::collect_inlay_hints;
use crate::instruction_docs::get_instruction_hover;
use crate::util::{extract_token_at_position, extract_word_at_position, to_miden_uri};
use crate::InlayHintType;
use miden_debug_types::SourceManager;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        request::{GotoImplementationParams, GotoImplementationResponse},
        CodeLens, CodeLensOptions, CodeLensParams, GotoDefinitionParams, GotoDefinitionResponse,
        Hover, HoverContents, HoverParams, InitializeParams, InitializeResult, InitializedParams,
        InlayHint, InlayHintParams, Location, MarkupContent, MarkupKind, ReferenceParams,
        ServerCapabilities, SymbolInformation, SymbolKind, TextDocumentSyncCapability,
        TextDocumentSyncKind, WorkspaceSymbolParams,
    },
    LanguageServer,
};
use tracing::{debug, info};

use super::backend::Backend;
use super::config::{
    extract_code_lens_stack_effects, extract_inlay_hint_type, extract_library_paths,
    extract_tab_count,
};
use super::helpers::{extract_doc_comment, extract_procedure_signature, is_on_use_statement};

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
            hover_provider: Some(tower_lsp::lsp_types::HoverProviderCapability::Simple(true)),
            definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            implementation_provider: Some(
                tower_lsp::lsp_types::ImplementationProviderCapability::Simple(true),
            ),
            references_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            inlay_hint_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(false),
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

    async fn did_change_configuration(
        &self,
        params: tower_lsp::lsp_types::DidChangeConfigurationParams,
    ) {
        let mut cfg = self.config.write().await;
        if let Some(tab_count) = extract_tab_count(&params.settings) {
            cfg.inlay_hint_tabs = tab_count;
            info!("updated inlay hint tab padding: {}", tab_count);
        }
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
        let _ = self.handle_open(uri, version, text).await;
    }

    async fn did_change(&self, params: tower_lsp::lsp_types::DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        self.handle_change(uri, version, params.content_changes)
            .await;
        let _ = self.publish_diagnostics(params.text_document.uri).await;
    }

    async fn did_close(&self, params: tower_lsp::lsp_types::DidCloseTextDocumentParams) {
        self.handle_close(params.text_document.uri).await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let miden_uri = to_miden_uri(&uri);

        if let Some(source) = self.sources.get_by_uri(&miden_uri) {
            if is_on_use_statement(source.as_str(), pos) {
                if let Some(token) = extract_token_at_position(&source, pos) {
                    let normalized = token.trim_start_matches(':');
                    let workspace = self.workspace.read().await;

                    if let Some(loc) =
                        workspace
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
            }
        }

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

        let instruction_hovers_enabled = self.config.read().await.instruction_hovers_enabled;

        let source = self.sources.get_by_uri(&to_miden_uri(&uri));
        let token = source
            .as_ref()
            .and_then(|s| extract_token_at_position(s, pos));
        let word = source
            .as_ref()
            .and_then(|s| extract_word_at_position(s, pos));

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

        match resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos) {
            Ok(symbol) => {
                let workspace = self.workspace.read().await;
                let def_loc = workspace
                    .definition(symbol.path.as_str())
                    .or_else(|| workspace.definition_by_suffix(symbol.path.as_str()))
                    .or_else(|| workspace.definition_by_name(&symbol.name));

                let contract = workspace
                    .contracts()
                    .get_by_path(symbol.path.as_str())
                    .or_else(|| workspace.contracts().get_by_suffix(symbol.path.as_str()))
                    .or_else(|| workspace.contracts().get_by_name(&symbol.name))
                    .cloned();
                drop(workspace);

                if let Some(loc) = def_loc {
                    let def_uri = to_miden_uri(&loc.uri);
                    if let Some(source) = self.sources.get_by_uri(&def_uri) {
                        let content = source.as_str();
                        let def_line = loc.range.start.line as usize;
                        let signature = extract_procedure_signature(content, def_line);
                        let comment = extract_doc_comment(content, def_line);

                        let proc_name = signature
                            .as_ref()
                            .and_then(|s| s.lines().last())
                            .map(|line| line.trim().to_string())
                            .unwrap_or_else(|| symbol.name.clone());

                        let contract_sig =
                            contract.and_then(|c| c.format_signature_for_display(&proc_name));

                        let hover_text = match (&contract_sig, comment) {
                            (Some(csig), Some(doc)) => {
                                format!("```masm\n{csig}\n```\n\n---\n\n{doc}")
                            }
                            (Some(csig), None) => {
                                format!("```masm\n{csig}\n```")
                            }
                            (None, Some(doc)) if signature.is_some() => {
                                format!("```masm\n{}\n```\n\n---\n\n{doc}", signature.unwrap())
                            }
                            (None, None) if signature.is_some() => {
                                format!("```masm\n{}\n```", signature.unwrap())
                            }
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
        let config = self.snapshot_config().await;
        if !config.code_lens_stack_effects {
            return Ok(Some(vec![]));
        }
        let uri = params.text_document.uri;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let workspace = self.workspace.read().await;
        let contracts = workspace.contracts();
        let lenses = collect_code_lenses(&doc.module, self.sources.as_ref(), Some(contracts));
        Ok(Some(lenses))
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> Result<CodeLens> {
        Ok(params)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let config = self.snapshot_config().await;

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

        let hints = match config.inlay_hint_type {
            InlayHintType::Description => collect_inlay_hints(
                &doc.module,
                self.sources.as_ref(),
                &params.range,
                config.inlay_hint_tabs,
                source.as_str(),
            ),
            InlayHintType::Decompilation => {
                let workspace = self.workspace.read().await;
                let contracts = workspace.contracts();
                let result = crate::decompiler::collect_decompilation_hints(
                    &doc.module,
                    self.sources.as_ref(),
                    &uri,
                    &params.range,
                    config.inlay_hint_tabs,
                    source.as_str(),
                    Some(contracts),
                );
                drop(workspace);
                result.hints
            }
            InlayHintType::None => vec![],
        };
        Ok(Some(hints))
    }
}
