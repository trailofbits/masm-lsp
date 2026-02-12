use crate::client::PublishDiagnostics;
use crate::code_lens::collect_code_lenses;
use crate::cursor_resolution::resolve_symbol_at_position;
use crate::inlay_hints::collect_inlay_hints;
use crate::util::{extract_token_at_position, to_miden_uri};
use crate::{InlayHintType, LibraryPath};
use miden_debug_types::SourceManager;
use std::{
    collections::HashSet,
    ffi::OsStr,
    path::{Path, PathBuf},
};
use tower_lsp::{
    jsonrpc::{ErrorCode, Result},
    lsp_types::{
        request::{GotoImplementationParams, GotoImplementationResponse},
        CodeLens, CodeLensOptions, CodeLensParams, ExecuteCommandOptions, ExecuteCommandParams,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
        InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintParams,
        Location, MarkupContent, MarkupKind, Position, ReferenceParams, ServerCapabilities,
        SymbolInformation, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
        WorkspaceSymbolParams,
    },
    LanguageServer,
};
use tracing::{debug, info};

use super::backend::{Backend, ProcedureDecompilationError};
use super::config::{
    extract_code_lens_stack_effects, extract_inlay_hint_type, extract_library_paths,
};
use super::helpers::{
    extract_doc_comment, extract_procedure_attributes, extract_procedure_signature,
    is_on_use_statement,
};

const CMD_SET_STDLIB_ROOT: &str = "masm-lsp.setStdlibRoot";
const CMD_DECOMPILE_PROCEDURE_AT_CURSOR: &str = "masm-lsp.decompileProcedureAtCursor";

#[tower_lsp::async_trait]
impl<C> LanguageServer for Backend<C>
where
    C: PublishDiagnostics,
{
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let workspace_paths = workspace_library_paths_from_initialize(&params);
        self.set_workspace_library_paths(workspace_paths).await;

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
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![
                    CMD_SET_STDLIB_ROOT.to_string(),
                    CMD_DECOMPILE_PROCEDURE_AT_CURSOR.to_string(),
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
            CMD_SET_STDLIB_ROOT => {
                let Some(raw_path) = parse_stdlib_root_argument(&params.arguments) else {
                    return Err(tower_lsp::jsonrpc::Error::invalid_params(
                        "masm-lsp.setStdlibRoot expects path as first argument",
                    ));
                };
                let root = normalize_stdlib_root(&raw_path);

                let mut cfg = self.config.write().await;
                cfg.library_paths = vec![LibraryPath {
                    root: root.clone(),
                    prefix: "std".to_string(),
                }];
                drop(cfg);

                info!("updated stdlib root via command: {}", root.display());
                self.load_configured_libraries().await;

                Ok(Some(serde_json::json!({
                    "root": root.to_string_lossy().to_string(),
                    "prefix": "std",
                })))
            }
            CMD_DECOMPILE_PROCEDURE_AT_CURSOR => {
                self.execute_decompile_procedure_at_cursor(&params).await
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
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
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
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
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
        let config = self.snapshot_config().await;
        let effective_library_paths = self
            .effective_library_paths_from(&config.library_paths)
            .await;
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
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

        let result = collect_inlay_hints(
            &doc.module,
            self.sources.clone(),
            &uri,
            &params.range,
            source.as_str(),
            &effective_library_paths,
            config.inlay_hint_type,
        );
        let _ = self
            .publish_diagnostics_with_extra(uri.clone(), result.diagnostics)
            .await;
        Ok(Some(result.hints))
    }
}

impl<C> Backend<C>
where
    C: PublishDiagnostics,
{
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
            .decompile_procedure_at_position(&uri, position, &effective_library_paths)
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
}

fn workspace_library_paths_from_initialize(params: &InitializeParams) -> Vec<LibraryPath> {
    let mut roots = Vec::new();

    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }

    if roots.is_empty() {
        if let Some(root_uri) = &params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                roots.push(path);
            }
        }
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

fn parse_stdlib_root_argument(arguments: &[serde_json::Value]) -> Option<PathBuf> {
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

fn parse_uri_value(value: &serde_json::Value) -> Option<Url> {
    value.as_str().and_then(|raw| Url::parse(raw).ok())
}

fn parse_position_value(value: &serde_json::Value) -> Option<Position> {
    serde_json::from_value(value.clone()).ok()
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

fn normalize_stdlib_root(path: &Path) -> PathBuf {
    let normalized = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if is_stdlib_asm_path(&normalized) {
        return normalized;
    }
    if is_stdlib_path(&normalized) {
        return normalized.join("asm");
    }
    normalized.join("stdlib").join("asm")
}

fn is_stdlib_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new("stdlib"))
}

fn is_stdlib_asm_path(path: &Path) -> bool {
    path.file_name() == Some(OsStr::new("asm"))
        && path.parent().and_then(|p| p.file_name()) == Some(OsStr::new("stdlib"))
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

        let paths = workspace_library_paths_from_initialize(&params);
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

        let paths = workspace_library_paths_from_initialize(&params);
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0].root, base);
        assert!(paths[0].prefix.is_empty());
    }
}
