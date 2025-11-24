use std::{collections::{HashMap, VecDeque}, sync::Arc};

use miden_assembly_syntax::{Parse, ParseOptions, SemanticAnalysisError};
use miden_debug_types::{DefaultSourceManager, SourceLanguage, SourceManager, Uri};
use miden_utils_diagnostics as diagnostics;
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        self, request::{GotoImplementationParams, GotoImplementationResponse},
        Diagnostic, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
        InitializeResult, InitializedParams, Location, ReferenceParams, ServerCapabilities,
        SymbolInformation, SymbolKind, TextDocumentContentChangeEvent,
        TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkspaceSymbolParams,
    },
    LanguageServer,
};
use tracing::{error, info};

use crate::{
    client::PublishDiagnostics,
    diagnostics::diagnostics_from_report,
    index::{build_document_symbols, DocumentSymbols, WorkspaceIndex},
    resolution::resolve_symbol_at_position,
    util::{guess_module_kinds, lsp_range_to_selection},
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
}

impl<C> Backend<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            sources: Arc::new(DefaultSourceManager::default()),
            documents: Arc::new(RwLock::new(HashMap::new())),
            symbols: Arc::new(RwLock::new(HashMap::new())),
            workspace: Arc::new(RwLock::new(WorkspaceIndex::default())),
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
}

impl<C> Backend<C>
where
    C: PublishDiagnostics,
{
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

        let diagnostics = match self.parse_module(&uri).await {
            Ok(_) => {
                let _ = self.parse_and_index(&uri).await;
                Vec::new()
            }
            Err(report) => diagnostics_from_report(&self.sources, &uri, report),
        };

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
        let miden_uri = to_miden_uri(uri);
        let source_file = self
            .sources
            .get_by_uri(&miden_uri)
            .ok_or_else(|| diagnostics::report!("file not loaded in source manager"))?;
        let text = source_file.as_str().to_owned();

        let mut candidates: VecDeque<_> = guess_module_kinds(uri, &text).into();
        let mut first_err: Option<miden_assembly_syntax::Report> = None;

        while let Some(kind) = candidates.pop_front() {
            let mut opts = ParseOptions::default();
            opts.kind = kind;
            if let Some(path) = module_path_from_uri(uri) {
                opts.path = Some(path.into());
            }
            match source_file
                .clone()
                .parse_with_options(self.sources.as_ref(), opts)
            {
                Ok(module) => return Ok(module),
                Err(report) => {
                    if let Some(err) = report.downcast_ref::<SemanticAnalysisError>() {
                        match err {
                            SemanticAnalysisError::UnexpectedEntrypoint { .. } => {
                                enqueue_kind(
                                    &mut candidates,
                                    miden_assembly_syntax::ast::ModuleKind::Executable,
                                );
                            }
                            SemanticAnalysisError::MissingEntrypoint => {
                                enqueue_kind(
                                    &mut candidates,
                                    miden_assembly_syntax::ast::ModuleKind::Library,
                                );
                            }
                            _ => {}
                        }
                    }
                    if first_err.is_none() {
                        first_err = Some(report);
                    }
                }
            }
        }

        Err(first_err.unwrap_or_else(|| diagnostics::report!("parse failed")))
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
            ws.update_document(uri.clone(), &doc_symbols.definitions, &doc_symbols.references);
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
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            implementation_provider: Some(lsp_types::ImplementationProviderCapability::Simple(
                true,
            )),
            references_provider: Some(lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("MASM LSP initialized");
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
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        self.handle_change(uri, version, params.content_changes)
            .await;
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
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let resolved =
            resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos).map_err(|e| {
                tower_lsp::jsonrpc::Error::invalid_params(format!(
                    "failed to resolve symbol: {e}"
                ))
            })?;
        let Some(symbol) = resolved else {
            // fallback: try lookup by token name
            if let Some(token) = self
                .sources
                .get_by_uri(&miden_debug_types::Uri::new(uri.as_str()))
                .and_then(|src| crate::util::extract_token_at_position(&src, pos))
            {
                let workspace = self.workspace.read().await;
                if let Some(loc) = workspace.definition_by_name(&token) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
                }
            }
            return Ok(None);
        };

        let workspace = self.workspace.read().await;
        if let Some(loc) = workspace
            .definition(&symbol.path)
            .or_else(|| workspace.definition_by_name(&symbol.name))
        {
            return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
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
        Ok(res.map(GotoDefinitionResponse::into))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let Some(doc) = self.get_or_parse_document(&uri).await else {
            return Ok(None);
        };

        let resolved =
            resolve_symbol_at_position(&uri, &doc.module, &self.sources, pos).map_err(|e| {
                tower_lsp::jsonrpc::Error::invalid_params(format!(
                    "failed to resolve symbol: {e}"
                ))
            })?;
        let Some(symbol) = resolved else {
            return Ok(None);
        };

        let workspace = self.workspace.read().await;
        let mut refs = workspace.references(&symbol.path);
        if params.context.include_declaration {
            if let Some(def) = workspace.definition(&symbol.path) {
                refs.push(def);
            }
        }

        Ok(Some(refs))
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
}

fn to_miden_uri(uri: &Url) -> Uri {
    Uri::new(uri.as_str())
}

fn enqueue_kind(
    queue: &mut VecDeque<miden_assembly_syntax::ast::ModuleKind>,
    kind: miden_assembly_syntax::ast::ModuleKind,
) {
    if !queue.contains(&kind) {
        queue.push_back(kind);
    }
}

fn module_path_from_uri(uri: &Url) -> Option<miden_assembly_syntax::ast::PathBuf> {
    let path = uri.path();
    let segments: Vec<&str> = path
        .split('/')
        .filter(|s| !s.is_empty())
        .collect();
    if segments.is_empty() {
        return None;
    }
    let mut buf = miden_assembly_syntax::ast::PathBuf::default();
    for (i, seg) in segments.iter().enumerate() {
        let cleaned = if i == segments.len() - 1 {
            seg.split('.').next().unwrap_or(*seg)
        } else {
            seg
        };
        if cleaned.is_empty() {
            continue;
        }
        buf.push(cleaned);
    }
    Some(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::guess_module_kinds;
    use tower_lsp::lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, TextDocumentItem,
        VersionedTextDocumentIdentifier,
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
        assert!(diags.is_empty());
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
        // Expect two publish calls: one on open, one on change.
        assert_eq!(published.len(), 2);
        let (_, diags_after_change, version) = &published[1];
        assert!(!diags_after_change.is_empty());
        assert_eq!(*version, Some(2));
    }

    #[test]
    fn guesses_module_kind_prefers_exec_when_begin_present() {
        let uri = Url::parse("file:///tmp/exec.masm").unwrap();
        let kinds = guess_module_kinds(&uri, "begin\nend\n");
        assert_eq!(
            kinds.first().copied(),
            Some(miden_assembly_syntax::ast::ModuleKind::Executable)
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
