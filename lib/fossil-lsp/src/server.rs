use dashmap::DashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::DocumentState;
use crate::handlers;

/// The main Fossil Language Server implementation
pub struct FossilLanguageServer {
    /// LSP client for sending messages/notifications
    pub client: Client,
    /// Cache of open documents
    pub documents: Arc<DashMap<Url, DocumentState>>,
    /// Workspace root path (for resolving relative paths)
    pub workspace_root: Arc<RwLock<Option<PathBuf>>>,
}

impl FossilLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
            workspace_root: Arc::new(RwLock::new(None)),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for FossilLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract workspace root for resolving relative paths in providers
        if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                if let Ok(mut workspace) = self.workspace_root.write() {
                    *workspace = Some(path.clone());
                    tracing::info!("Workspace root set to: {:?}", path);
                    // Change working directory to workspace root
                    if let Err(e) = std::env::set_current_dir(&path) {
                        tracing::warn!("Failed to change working directory to {:?}: {}", path, e);
                    }
                }
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: handlers::semantic_tokens::get_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        }
                    )
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "fossil-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Fossil LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // ===== Document Lifecycle =====

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        tracing::info!("Document opened: {}", params.text_document.uri);
        handlers::diagnostics::handle_document_change(
            &self.client,
            &self.documents,
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        tracing::info!("Document changed: {}", params.text_document.uri);
        if let Some(change) = params.content_changes.first() {
            handlers::diagnostics::handle_document_change(
                &self.client,
                &self.documents,
                params.text_document.uri,
                change.text.clone(),
                params.text_document.version,
            )
            .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        tracing::info!("Document saved: {}", params.text_document.uri);
        // Get the current version from cached document
        let current_version = self.documents
            .get(&params.text_document.uri)
            .map(|doc| doc.version)
            .unwrap_or(0);

        // Re-compile on save if we have the text
        if let Some(text) = params.text {
            handlers::diagnostics::handle_document_change(
                &self.client,
                &self.documents,
                params.text_document.uri,
                text,
                current_version,
            )
            .await;
        } else if let Some(doc) = self.documents.get(&params.text_document.uri) {
            // Re-compile with cached text
            handlers::diagnostics::handle_document_change(
                &self.client,
                &self.documents,
                params.text_document.uri.clone(),
                doc.text.clone(),
                doc.version,
            )
            .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        tracing::info!("Document closed: {}", params.text_document.uri);
        self.documents.remove(&params.text_document.uri);
    }

    // ===== Language Features =====

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        handlers::hover::handle_hover(&self.documents, params).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        handlers::goto_definition::handle_goto_definition(&self.documents, params).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        handlers::completion::handle_completion(&self.documents, params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        handlers::symbols::handle_document_symbol(&self.documents, params).await
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        handlers::inlay_hints::handle_inlay_hints(&self.documents, params).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        handlers::semantic_tokens::handle_semantic_tokens_full(&self.documents, params).await
    }
}
