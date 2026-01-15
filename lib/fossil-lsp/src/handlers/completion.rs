use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::document::DocumentState;

/// Handle completion requests
pub async fn handle_completion(
    documents: &Arc<DashMap<Url, DocumentState>>,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    tracing::debug!("Completion request at {}:{}", position.line, position.character);

    // Get document
    let doc = match documents.get(&uri) {
        Some(doc) => doc,
        None => return Ok(None),
    };

    let mut items = vec![];

    // Add keywords
    for keyword in &["let", "type", "open", "as", "fn", "true", "false"] {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        });
    }

    // Add built-in types
    for ty in &["int", "bool", "string"] {
        items.push(CompletionItem {
            label: ty.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("built-in type".to_string()),
            ..Default::default()
        });
    }

    // TODO: Add identifiers from scope
    // TODO: Add module members if after `::`

    Ok(Some(CompletionResponse::Array(items)))
}
