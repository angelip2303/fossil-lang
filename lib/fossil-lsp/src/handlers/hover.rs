use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::document::DocumentState;

/// Handle hover requests
pub async fn handle_hover(
    documents: &Arc<DashMap<Url, DocumentState>>,
    params: HoverParams,
) -> Result<Option<Hover>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    tracing::debug!("Hover request at {}:{}", position.line, position.character);

    // Get document
    let doc = match documents.get(&uri) {
        Some(doc) => doc,
        None => return Ok(None),
    };

    // TODO: Implement actual hover logic
    // 1. Convert position to byte offset
    // 2. Find symbol at that position in AST
    // 3. Look up type information in gcx
    // 4. Format and return hover info

    // For now, return placeholder
    Ok(Some(Hover {
        contents: HoverContents::Scalar(MarkedString::String(
            "Hover support coming soon!".to_string(),
        )),
        range: None,
    }))
}
