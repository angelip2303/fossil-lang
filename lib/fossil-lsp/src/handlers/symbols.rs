use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::document::DocumentState;

/// Handle document symbol requests
///
/// NOTE: This handler currently returns an empty list because the THIR
/// representation doesn't expose the original AST symbols directly.
///
/// TODO: Either:
/// 1. Store the original AST alongside THIR in DocumentState
/// 2. Extract symbol information from THIR representation
/// 3. Add a symbols pass in the compiler that preserves this info
pub async fn handle_document_symbol(
    documents: &Arc<DashMap<Url, DocumentState>>,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let uri = params.text_document.uri;

    tracing::debug!("Document symbols request for: {}", uri);

    // Get document
    let _doc = match documents.get(&uri) {
        Some(doc) => doc,
        None => return Ok(None),
    };

    // TODO: Extract symbols from THIR or store original AST
    // For now, return empty list
    let symbols = vec![];

    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
}

/// Convert Fossil Loc to LSP Range
fn loc_to_range(loc: &fossil_lang::ast::Loc, source: &str) -> Range {
    let start = offset_to_position(source, loc.span.start);
    let end = offset_to_position(source, loc.span.end);

    Range {
        start,
        end,
    }
}

/// Convert byte offset to LSP Position
fn offset_to_position(source: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut col = 0;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Position {
        line: line as u32,
        character: col as u32,
    }
}
