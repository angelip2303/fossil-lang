use dashmap::DashMap;
use fossil_lang::ast::Loc;
use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;

use crate::compiler_bridge;
use crate::document::DocumentState;

/// Handle document changes and publish diagnostics
pub async fn handle_document_change(
    client: &Client,
    documents: &Arc<DashMap<Url, DocumentState>>,
    uri: Url,
    text: String,
    version: i32,
) {
    tracing::info!("Compiling document: {}", uri);

    // Compile the document
    let result = compiler_bridge::compile_document(uri.as_str(), &text);

    tracing::info!(
        "Compilation result: is_ok={}, errors_count={}",
        result.thir.is_some(),
        result.errors.len()
    );

    // Log each error for debugging
    for (i, err) in result.errors.iter().enumerate() {
        tracing::info!(
            "Error {}: {} at span {}..{} (source {})",
            i,
            err.message_with_interner(&result.gcx.interner),
            err.loc.span.start,
            err.loc.span.end,
            err.loc.source
        );
    }

    // Convert compiler errors to LSP diagnostics using the interner for proper symbol names
    let diagnostics: Vec<Diagnostic> = result
        .errors
        .iter()
        .map(|err| {
            let range = loc_to_range(&err.loc, &text);
            tracing::info!(
                "Diagnostic range: line {}:{} to {}:{}",
                range.start.line,
                range.start.character,
                range.end.line,
                range.end.character
            );
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: err.message_with_interner(&result.gcx.interner),
                source: Some("fossil-lsp".to_string()),
                ..Default::default()
            }
        })
        .collect();

    tracing::info!("Publishing {} diagnostics", diagnostics.len());

    // Store document state
    let doc_state = DocumentState {
        uri: uri.to_string(),
        text,
        version,
        thir: result.thir,
        errors: result.errors,
        gcx: result.gcx,
    };

    documents.insert(uri.clone(), doc_state);

    // Publish diagnostics to client
    client.publish_diagnostics(uri, diagnostics, Some(version)).await;
}

/// Convert Fossil Loc to LSP Range
fn loc_to_range(loc: &Loc, source: &str) -> Range {
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

