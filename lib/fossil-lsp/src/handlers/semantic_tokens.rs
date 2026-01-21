//! Semantic tokens handler
//!
//! Provides semantic highlighting for Fossil code, including:
//! - Named argument names (parameter style)
//! - Type names
//! - Function names
//! - Field names

use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use fossil_lang::ast::thir::{Argument, ExprKind, StmtKind, TypedHir};
use fossil_lang::passes::GlobalContext;

use crate::document::DocumentState;

/// Semantic token types used by the Fossil LSP
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::PARAMETER,   // 0: Named argument names
    SemanticTokenType::TYPE,        // 1: Type names
    SemanticTokenType::FUNCTION,    // 2: Function names
    SemanticTokenType::PROPERTY,    // 3: Field names
    SemanticTokenType::VARIABLE,    // 4: Variables
    SemanticTokenType::KEYWORD,     // 5: Keywords
    SemanticTokenType::STRING,      // 6: Strings
    SemanticTokenType::NUMBER,      // 7: Numbers
];

/// Semantic token modifiers
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
];

/// Get the semantic tokens legend for capability registration
pub fn get_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

/// Handle semantic tokens full request
pub async fn handle_semantic_tokens_full(
    documents: &Arc<DashMap<Url, DocumentState>>,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let uri = params.text_document.uri;

    let doc = match documents.get(&uri) {
        Some(doc) => doc,
        None => return Ok(None),
    };

    let thir_program = match doc.thir_for_completion() {
        Some(thir) => thir,
        None => return Ok(None),
    };

    let tokens = collect_semantic_tokens(
        &thir_program.thir,
        &thir_program.gcx,
        &doc.text,
    );

    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: tokens,
    })))
}

/// Collect semantic tokens from THIR
fn collect_semantic_tokens(
    thir: &TypedHir,
    gcx: &GlobalContext,
    doc_text: &str,
) -> Vec<SemanticToken> {
    let mut tokens = Vec::new();
    let mut raw_tokens: Vec<(usize, usize, u32, u32)> = Vec::new(); // (line, col, length, type)

    // Collect named argument names from function applications
    for (_, expr) in thir.exprs.iter() {
        if let ExprKind::Application { args, .. } = &expr.kind {
            for arg in args {
                if let Argument::Named { name, value } = arg {
                    let arg_name = gcx.interner.resolve(*name);
                    let value_expr = thir.exprs.get(*value);

                    // Find the argument name in the text before the value
                    if value_expr.loc.span.start > 0 && expr.loc.span.start < value_expr.loc.span.start {
                        let search_start = expr.loc.span.start;
                        let search_end = value_expr.loc.span.start.min(doc_text.len());

                        if let Some(search_text) = doc_text.get(search_start..search_end)
                            && let Some(name_pos) = search_text.rfind(arg_name) {
                                let abs_pos = search_start + name_pos;
                                if let Some((line, col)) = offset_to_line_col(doc_text, abs_pos) {
                                    raw_tokens.push((line, col, arg_name.len() as u32, 0)); // PARAMETER
                                }
                            }
                    }
                }
            }
        }
    }

    // Collect type names from type statements
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let StmtKind::Type { name, .. } = &stmt.kind {
            let type_name = gcx.interner.resolve(*name);

            // Find "type TypeName" in the statement
            if let Some(stmt_text) = doc_text.get(stmt.loc.span.start..stmt.loc.span.end.min(doc_text.len()))
                && let Some(type_keyword_pos) = stmt_text.find("type ") {
                    let name_start_in_stmt = type_keyword_pos + 5;
                    let abs_pos = stmt.loc.span.start + name_start_in_stmt;
                    if let Some((line, col)) = offset_to_line_col(doc_text, abs_pos) {
                        raw_tokens.push((line, col, type_name.len() as u32, 1)); // TYPE
                    }
                }
        }
    }

    // Sort by position (line, then column)
    raw_tokens.sort_by(|a, b| {
        if a.0 != b.0 {
            a.0.cmp(&b.0)
        } else {
            a.1.cmp(&b.1)
        }
    });

    // Convert to delta-encoded semantic tokens
    let mut prev_line = 0;
    let mut prev_col = 0;

    for (line, col, length, token_type) in raw_tokens {
        let delta_line = (line - prev_line) as u32;
        let delta_col = if line == prev_line {
            (col - prev_col) as u32
        } else {
            col as u32
        };

        tokens.push(SemanticToken {
            delta_line,
            delta_start: delta_col,
            length,
            token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = line;
        prev_col = col;
    }

    tokens
}

/// Convert byte offset to (line, column)
fn offset_to_line_col(text: &str, offset: usize) -> Option<(usize, usize)> {
    if offset > text.len() {
        return None;
    }

    let mut line = 0;
    let mut col = 0;

    for (i, ch) in text.char_indices() {
        if i >= offset {
            return Some((line, col));
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Some((line, col))
}
