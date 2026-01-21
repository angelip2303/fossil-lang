//! Inlay hints handler
//!
//! Provides parameter name hints for constructor calls, similar to Rust's inlay hints.

use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use fossil_lang::ast::thir::{ExprKind, TypeKind, TypedHir};
use fossil_lang::passes::GlobalContext;

use crate::document::DocumentState;

/// Handle inlay hints request
pub async fn handle_inlay_hints(
    documents: &Arc<DashMap<Url, DocumentState>>,
    params: InlayHintParams,
) -> Result<Option<Vec<InlayHint>>> {
    let uri = params.text_document.uri;
    let range = params.range;

    tracing::info!("Inlay hints request for {} at range {:?}", uri, range);

    let doc = match documents.get(&uri) {
        Some(doc) => doc,
        None => {
            tracing::info!("No document found for {}", uri);
            return Ok(None);
        }
    };

    // Get THIR for hints (prefer current, fall back to last valid)
    let thir_program = match doc.thir_for_completion() {
        Some(thir) => thir,
        None => {
            tracing::info!("No THIR available for {}", uri);
            return Ok(None);
        }
    };

    tracing::info!("THIR available, collecting hints...");

    let hints = collect_inlay_hints(
        &thir_program.thir,
        &thir_program.gcx,
        &doc.text,
        range,
    );

    tracing::info!("Collected {} hints", hints.len());

    if hints.is_empty() {
        Ok(None)
    } else {
        Ok(Some(hints))
    }
}

/// Collect inlay hints for the given range
fn collect_inlay_hints(
    thir: &TypedHir,
    gcx: &GlobalContext,
    source: &str,
    _range: Range,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    tracing::debug!("Scanning expressions for constructor calls");

    // Iterate through all expressions looking for constructor calls
    for (_expr_id, expr) in thir.exprs.iter() {
        if let ExprKind::Application { callee, args } = &expr.kind {
            // Check if callee is a record constructor
            let callee_expr = thir.exprs.get(*callee);
            tracing::debug!("Found Application, callee kind: {:?}", callee_expr.kind);

            if let ExprKind::Identifier(def_id) = &callee_expr.kind {
                let def = gcx.definitions.get(*def_id);
                let callee_name = gcx.interner.resolve(def.name);
                tracing::debug!("Callee is Identifier: {}", callee_name);

                // Check if this is a constructor (its type is a function returning a named type)
                let callee_ty = thir.types.get(callee_expr.ty);

                // Try to get constructor field names - we check both:
                // 1. If the return type is Named (direct constructor)
                // 2. If the callee name matches a type name (fallback for when types are resolved differently)
                let field_names = if let TypeKind::Function(_params, ret) = &callee_ty.kind {
                    let ret_ty = thir.types.get(*ret);
                    match &ret_ty.kind {
                        TypeKind::Named(type_def_id) => {
                            tracing::info!("Constructor returns Named type");
                            get_constructor_field_names(thir, gcx, *type_def_id)
                        }
                        TypeKind::Record(row) => {
                            // Direct record return - collect fields directly
                            tracing::info!("Constructor returns Record type directly");
                            Some(collect_field_names(gcx, row))
                        }
                        _ => {
                            // Try to find by callee name (fallback)
                            tracing::debug!("Trying fallback: find type by callee name '{}'", callee_name);
                            find_type_fields_by_name(thir, gcx, callee_name)
                        }
                    }
                } else {
                    tracing::debug!("Callee type is not Function");
                    None
                };

                if let Some(field_names) = field_names {
                    tracing::info!("Field names for {}: {:?}", callee_name, field_names);

                    // Add hints for each argument
                    for (i, arg) in args.iter().enumerate() {
                        if i >= field_names.len() {
                            break;
                        }

                        let arg_expr = thir.exprs.get(arg.value());
                        let arg_loc = &arg_expr.loc;

                        // Skip generated locations
                        if arg_loc.span.start == 0 && arg_loc.span.end == 0 {
                            continue;
                        }

                        // Convert byte offset to position
                        if let Some(position) = offset_to_position(source, arg_loc.span.start) {
                            tracing::info!("Adding hint '{}:' at line {} col {}",
                                field_names[i], position.line, position.character);
                            hints.push(InlayHint {
                                position,
                                label: InlayHintLabel::String(format!("{}:", field_names[i])),
                                kind: Some(InlayHintKind::PARAMETER),
                                padding_left: Some(false),
                                padding_right: Some(true),
                                text_edits: None,
                                tooltip: None,
                                data: None,
                            });
                        }
                    }
                }
            }
        }
    }

    hints
}

/// Get field names for a constructor from the type definition
fn get_constructor_field_names(
    thir: &TypedHir,
    gcx: &GlobalContext,
    type_def_id: fossil_lang::context::DefId,
) -> Option<Vec<String>> {
    let def = gcx.definitions.get(type_def_id);
    let type_name = def.name;

    // Find the type definition in THIR
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Type { name, ty } = &stmt.kind {
            if *name == type_name {
                let ty_data = thir.types.get(*ty);
                if let TypeKind::Record(row) = &ty_data.kind {
                    return Some(collect_field_names(gcx, row));
                }
            }
        }
    }

    None
}

/// Find type fields by looking up the type name in THIR statements
fn find_type_fields_by_name(
    thir: &TypedHir,
    gcx: &GlobalContext,
    type_name: &str,
) -> Option<Vec<String>> {
    // Look for a type statement with this name
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Type { name, ty } = &stmt.kind {
            let stmt_type_name = gcx.interner.resolve(*name);
            if stmt_type_name == type_name {
                let ty_data = thir.types.get(*ty);
                if let TypeKind::Record(row) = &ty_data.kind {
                    return Some(collect_field_names(gcx, row));
                }
            }
        }
    }

    None
}

/// Collect field names from a record row
fn collect_field_names(
    gcx: &GlobalContext,
    row: &fossil_lang::ast::thir::RecordRow,
) -> Vec<String> {
    let mut names = Vec::new();
    let mut current = row;

    loop {
        match current {
            fossil_lang::ast::thir::RecordRow::Empty => break,
            fossil_lang::ast::thir::RecordRow::Var(_) => break,
            fossil_lang::ast::thir::RecordRow::Extend { field, rest, .. } => {
                names.push(gcx.interner.resolve(*field).to_string());
                current = rest;
            }
        }
    }

    names
}

/// Convert byte offset to LSP position
fn offset_to_position(source: &str, offset: usize) -> Option<Position> {
    let mut current_offset = 0;
    for (line_num, line) in source.lines().enumerate() {
        let line_len = line.len();
        if current_offset + line_len >= offset {
            let character = offset - current_offset;
            return Some(Position {
                line: line_num as u32,
                character: character as u32,
            });
        }
        current_offset += line_len + 1; // +1 for newline
    }
    None
}
