use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use fossil_lang::ast::thir::{Argument, ExprId, ExprKind, RecordRow, StmtKind, TypeId, TypeKind, TypeVar, TypedHir};
use fossil_lang::context::DefId;
use fossil_lang::passes::GlobalContext;

use crate::document::DocumentState;

/// Characters that should not trigger hover
const PUNCTUATION: &[char] = &[',', '(', ')', '[', ']', '{', '}', '|', '>', '<', '=', ';', ':'];

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

    // Get THIR program
    let thir_program = match &doc.thir {
        Some(thir) => thir,
        None => return Ok(None),
    };

    // Convert position to byte offset
    let offset = match doc.position_to_offset(position.line, position.character) {
        Some(o) => o,
        None => return Ok(None),
    };

    // Check if we're hovering over punctuation - skip if so
    if let Some(ch) = doc.text.chars().nth(offset)
        && (PUNCTUATION.contains(&ch) || ch.is_whitespace()) {
            return Ok(None);
        }

    // Try to find a let binding at this offset (for variable hover)
    if let Some((name, ty_id)) = find_let_binding_at_offset(&thir_program.thir, offset, &thir_program.gcx) {
        let type_str = format_type_expanded(&thir_program.thir, &thir_program.gcx, ty_id);
        let content = format!("```fossil\nlet {}: {}\n```", name, type_str);
        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }));
    }

    // Try to find a type definition statement (only if cursor is on the type name)
    if let Some((name, ty_id, def_id)) = find_type_stmt_at_offset(&thir_program.thir, offset, &thir_program.gcx, &doc.text) {
        // Use format_type_definition to show full metadata for type definitions
        let type_str = format_type_definition(&thir_program.thir, &thir_program.gcx, ty_id, def_id);
        let content = format!("```fossil\ntype {} = {}\n```", name, type_str);
        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }));
    }

    // Check if cursor is on a provider argument (e.g., string in csv!("..."))
    if let Some(literal_type) = find_provider_arg_at_offset(&doc.text, offset) {
        let content = format!("```fossil\n(provider argument) {}\n```", literal_type);
        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }));
    }

    // Check if cursor is on a named argument name (e.g., "left_on" in "left_on: _.id")
    if let Some((arg_name, arg_type)) = find_named_arg_at_offset(&thir_program.thir, &thir_program.gcx, &doc.text, offset) {
        let type_str = format_type_expanded(&thir_program.thir, &thir_program.gcx, arg_type);
        let content = format!("```fossil\n(named argument) {}: {}\n```", arg_name, type_str);
        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }));
    }

    // Find expression at this position
    let expr_id = match find_expr_at_offset(&thir_program.thir, offset) {
        Some(id) => id,
        None => return Ok(None),
    };

    // Get the expression and format based on its kind
    let expr = thir_program.thir.exprs.get(expr_id);
    tracing::info!("Found expr at offset {}: kind={:?}, span={}..{}",
        offset,
        std::mem::discriminant(&expr.kind),
        expr.loc.span.start,
        expr.loc.span.end
    );

    // Check if this is an Identifier and cursor is on a module part of a qualified path
    if let ExprKind::Identifier(def_id) = &expr.kind {
        match analyze_path_position(&doc.text, offset, expr.loc.span.start, expr.loc.span.end) {
            PathComponent::Module(module_name) => {
                // Cursor is on a module part - show module info
                let content = format!("```fossil\n(module) {}\n```", module_name);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                }));
            }
            PathComponent::Identifier => {
                // Cursor is on the final identifier - show normal hover
                let def = thir_program.gcx.definitions.get(*def_id);
                let name = thir_program.gcx.interner.resolve(def.name);
                let type_str = format_type_expanded(&thir_program.thir, &thir_program.gcx, expr.ty);
                let content = format!("```fossil\n{}: {}\n```", name, type_str);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                }));
            }
        }
    }

    // Check if this is a Function and cursor is on a parameter
    if let ExprKind::Function { params, .. } = &expr.kind
        && let Some((param_name, param_type)) = find_param_at_offset(
            &thir_program.thir,
            &thir_program.gcx,
            params,
            &doc.text,
            offset,
            expr.loc.span.start,
            expr.ty,
        ) {
            let type_str = format_type_expanded(&thir_program.thir, &thir_program.gcx, param_type);
            let content = format!("```fossil\n(parameter) {}: {}\n```", param_name, type_str);
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            }));
        }

    let content = format_expr_hover(&thir_program.thir, &thir_program.gcx, expr_id, expr.ty);

    Ok(Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    }))
}

/// Format hover content based on expression kind
fn format_expr_hover(thir: &TypedHir, gcx: &GlobalContext, expr_id: ExprId, ty_id: TypeId) -> String {
    let expr = thir.exprs.get(expr_id);

    match &expr.kind {
        ExprKind::Identifier(def_id) => {
            // Show identifier name with its resolved type
            let def = gcx.definitions.get(*def_id);
            let name = gcx.interner.resolve(def.name);
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n{}: {}\n```", name, type_str)
        }
        ExprKind::FieldAccess { field, .. } => {
            // Show field name with its resolved type
            let field_name = gcx.interner.resolve(*field);
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n(field) {}: {}\n```", field_name, type_str)
        }
        ExprKind::Application { callee, .. } => {
            // For function applications, show the result type
            let type_str = format_type_expanded(thir, gcx, ty_id);

            // Try to get the function name for better context
            let callee_expr = thir.exprs.get(*callee);
            if let ExprKind::Identifier(def_id) = &callee_expr.kind {
                let def = gcx.definitions.get(*def_id);
                let name = gcx.interner.resolve(def.name);
                format!("```fossil\n{}(...): {}\n```", name, type_str)
            } else if let ExprKind::FieldAccess { field, .. } = &callee_expr.kind {
                let field_name = gcx.interner.resolve(*field);
                format!("```fossil\n...::{}(...): {}\n```", field_name, type_str)
            } else {
                format!("```fossil\n{}\n```", type_str)
            }
        }
        ExprKind::Function { params, .. } => {
            // Show function signature
            let type_str = format_type_expanded(thir, gcx, ty_id);
            if params.is_empty() {
                format!("```fossil\nfn(): {}\n```", type_str)
            } else {
                format!("```fossil\n{}\n```", type_str)
            }
        }
        ExprKind::Literal(_) => {
            // Show literal type
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n(literal) {}\n```", type_str)
        }
        ExprKind::Record(fields) => {
            // Show record type with field count
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n(record with {} fields)\n{}\n```", fields.len(), type_str)
        }
        ExprKind::List(items) => {
            // Show list type with item count
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n(list with {} items)\n{}\n```", items.len(), type_str)
        }
        _ => {
            // Default: just show the type
            let type_str = format_type_expanded(thir, gcx, ty_id);
            format!("```fossil\n{}\n```", type_str)
        }
    }
}

/// Find a let binding at a given byte offset (searches recursively in blocks)
fn find_let_binding_at_offset(thir: &TypedHir, offset: usize, gcx: &GlobalContext) -> Option<(String, TypeId)> {
    // First search root statements
    if let Some(result) = find_let_in_stmts(&thir.root, thir, offset, gcx) {
        return Some(result);
    }

    // Then search inside block expressions
    for (_, expr) in thir.exprs.iter() {
        if let ExprKind::Block { stmts } = &expr.kind
            && let Some(result) = find_let_in_stmts(stmts, thir, offset, gcx) {
                return Some(result);
            }
    }

    None
}

/// Search for a let binding in a list of statements
fn find_let_in_stmts(
    stmts: &[fossil_lang::ast::thir::StmtId],
    thir: &TypedHir,
    offset: usize,
    gcx: &GlobalContext,
) -> Option<(String, TypeId)> {
    for stmt_id in stmts {
        let stmt = thir.stmts.get(*stmt_id);

        // Check if offset is within this statement's span
        if stmt.loc.span.start <= offset && offset <= stmt.loc.span.end
            && let StmtKind::Let { name, value } = &stmt.kind {
                // Get the name's position - check if cursor is on the name part
                let name_str = gcx.interner.resolve(*name);
                let stmt_text_start = stmt.loc.span.start;

                // The name typically appears after "let " (4 chars) in the statement
                let approx_name_start = stmt_text_start + 4; // "let "
                let approx_name_end = approx_name_start + name_str.len();

                if offset >= approx_name_start && offset <= approx_name_end + 1 {
                    let value_expr = thir.exprs.get(*value);
                    return Some((name_str.to_string(), value_expr.ty));
                }
            }
    }
    None
}

/// Find a function parameter at a given byte offset by analyzing the document text
///
/// Returns (param_name, param_type) if cursor is on a parameter
fn find_param_at_offset(
    thir: &TypedHir,
    gcx: &GlobalContext,
    params: &[fossil_lang::ast::thir::Param],
    doc_text: &str,
    offset: usize,
    fn_span_start: usize,
    fn_ty: TypeId,
) -> Option<(String, TypeId)> {
    if params.is_empty() {
        return None;
    }

    // Get the function type to extract parameter types
    let fn_type = thir.types.get(fn_ty);
    let param_types = match &fn_type.kind {
        TypeKind::Function(param_tys, _) => param_tys.clone(),
        _ => return None,
    };

    // Get the function text from span start
    let fn_text = doc_text.get(fn_span_start..)?;

    // Find "fn (" to locate parameters
    let fn_paren_start = fn_text.find("fn ")?.checked_add(3)?;
    let paren_open = fn_text[fn_paren_start..].find('(')? + fn_paren_start;
    let paren_close = fn_text[paren_open..].find(')')? + paren_open;

    // Calculate absolute positions
    let params_start = fn_span_start + paren_open + 1;
    let params_end = fn_span_start + paren_close;

    // Check if cursor is within the parameters section
    if offset < params_start || offset > params_end {
        return None;
    }

    // Parse parameter positions from the text
    let params_text = &fn_text[paren_open + 1..paren_close];
    let mut current_pos = params_start;

    for (i, param) in params.iter().enumerate() {
        let param_name = gcx.interner.resolve(param.name);

        // Find this parameter name in the params text
        let relative_offset = current_pos - params_start;
        if let Some(name_pos) = params_text[relative_offset..].find(param_name) {
            let abs_name_start = current_pos + name_pos;
            let abs_name_end = abs_name_start + param_name.len();

            // Check if cursor is on this parameter name
            if offset >= abs_name_start && offset <= abs_name_end {
                // Get the corresponding type
                if let Some(param_ty) = param_types.get(i) {
                    return Some((param_name.to_string(), *param_ty));
                }
            }

            // Move current_pos past this parameter (look for comma or end)
            if let Some(comma_pos) = params_text[relative_offset + name_pos..].find(',') {
                current_pos = current_pos + name_pos + comma_pos + 1;
            } else {
                break;
            }
        }
    }

    None
}

/// Find a type definition statement at a given byte offset
/// Returns (type_name, type_id, optional_def_id) only if cursor is on the type name
fn find_type_stmt_at_offset(thir: &TypedHir, offset: usize, gcx: &GlobalContext, doc_text: &str) -> Option<(String, TypeId, Option<DefId>)> {
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);

        // Check if offset is within this statement's span
        if stmt.loc.span.start <= offset && offset <= stmt.loc.span.end
            && let StmtKind::Type { name, ty } = &stmt.kind {
                let name_str = gcx.interner.resolve(*name).to_string();

                // Check if cursor is specifically on the type name, not on the RHS
                // The type name appears after "type " (5 chars) at the start of the statement
                let stmt_text_start = stmt.loc.span.start;

                // Find where the type name is in the statement text
                // Format is: "type TypeName = ..."
                if let Some(stmt_text) = doc_text.get(stmt.loc.span.start..stmt.loc.span.end) {
                    // Find "type " keyword
                    if let Some(type_keyword_end) = stmt_text.find("type ").map(|i| i + 5) {
                        let name_start = stmt_text_start + type_keyword_end;
                        let name_end = name_start + name_str.len();

                        // Only return if cursor is on the type name
                        if offset >= name_start && offset <= name_end {
                            let def_id = gcx.definitions.get_by_symbol(*name).map(|d| d.id());
                            return Some((name_str, *ty, def_id));
                        }
                    }
                }
            }
    }
    None
}

/// Find the expression at a given byte offset
fn find_expr_at_offset(thir: &TypedHir, offset: usize) -> Option<ExprId> {
    let mut best_match: Option<(ExprId, usize)> = None;
    let mut candidates: Vec<(ExprId, usize, usize, &str)> = Vec::new();

    for (id, expr) in thir.exprs.iter() {
        let loc = &expr.loc;
        // Skip generated locations
        if loc.span.start == 0 && loc.span.end == 0 {
            continue;
        }

        // Check if offset is within this expression's span
        if loc.span.start <= offset && offset <= loc.span.end {
            let span_size = loc.span.end - loc.span.start;
            let kind_name = match &expr.kind {
                ExprKind::Identifier(_) => "Identifier",
                ExprKind::Function { .. } => "Function",
                ExprKind::Application { .. } => "Application",
                ExprKind::FieldAccess { .. } => "FieldAccess",
                ExprKind::Record(_) => "Record",
                ExprKind::List(_) => "List",
                ExprKind::Literal(_) => "Literal",
                ExprKind::Unit => "Unit",
                ExprKind::Block { .. } => "Block",
                ExprKind::StringInterpolation { .. } => "StringInterpolation",
                ExprKind::FieldSelector { .. } => "FieldSelector",
            };
            candidates.push((id, loc.span.start, loc.span.end, kind_name));

            // Prefer smaller (more specific) spans
            match best_match {
                None => best_match = Some((id, span_size)),
                Some((_, best_size)) if span_size < best_size => {
                    best_match = Some((id, span_size));
                }
                _ => {}
            }
        }
    }

    // Log all candidates for debugging
    if !candidates.is_empty() {
        tracing::info!("Candidates at offset {}: {:?}", offset, candidates);
    }

    best_match.map(|(id, _)| id)
}

/// Helper to convert type variable numbers to readable letters
struct TypeVarNamer {
    mapping: HashMap<usize, String>,
    next_letter: u8,
}

impl TypeVarNamer {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
            next_letter: b'a',
        }
    }

    fn get_name(&mut self, var: &TypeVar) -> String {
        if let Some(name) = self.mapping.get(&var.0) {
            return name.clone();
        }

        let name = if self.next_letter <= b'z' {
            let c = self.next_letter as char;
            self.next_letter += 1;
            format!("'{}", c)
        } else {
            // Fallback to numbered if we run out of letters
            format!("'t{}", self.mapping.len())
        };

        self.mapping.insert(var.0, name.clone());
        name
    }
}

/// Format a type in a human-readable way
/// Uses compact representation for small types, expanded for complex ones
fn format_type_expanded(thir: &TypedHir, gcx: &GlobalContext, ty_id: TypeId) -> String {
    let mut namer = TypeVarNamer::new();
    format_type_smart(thir, gcx, ty_id, &mut namer, 0)
}

/// Smart type formatting that adapts based on complexity
/// - Named types: show name, not expanded definition
/// - Small records: inline { field: type }
/// - Large records: multiline with indentation
/// - Functions: clean arrow notation
fn format_type_smart(
    thir: &TypedHir,
    gcx: &GlobalContext,
    ty_id: TypeId,
    namer: &mut TypeVarNamer,
    depth: usize,
) -> String {
    let ty = thir.types.get(ty_id);
    match &ty.kind {
        TypeKind::Primitive(p) => match p {
            fossil_lang::ast::ast::PrimitiveType::Int => "int".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Float => "float".to_string(),
            fossil_lang::ast::ast::PrimitiveType::String => "string".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Bool => "bool".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Unit => "unit".to_string(),
        },
        TypeKind::Var(v) => namer.get_name(v),
        TypeKind::Named(def_id) => {
            // Always show the type name for Named types (cleaner than expanding)
            let def = gcx.definitions.get(*def_id);
            gcx.interner.resolve(def.name).to_string()
        }
        TypeKind::App { ctor, args } => {
            let ctor_def = gcx.definitions.get(*ctor);
            let ctor_name = gcx.interner.resolve(ctor_def.name);
            if args.is_empty() {
                ctor_name.to_string()
            } else {
                let arg_strs: Vec<_> = args
                    .iter()
                    .map(|arg| format_type_smart(thir, gcx, *arg, namer, depth + 1))
                    .collect();
                format!("{}<{}>", ctor_name, arg_strs.join(", "))
            }
        }
        TypeKind::Function(params, ret) => {
            format_function_type(thir, gcx, params, *ret, namer, depth)
        }
        TypeKind::Record(row) => {
            format_record_smart(thir, gcx, row, namer, depth)
        }
        TypeKind::FieldSelector { record_ty, field_ty, field } => {
            let record_str = format_type_smart(thir, gcx, *record_ty, namer, depth + 1);
            let field_str = format_type_smart(thir, gcx, *field_ty, namer, depth + 1);
            let field_name = gcx.interner.resolve(*field);
            format!("FieldSelector<{}, {} = {}>", record_str, field_name, field_str)
        }
    }
}

/// Format function type with clean notation
fn format_function_type(
    thir: &TypedHir,
    gcx: &GlobalContext,
    params: &[TypeId],
    ret: TypeId,
    namer: &mut TypeVarNamer,
    depth: usize,
) -> String {
    let ret_str = format_type_smart(thir, gcx, ret, namer, depth + 1);

    if params.is_empty() {
        format!("() -> {}", ret_str)
    } else if params.len() == 1 {
        // Single param: show without parens if simple
        let param_str = format_type_smart(thir, gcx, params[0], namer, depth + 1);
        let param_ty = thir.types.get(params[0]);
        if matches!(param_ty.kind, TypeKind::Function(_, _)) {
            // Wrap function params in parens for clarity
            format!("({}) -> {}", param_str, ret_str)
        } else {
            format!("{} -> {}", param_str, ret_str)
        }
    } else {
        let param_strs: Vec<_> = params
            .iter()
            .map(|p| format_type_smart(thir, gcx, *p, namer, depth + 1))
            .collect();
        format!("({}) -> {}", param_strs.join(", "), ret_str)
    }
}

/// Format record type - inline for small, abbreviated for nested, multiline for top-level
fn format_record_smart(
    thir: &TypedHir,
    gcx: &GlobalContext,
    row: &RecordRow,
    namer: &mut TypeVarNamer,
    depth: usize,
) -> String {
    let fields = collect_record_fields(row);

    if fields.is_empty() {
        return "{}".to_string();
    }

    // For deeply nested records, just show field names without types
    if depth >= 2 {
        let field_names: Vec<_> = fields
            .iter()
            .map(|(field_sym, _)| gcx.interner.resolve(*field_sym).to_string())
            .collect();
        return format!("{{ {} }}", field_names.join(", "));
    }

    // Calculate field strings
    let field_strs: Vec<(String, String)> = fields
        .iter()
        .map(|(field_sym, field_ty)| {
            let name = gcx.interner.resolve(*field_sym).to_string();
            let ty = format_type_smart(thir, gcx, *field_ty, namer, depth + 1);
            (name, ty)
        })
        .collect();

    // Use inline for small records (<=3 fields and short total length)
    let total_len: usize = field_strs.iter().map(|(n, t)| n.len() + t.len() + 4).sum();
    let use_inline = fields.len() <= 3 && total_len < 50;

    if use_inline || depth > 0 {
        let parts: Vec<_> = field_strs
            .iter()
            .map(|(n, t)| format!("{}: {}", n, t))
            .collect();
        format!("{{ {} }}", parts.join(", "))
    } else {
        // Multiline format for top-level large records
        let indent = "    ";
        let mut result = String::from("{\n");

        for (i, (name, ty)) in field_strs.iter().enumerate() {
            result.push_str(&format!("{}  {}: {}", indent, name, ty));
            if i < field_strs.len() - 1 {
                result.push(',');
            }
            result.push('\n');
        }

        result.push_str(indent);
        result.push('}');
        result
    }
}

/// Format a type definition with full metadata (for type statement hovers)
/// This shows the expanded record with all attributes like #[uri(...)]
fn format_type_definition(
    thir: &TypedHir,
    gcx: &GlobalContext,
    ty_id: TypeId,
    type_def_id: Option<DefId>,
) -> String {
    let mut namer = TypeVarNamer::new();
    let ty = thir.types.get(ty_id);

    match &ty.kind {
        TypeKind::Record(row) => {
            format_record_with_metadata_and_namer(thir, gcx, row, type_def_id, &mut namer)
        }
        _ => format_type_smart(thir, gcx, ty_id, &mut namer, 0)
    }
}


/// Format a record type with metadata (attributes like #[uri(...)])
fn format_record_with_metadata_and_namer(
    thir: &TypedHir,
    gcx: &GlobalContext,
    row: &RecordRow,
    type_def_id: Option<DefId>,
    namer: &mut TypeVarNamer,
) -> String {
    let fields = collect_record_fields(row);

    if fields.is_empty() {
        return "{}".to_string();
    }

    // Get type metadata if available
    let type_metadata = type_def_id.and_then(|def_id| gcx.type_metadata.get(&def_id));

    let mut result = String::from("{\n");

    for (i, (field_sym, field_ty)) in fields.iter().enumerate() {
        let field_name = gcx.interner.resolve(*field_sym);
        let field_type_str = format_type_smart(thir, gcx, *field_ty, namer, 1);

        // Check if this field has a URI attribute
        if let Some(metadata) = &type_metadata
            && let Some(field_meta) = metadata.get_field(*field_sym) {
                // Look for "uri" attribute
                let uri_sym = gcx.interner.lookup("uri");
                if let Some(uri_sym) = uri_sym
                    && let Some(uri_attr) = field_meta.get_attribute(uri_sym) {
                        // Get the first argument as the URI string
                        if let Some(fossil_lang::ast::ast::Literal::String(uri_str_sym)) =
                            uri_attr.args.first()
                        {
                            let uri_str = gcx.interner.resolve(*uri_str_sym);
                            result.push_str(&format!("    #[uri(\"{}\")]\n", uri_str));
                        }
                    }
            }

        result.push_str(&format!("    {}: {}", field_name, field_type_str));

        if i < fields.len() - 1 {
            result.push(',');
        }
        result.push('\n');
    }

    result.push('}');
    result
}

/// Collect all fields from a record row into a vector
fn collect_record_fields(row: &RecordRow) -> Vec<(fossil_lang::context::Symbol, TypeId)> {
    let mut fields = Vec::new();
    let mut current = row;

    loop {
        match current {
            RecordRow::Empty => break,
            RecordRow::Var(_) => break,
            RecordRow::Extend { field, ty, rest } => {
                fields.push((*field, *ty));
                current = rest;
            }
        }
    }

    // Reverse to get fields in declaration order
    fields.reverse();
    fields
}

/// Result of analyzing cursor position in a qualified path
#[derive(Debug)]
enum PathComponent {
    /// Cursor is on the module part (e.g., "Rdf" in "Rdf::serialize")
    Module(String),
    /// Cursor is on the final identifier (e.g., "serialize" in "Rdf::serialize")
    Identifier,
}

/// Analyze if cursor is on a module part of a qualified path
///
/// Given text like "Rdf::serialize" and an offset, determines which component
/// the cursor is over.
fn analyze_path_position(text: &str, offset: usize, expr_span_start: usize, expr_span_end: usize) -> PathComponent {
    // Extract the path text from the expression span
    let path_text = if expr_span_end <= text.len() {
        &text[expr_span_start..expr_span_end]
    } else {
        return PathComponent::Identifier;
    };

    // Find :: separators in the path
    let mut separators: Vec<usize> = Vec::new();
    let mut i = 0;
    let bytes = path_text.as_bytes();
    while i < bytes.len().saturating_sub(1) {
        if bytes[i] == b':' && bytes[i + 1] == b':' {
            separators.push(expr_span_start + i);
            i += 2;
        } else {
            i += 1;
        }
    }

    // If no :: separators, it's a simple path - cursor is on identifier
    if separators.is_empty() {
        return PathComponent::Identifier;
    }

    // Find which component the cursor is on
    // The cursor is on a module if it's before the last :: separator
    let last_separator = separators.last().unwrap();

    if offset < *last_separator {
        // Cursor is before the last ::, so it's on a module
        // Find which module component
        let mut component_start = expr_span_start;
        for sep in &separators {
            if offset < *sep {
                // Cursor is in this component
                let component_end = *sep;
                let module_name = text[component_start..component_end].to_string();
                return PathComponent::Module(module_name);
            }
            component_start = sep + 2; // Skip ::
        }
        // Should not reach here
        PathComponent::Identifier
    } else {
        // Cursor is on or after the last ::, so it's on the final identifier
        PathComponent::Identifier
    }
}

/// Check if cursor is on a provider argument (e.g., string literal in csv!("..."))
/// Returns the type name of the literal if found
fn find_provider_arg_at_offset(text: &str, offset: usize) -> Option<&'static str> {
    // Find the line containing the offset
    let line_start = text[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = text[offset..].find('\n').map(|i| offset + i).unwrap_or(text.len());
    let line = &text[line_start..line_end];

    // Check if this line contains a provider invocation (name!)
    if !line.contains('!') {
        return None;
    }

    // Check if cursor is inside a string literal
    let relative_offset = offset - line_start;
    let bytes = line.as_bytes();

    // Find if we're inside quotes
    let mut in_string = false;
    let mut string_start = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        if byte == b'"' {
            if in_string {
                // End of string - check if cursor was inside
                if relative_offset > string_start && relative_offset <= i {
                    return Some("string");
                }
                in_string = false;
            } else {
                // Start of string
                in_string = true;
                string_start = i;
            }
        }
    }

    // Check if cursor is on a number (integer literal)
    if relative_offset < bytes.len() {
        let ch = bytes[relative_offset];
        if ch.is_ascii_digit() {
            // Verify this is within a provider call (after !)
            if line[..relative_offset].contains('!') {
                return Some("int");
            }
        }
    }

    None
}

/// Find a named argument at a given byte offset
/// Returns (argument_name, argument_type) if cursor is on a named argument name
fn find_named_arg_at_offset(
    thir: &TypedHir,
    gcx: &GlobalContext,
    doc_text: &str,
    offset: usize,
) -> Option<(String, TypeId)> {
    // Search all expressions for Application with named arguments
    for (_, expr) in thir.exprs.iter() {
        if let ExprKind::Application { args, .. } = &expr.kind {
            // Check if this application contains the offset
            if expr.loc.span.start > offset || offset > expr.loc.span.end {
                continue;
            }

            // Look for named arguments
            for arg in args {
                if let Argument::Named { name, value } = arg {
                    let arg_name = gcx.interner.resolve(*name);

                    // Get the value expression to find its location
                    let value_expr = thir.exprs.get(*value);

                    // The named argument name appears before the value
                    // Format: "name: value"
                    // We need to find "name:" in the text before the value
                    if value_expr.loc.span.start > 0 {
                        // Search backwards from value start for the argument name
                        let search_start = expr.loc.span.start;
                        let search_end = value_expr.loc.span.start;

                        if search_end <= doc_text.len() {
                            let search_text = &doc_text[search_start..search_end];

                            // Find "arg_name:" pattern
                            if let Some(name_pos) = search_text.rfind(arg_name) {
                                let abs_name_start = search_start + name_pos;
                                let abs_name_end = abs_name_start + arg_name.len();

                                // Check if cursor is on this argument name
                                if offset >= abs_name_start && offset <= abs_name_end {
                                    return Some((arg_name.to_string(), value_expr.ty));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

