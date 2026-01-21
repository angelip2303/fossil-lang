use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use fossil_lang::ast::thir::{TypeKind, RecordRow, TypedHir};
use fossil_lang::passes::GlobalContext;

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

    // Convert position to byte offset
    let offset = match doc.position_to_offset(position.line, position.character) {
        Some(o) => o,
        None => return Ok(None),
    };

    // Check what triggered the completion
    let trigger_char = if offset > 0 {
        doc.text.chars().nth(offset - 1)
    } else {
        None
    };

    tracing::debug!("Trigger char: {:?}, offset: {}", trigger_char, offset);

    // If we have a compiled program (or a previous valid one), provide smart completions
    if let Some(thir_program) = doc.thir_for_completion() {
        // Check if we're completing after '_.' (field selector)
        if trigger_char == Some('.') && offset >= 2 {
            let char_before_dot = doc.text.chars().nth(offset - 2);
            if char_before_dot == Some('_') {
                // Check if it's a standalone underscore (not part of an identifier like `foo_`)
                let char_before_underscore = if offset >= 3 {
                    doc.text.chars().nth(offset - 3)
                } else {
                    None
                };
                let is_standalone_underscore = char_before_underscore
                    .map(|c| !c.is_alphanumeric() && c != '_')
                    .unwrap_or(true);

                if is_standalone_underscore {
                    return Ok(complete_field_selector(
                        &thir_program.thir,
                        &thir_program.gcx,
                        &doc.text,
                        offset,
                    ));
                }
            }
        }

        // Check if we're completing after a '.' (field access)
        if trigger_char == Some('.') {
            return Ok(complete_field_access(
                &thir_program.thir,
                &thir_program.gcx,
                &doc.text,
                offset - 1, // Position before the '.'
            ));
        }

        // Check if we're completing after '::' (module access)
        if offset >= 2 && &doc.text[offset.saturating_sub(2)..offset] == "::" {
            return Ok(complete_module_access(
                &thir_program.thir,
                &thir_program.gcx,
                &doc.text,
                offset - 2, // Position before the '::'
            ));
        }

        // Otherwise, provide general completions (variables in scope, types, etc.)
        return Ok(complete_general(
            &thir_program.thir,
            &thir_program.gcx,
            offset,
        ));
    }

    // Fallback to basic completions if no compiled program
    Ok(Some(basic_completions()))
}

/// Complete field selector after '_.'
/// This tries to infer the expected record type from context and suggest fields
fn complete_field_selector(
    thir: &TypedHir,
    gcx: &GlobalContext,
    source: &str,
    offset: usize,
) -> Option<CompletionResponse> {
    eprintln!("[COMPLETION] complete_field_selector at offset {}", offset);

    // Try to find what type is expected at this position
    // This is complex because we need to analyze the surrounding context
    // For now, we'll look for common patterns like:
    // - Function arguments where a FieldSelector is expected
    // - Let bindings with type annotations

    let mut items = vec![];

    // First, try to find if we're inside a function call
    // and determine which argument position we're at
    if let Some(expected_type) = find_expected_type_at_offset(thir, gcx, source, offset) {
        eprintln!("[COMPLETION] Found expected type for field selector");
        let fields = collect_fields_from_type(thir, gcx, &expected_type);
        for (name, type_str) in fields {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!("_.{}: {}", name, type_str)),
                insert_text: Some(name),
                ..Default::default()
            });
        }
    }

    // If we couldn't determine the expected type, provide completions from
    // all record types defined in the program
    if items.is_empty() {
        eprintln!("[COMPLETION] No expected type found, collecting all record fields");
        let all_fields = collect_all_record_fields(thir, gcx);
        for (name, type_str, source_type) in all_fields {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!("{} (from {})", type_str, source_type)),
                insert_text: Some(name),
                ..Default::default()
            });
        }
    }

    if items.is_empty() {
        return None;
    }

    Some(CompletionResponse::Array(items))
}

/// Configuration for field selector argument relationships
/// Maps (function_name, selector_arg_index) -> source_list_arg_index
/// For example: ("join", 2) -> 0 means left_on (arg 2) gets fields from left list (arg 0)
fn get_field_selector_source_arg(func_name: &str, arg_index: usize, arg_name: Option<&str>) -> Option<usize> {
    match func_name {
        "join" => {
            // List::join(left, right, left_on, right_on)
            // left_on (arg 2 or named "left_on") -> left (arg 0)
            // right_on (arg 3 or named "right_on") -> right (arg 1)
            match arg_name {
                Some("left_on") => Some(0),
                Some("right_on") => Some(1),
                None if arg_index == 2 => Some(0),
                None if arg_index == 3 => Some(1),
                _ => None,
            }
        }
        "sort" => {
            // List::sort(data, column, descending)
            // column (arg 1) -> data (arg 0)
            if arg_index == 1 || arg_name == Some("column") {
                Some(0)
            } else {
                None
            }
        }
        "filter" => {
            // List::filter(data, predicate) - predicate might use field selectors
            // In the future, could support Expr<T, bool> where T comes from arg 0
            if arg_index == 1 {
                Some(0)
            } else {
                None
            }
        }
        "map" => {
            // List::map(data, fn) - fn might use field selectors referring to data's element type
            if arg_index == 1 {
                Some(0)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Try to find the expected type at a given offset
/// This analyzes the surrounding context to determine what record type is expected
fn find_expected_type_at_offset(
    thir: &TypedHir,
    gcx: &GlobalContext,
    source: &str,
    offset: usize,
) -> Option<TypeKind> {
    eprintln!("[COMPLETION] find_expected_type_at_offset at {}", offset);

    // Strategy 1: Parse source text to find function call context
    // This works even when the code doesn't compile
    if let Some(context) = parse_call_context(source, offset) {
        eprintln!(
            "[COMPLETION] Found call context: func={}, arg_idx={}, arg_name={:?}, earlier_args={:?}",
            context.function_name, context.arg_index, context.current_arg_name, context.earlier_args
        );

        // Check if this argument position expects a field selector from another argument
        if let Some(source_arg_idx) = get_field_selector_source_arg(
            &context.function_name,
            context.arg_index,
            context.current_arg_name.as_deref(),
        ) {
            eprintln!("[COMPLETION] Field selector should use fields from arg {}", source_arg_idx);

            // Get the variable name from that argument position
            if let Some(list_name) = context.earlier_args.get(source_arg_idx) {
                eprintln!("[COMPLETION] Source list variable: {}", list_name);
                if let Some(elem_type) = find_list_element_type(thir, gcx, list_name) {
                    eprintln!("[COMPLETION] Found element type for {}", list_name);
                    return Some(elem_type);
                }
            }
        }
    }

    // Strategy 2: Look for function applications in THIR where the offset is within an argument
    // This works when the code compiles successfully
    for (_expr_id, expr) in thir.exprs.iter() {
        if let fossil_lang::ast::thir::ExprKind::Application { callee, args } = &expr.kind {
            // Check if offset is within any of the arguments
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg_expr_id = match arg {
                    fossil_lang::ast::thir::Argument::Positional(e) => *e,
                    fossil_lang::ast::thir::Argument::Named { value, .. } => *value,
                };
                let arg_expr = thir.exprs.get(arg_expr_id);

                // Check if we're within this argument's span (with some tolerance for typing)
                if arg_expr.loc.span.start <= offset && offset <= arg_expr.loc.span.end + 5 {
                    // Get the callee's type to find expected argument types
                    let callee_expr = thir.exprs.get(*callee);
                    let callee_ty = thir.types.get(callee_expr.ty);

                    if let TypeKind::Function(param_types, _) = &callee_ty.kind
                        && arg_idx < param_types.len() {
                            let param_ty = thir.types.get(param_types[arg_idx]);
                            // If the parameter is a FieldSelector, return the record type
                            if let TypeKind::FieldSelector { record_ty, .. } = &param_ty.kind {
                                let record_type = thir.types.get(*record_ty);
                                eprintln!("[COMPLETION] Found FieldSelector param in THIR, returning record type");
                                return Some(record_type.kind.clone());
                            }
                        }
                }
            }
        }
    }

    // Strategy 3: Find all List<T> variables defined before this offset
    // and return the element type of the most recently defined one
    let mut best_match: Option<(usize, TypeKind)> = None;
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Let { value, .. } = &stmt.kind {
            let value_expr = thir.exprs.get(*value);
            let value_ty = thir.types.get(value_expr.ty);

            // If it's a List type, extract the element type
            if let TypeKind::App { ctor, args } = &value_ty.kind {
                let ctor_def = gcx.definitions.get(*ctor);
                let ctor_name = gcx.interner.resolve(ctor_def.name);
                if ctor_name == "List" && !args.is_empty() {
                    let elem_ty = thir.types.get(args[0]);
                    // Only consider let bindings defined before the offset
                    if stmt.loc.span.end < offset {
                        let distance = offset - stmt.loc.span.end;
                        if best_match.is_none() || distance < best_match.as_ref().unwrap().0 {
                            best_match = Some((distance, elem_ty.kind.clone()));
                        }
                    }
                }
            }
        }
    }

    if let Some((_, elem_type)) = best_match {
        eprintln!("[COMPLETION] Fallback: Found nearby List, using its element type");
        return Some(elem_type);
    }

    None
}

/// Context information about a function call parsed from source text
struct CallContext {
    /// The function name (simple, without module prefix)
    function_name: String,
    /// The full function path (e.g., "List::join")
    full_function_path: String,
    /// Zero-based index of the current argument
    arg_index: usize,
    /// Name of the current argument if it's a named argument (e.g., "left_on")
    current_arg_name: Option<String>,
    /// Values of earlier arguments (variable names or expressions)
    earlier_args: Vec<String>,
}

/// Parse source text to find information about the function call context
fn parse_call_context(source: &str, offset: usize) -> Option<CallContext> {
    let before = &source[..offset];

    // Look for the opening parenthesis of the function call
    let mut paren_depth = 0;
    let mut call_start = None;
    let mut arg_count = 0;
    let mut comma_positions = vec![];

    for (i, c) in before.char_indices().rev() {
        match c {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth == 0 {
                    call_start = Some(i);
                    break;
                }
                paren_depth -= 1;
            }
            ',' if paren_depth == 0 => {
                arg_count += 1;
                comma_positions.push(i);
            }
            _ => {}
        }
    }

    let call_start = call_start?;

    // Extract function name (look backwards from the opening paren)
    let before_paren = &source[..call_start];
    let func_name_start = before_paren
        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != ':')
        .map(|i| i + 1)
        .unwrap_or(0);
    let full_func_name = before_paren[func_name_start..].trim().to_string();

    // Extract the simple function name (after ::)
    let simple_name = full_func_name
        .rsplit("::")
        .next()
        .unwrap_or(&full_func_name)
        .to_string();

    // Parse all arguments
    let args_text = &source[call_start + 1..offset];
    let arg_parts: Vec<&str> = args_text.split(',').collect();

    // Extract earlier argument values
    let earlier_args: Vec<String> = arg_parts
        .iter()
        .take(arg_count)
        .filter_map(|arg| extract_arg_value(arg))
        .collect();

    // Check if the current argument (the one being typed) has a name
    let current_arg_name = if arg_parts.len() > arg_count {
        let current_arg_text = arg_parts.get(arg_count).unwrap_or(&"").trim();
        extract_arg_name(current_arg_text)
    } else {
        None
    };

    Some(CallContext {
        function_name: simple_name,
        full_function_path: full_func_name,
        arg_index: arg_count,
        current_arg_name,
        earlier_args,
    })
}

/// Extract the value from an argument string
/// Handles both positional ("customers") and named ("left_on: customers")
fn extract_arg_value(arg: &str) -> Option<String> {
    let arg = arg.trim();
    if arg.is_empty() {
        return None;
    }

    if let Some(colon_pos) = arg.find(':') {
        // Named argument: extract value after colon
        let value = arg[colon_pos + 1..].trim();
        // Get the first identifier-like token
        extract_identifier(value)
    } else {
        // Positional argument
        extract_identifier(arg)
    }
}

/// Extract the name from a named argument (e.g., "left_on:" -> "left_on")
fn extract_arg_name(arg: &str) -> Option<String> {
    let arg = arg.trim();
    if let Some(colon_pos) = arg.find(':') {
        let name = arg[..colon_pos].trim();
        if !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Some(name.to_string());
        }
    }
    None
}

/// Extract the first identifier from a string
fn extract_identifier(s: &str) -> Option<String> {
    let s = s.trim();
    let end = s
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(s.len());
    if end > 0 {
        Some(s[..end].to_string())
    } else {
        None
    }
}

/// Find the element type of a List variable by name
fn find_list_element_type(
    thir: &TypedHir,
    gcx: &GlobalContext,
    name: &str,
) -> Option<TypeKind> {
    let symbol = gcx.interner.lookup(name)?;

    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Let { name: binding_name, value } = &stmt.kind
            && *binding_name == symbol {
                let value_expr = thir.exprs.get(*value);
                let value_ty = thir.types.get(value_expr.ty);

                if let TypeKind::App { ctor, args } = &value_ty.kind {
                    let ctor_def = gcx.definitions.get(*ctor);
                    let ctor_name = gcx.interner.resolve(ctor_def.name);
                    if ctor_name == "List" && !args.is_empty() {
                        let elem_ty = thir.types.get(args[0]);
                        return Some(elem_ty.kind.clone());
                    }
                }
            }
    }

    None
}

/// Collect all record fields from all defined types in the program
fn collect_all_record_fields(
    thir: &TypedHir,
    gcx: &GlobalContext,
) -> Vec<(String, String, String)> {
    let mut all_fields = vec![];
    let mut seen_fields = std::collections::HashSet::new();

    // Collect from type definitions
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Type { name, ty } = &stmt.kind {
            let type_name = gcx.interner.resolve(*name).to_string();
            let ty_data = thir.types.get(*ty);

            if let TypeKind::Record(row) = &ty_data.kind {
                let fields = collect_record_fields(thir, gcx, row);
                for (field_name, field_type) in fields {
                    if seen_fields.insert(field_name.clone()) {
                        all_fields.push((field_name, field_type, type_name.clone()));
                    }
                }
            }
        }
    }

    // Also collect from let bindings with record types
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Let { name, value } = &stmt.kind {
            let binding_name = gcx.interner.resolve(*name).to_string();
            let value_expr = thir.exprs.get(*value);
            let value_ty = thir.types.get(value_expr.ty);

            // Check if it's a List<Record> type
            if let TypeKind::App { ctor, args } = &value_ty.kind {
                let ctor_def = gcx.definitions.get(*ctor);
                let ctor_name = gcx.interner.resolve(ctor_def.name);
                if ctor_name == "List" && !args.is_empty() {
                    let elem_ty = thir.types.get(args[0]);
                    let fields = collect_fields_from_type(thir, gcx, &elem_ty.kind);
                    for (field_name, field_type) in fields {
                        if seen_fields.insert(field_name.clone()) {
                            all_fields.push((field_name, field_type, binding_name.clone()));
                        }
                    }
                }
            }
        }
    }

    all_fields
}

/// Complete field access after '.'
fn complete_field_access(
    thir: &TypedHir,
    gcx: &GlobalContext,
    source: &str,
    dot_offset: usize,
) -> Option<CompletionResponse> {
    eprintln!("[COMPLETION] complete_field_access at offset {}", dot_offset);

    // First, try to find the identifier name before the dot from source text
    let identifier_name = find_identifier_before(source, dot_offset)?;
    eprintln!("[COMPLETION] Looking for identifier: {}", identifier_name);

    // Look up the identifier's type in THIR (passing offset for scope checking)
    let fields = if let Some(ty_kind) = find_identifier_type(thir, gcx, &identifier_name, dot_offset) {
        eprintln!("[COMPLETION] Found type for '{}': {:?}", identifier_name, ty_kind);
        collect_fields_from_type(thir, gcx, &ty_kind)
    } else {
        // Fallback: try to find an expression at the offset (for chained access like a.b.c)
        if let Some(expr_id) = find_expr_at_offset(thir, dot_offset) {
            let expr = thir.exprs.get(expr_id);
            let ty = thir.types.get(expr.ty);
            collect_fields_from_type(thir, gcx, &ty.kind)
        } else {
            tracing::debug!("Could not find type for {}", identifier_name);
            vec![]
        }
    };

    if fields.is_empty() {
        tracing::debug!("No fields found for type");
        return None;
    }

    let items: Vec<CompletionItem> = fields
        .into_iter()
        .map(|(name, type_str)| CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(type_str),
            insert_text: Some(name),
            ..Default::default()
        })
        .collect();

    Some(CompletionResponse::Array(items))
}

/// Find the type of an identifier by searching let bindings and function parameters
/// The offset parameter is used to check scope - only return types for identifiers
/// that are in scope at that position.
fn find_identifier_type(
    thir: &TypedHir,
    gcx: &GlobalContext,
    name: &str,
    offset: usize,
) -> Option<TypeKind> {
    // First, look for the symbol in the interner
    let symbol = gcx.interner.lookup(name)?;

    // Search through all let bindings in THIR (check scope)
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Let { name: binding_name, value } = &stmt.kind {
            // Only consider let bindings that are defined before or contain the offset
            if *binding_name == symbol && stmt.loc.span.start <= offset {
                let value_expr = thir.exprs.get(*value);
                let ty = thir.types.get(value_expr.ty);
                return Some(ty.kind.clone());
            }
        }
    }

    // Search through all expressions for function parameters
    // Only consider parameters if the offset is within the function's body
    for (_expr_id, expr) in thir.exprs.iter() {
        if let fossil_lang::ast::thir::ExprKind::Function { params, body: _ } = &expr.kind {
            // Check if the offset is within this function's span
            // This ensures we only find params that are in scope at the cursor position
            let in_function_scope = expr.loc.span.start <= offset && offset <= expr.loc.span.end;

            if in_function_scope {
                for param in params {
                    if param.name == symbol {
                        // Find the function's type to get parameter types
                        let func_ty = thir.types.get(expr.ty);
                        if let TypeKind::Function(param_types, _) = &func_ty.kind {
                            // Find which parameter index this is
                            for (i, p) in params.iter().enumerate() {
                                if p.name == symbol && i < param_types.len() {
                                    let param_ty = thir.types.get(param_types[i]);
                                    return Some(param_ty.kind.clone());
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

/// Collect fields from a type, handling Named types and Records
fn collect_fields_from_type(
    thir: &TypedHir,
    gcx: &GlobalContext,
    type_kind: &TypeKind,
) -> Vec<(String, String)> {
    eprintln!("[COMPLETION] collect_fields_from_type: {:?}", type_kind);
    match type_kind {
        TypeKind::Record(row) => {
            eprintln!("[COMPLETION] Type is Record, collecting fields from row");
            collect_record_fields(thir, gcx, row)
        }
        TypeKind::Named(def_id) => {
            // Look up the type definition to get its fields
            let def = gcx.definitions.get(*def_id);
            let type_name = def.name;
            let type_name_str = gcx.interner.resolve(type_name);
            eprintln!("[COMPLETION] Type is Named '{}' (def_id {:?}), looking up definition", type_name_str, def_id);

            // Search for the type definition in THIR statements
            for stmt_id in &thir.root {
                let stmt = thir.stmts.get(*stmt_id);
                if let fossil_lang::ast::thir::StmtKind::Type { name, ty } = &stmt.kind
                    && *name == type_name {
                        let ty_data = thir.types.get(*ty);
                        eprintln!("[COMPLETION] Found type definition, underlying type: {:?}", ty_data.kind);
                        return collect_fields_from_type(thir, gcx, &ty_data.kind);
                    }
            }
            eprintln!("[COMPLETION] Could not find type definition for '{}'", type_name_str);
            vec![]
        }
        TypeKind::App { ctor, args } => {
            // For type applications like List<T>, Entity<T>, etc.
            // Check if it's Entity - if so, get fields from the inner type
            let ctor_def = gcx.definitions.get(*ctor);
            let ctor_name = gcx.interner.resolve(ctor_def.name);

            if ctor_name == "Entity" && !args.is_empty() {
                let inner_ty = thir.types.get(args[0]);
                return collect_fields_from_type(thir, gcx, &inner_ty.kind);
            }

            vec![]
        }
        TypeKind::Var(_) => {
            // Type variable - can't determine fields
            tracing::debug!("Type is a variable, cannot determine fields");
            vec![]
        }
        _ => vec![],
    }
}

/// Collect fields from a record row
fn collect_record_fields(
    thir: &TypedHir,
    gcx: &GlobalContext,
    row: &RecordRow,
) -> Vec<(String, String)> {
    let mut fields = vec![];
    let mut current = row;

    loop {
        match current {
            RecordRow::Empty => break,
            RecordRow::Var(_) => break,
            RecordRow::Extend { field, ty, rest } => {
                let field_name = gcx.interner.resolve(*field).to_string();
                let type_str = format_type_simple(thir, gcx, *ty);
                fields.push((field_name, type_str));
                current = rest;
            }
        }
    }

    fields
}

/// Format a type simply for display
fn format_type_simple(thir: &TypedHir, gcx: &GlobalContext, ty_id: fossil_lang::ast::thir::TypeId) -> String {
    let ty = thir.types.get(ty_id);
    match &ty.kind {
        TypeKind::Primitive(p) => match p {
            fossil_lang::ast::ast::PrimitiveType::Int => "int".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Float => "float".to_string(),
            fossil_lang::ast::ast::PrimitiveType::String => "string".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Bool => "bool".to_string(),
            fossil_lang::ast::ast::PrimitiveType::Unit => "unit".to_string(),
        },
        TypeKind::Var(v) => format!("'{}", v.0),
        TypeKind::Named(def_id) => {
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
                    .map(|arg| format_type_simple(thir, gcx, *arg))
                    .collect();
                format!("{}<{}>", ctor_name, arg_strs.join(", "))
            }
        }
        TypeKind::Function(params, ret) => {
            let param_strs: Vec<_> = params
                .iter()
                .map(|p| format_type_simple(thir, gcx, *p))
                .collect();
            format!("({}) -> {}", param_strs.join(", "), format_type_simple(thir, gcx, *ret))
        }
        TypeKind::Record(_) => "{...}".to_string(),
        TypeKind::FieldSelector { field, .. } => {
            format!("_.{}", gcx.interner.resolve(*field))
        }
    }
}

/// Complete module access after '::'
fn complete_module_access(
    _thir: &TypedHir,
    gcx: &GlobalContext,
    source: &str,
    colons_offset: usize,
) -> Option<CompletionResponse> {
    eprintln!("[COMPLETION] complete_module_access at offset {}", colons_offset);

    // Find the module/type name before '::'
    let module_name = find_identifier_before(source, colons_offset)?;
    eprintln!("[COMPLETION] Module name: {}", module_name);

    let mut items = vec![];

    // Look for the module in definitions and get its child functions
    // We need to find a Mod definition specifically, not just any definition with that name
    // (providers create both a Type and a Mod with the same name)
    if let Some(module_sym) = gcx.interner.lookup(&module_name) {
        // Find all definitions with this symbol and look for modules
        for def in gcx.definitions.iter() {
            if def.name == module_sym {
                let is_module = matches!(def.kind, fossil_lang::context::DefKind::Mod { .. });
                let module_id = def.id();
                eprintln!("[COMPLETION] Found def '{}' with def_id {:?}, is_module: {}", module_name, module_id, is_module);

                // Get all children of this definition (functions, etc.)
                let children = gcx.definitions.get_children(module_id);
                eprintln!("[COMPLETION] Def has {} children", children.len());

                for child in children {
                    let child_name = gcx.interner.resolve(child.name).to_string();
                    eprintln!("[COMPLETION] Child: {}", child_name);

                    // Check if it's a function
                    if let fossil_lang::context::DefKind::Func(_) = &child.kind {
                        items.push(CompletionItem {
                            label: child_name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some("function".to_string()),
                            insert_text: Some(format!("{}()", child_name)),
                            ..Default::default()
                        });
                    }
                }
            }
        }
    }

    // Also check for stdlib modules (List, String, etc.)
    let stdlib_funcs = get_stdlib_functions(&module_name);
    for (name, signature) in stdlib_funcs {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(signature.to_string()),
            insert_text: Some(format!("{}(", name)),
            ..Default::default()
        });
    }

    if items.is_empty() {
        eprintln!("[COMPLETION] No completions found for module '{}'", module_name);
        return None;
    }

    eprintln!("[COMPLETION] Returning {} completions for module", items.len());
    Some(CompletionResponse::Array(items))
}

/// Get stdlib functions for common modules
fn get_stdlib_functions(module_name: &str) -> Vec<(&'static str, &'static str)> {
    match module_name {
        "List" => vec![
            ("map", "(fn) -> List<B>"),
            ("filter", "(fn) -> List<A>"),
            ("head", "() -> A"),
            ("tail", "() -> List<A>"),
            ("length", "() -> int"),
            ("join", "(List<B>, string, string) -> List<{...}>"),
            ("concat", "(List<A>) -> List<A>"),
        ],
        "String" => vec![
            ("concat", "(string) -> string"),
            ("length", "() -> int"),
            ("to_string", "(A) -> string"),
        ],
        "Entity" => vec![
            ("new", "(A) -> Entity<A>"),
            ("with_id", "(A, string) -> Entity<A>"),
        ],
        "Rdf" => vec![
            ("serialize", "(List<Entity<A>>, string) -> unit"),
        ],
        _ => vec![],
    }
}

/// Find identifier before a given offset
fn find_identifier_before(source: &str, offset: usize) -> Option<String> {
    let before = &source[..offset];
    let mut end = before.len();

    // Skip whitespace
    while end > 0 && before.chars().nth(end - 1).is_some_and(|c| c.is_whitespace()) {
        end -= 1;
    }

    // Find start of identifier
    let mut start = end;
    while start > 0 {
        let c = before.chars().nth(start - 1)?;
        if c.is_alphanumeric() || c == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    Some(before[start..end].to_string())
}

/// General completions (variables, types, etc.)
fn complete_general(
    thir: &TypedHir,
    gcx: &GlobalContext,
    offset: usize,
) -> Option<CompletionResponse> {
    let mut items = vec![];

    // Add keywords
    for keyword in &["let", "type", "open", "as", "fn", "true", "false"] {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        });
    }

    // Add field selector placeholder
    items.push(CompletionItem {
        label: "_".to_string(),
        kind: Some(CompletionItemKind::SNIPPET),
        detail: Some("Field selector placeholder".to_string()),
        documentation: Some(Documentation::String(
            "Use _.field for type-safe column references in functions like List::join".to_string()
        )),
        insert_text: Some("_.".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });

    // Add built-in types
    for ty in &["int", "bool", "string"] {
        items.push(CompletionItem {
            label: ty.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("built-in type".to_string()),
            ..Default::default()
        });
    }

    // Add defined types from the program
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Type { name, .. } = &stmt.kind {
            let type_name = gcx.interner.resolve(*name).to_string();
            items.push(CompletionItem {
                label: type_name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("type".to_string()),
                ..Default::default()
            });
        }
    }

    // Add let bindings
    for stmt_id in &thir.root {
        let stmt = thir.stmts.get(*stmt_id);
        if let fossil_lang::ast::thir::StmtKind::Let { name, value } = &stmt.kind {
            // Only suggest variables defined before the cursor
            if stmt.loc.span.start < offset {
                let var_name = gcx.interner.resolve(*name).to_string();
                let value_expr = thir.exprs.get(*value);
                let type_str = format_type_simple(thir, gcx, value_expr.ty);
                items.push(CompletionItem {
                    label: var_name,
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(type_str),
                    ..Default::default()
                });
            }
        }
    }

    // Add common modules
    for module in &["List", "String", "Entity", "Rdf", "Int"] {
        items.push(CompletionItem {
            label: module.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some("module".to_string()),
            ..Default::default()
        });
    }

    Some(CompletionResponse::Array(items))
}

/// Find the expression at a given offset
fn find_expr_at_offset(thir: &TypedHir, offset: usize) -> Option<fossil_lang::ast::thir::ExprId> {
    let mut best_match: Option<(fossil_lang::ast::thir::ExprId, usize)> = None;

    for (id, expr) in thir.exprs.iter() {
        let loc = &expr.loc;
        // Skip generated locations
        if loc.span.start == 0 && loc.span.end == 0 {
            continue;
        }

        // Check if offset is within or just before this expression's span
        // For completion after '.', we want the expression that ends at or just before the dot
        if loc.span.start <= offset && offset <= loc.span.end + 1 {
            let span_size = loc.span.end - loc.span.start;

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

    best_match.map(|(id, _)| id)
}

/// Basic completions when no compiled program is available
fn basic_completions() -> CompletionResponse {
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
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("built-in type".to_string()),
            ..Default::default()
        });
    }

    CompletionResponse::Array(items)
}
