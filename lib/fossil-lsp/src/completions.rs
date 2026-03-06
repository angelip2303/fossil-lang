use fossil_lang::common::PrimitiveType;
use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::context::DefKind;
use fossil_lang::ir::{RecordFields, TypeKind};
use fossil_lang::passes::IrProgram;
use serde::Serialize;

use crate::analysis::AnalysisSnapshot;
use crate::cursor::CursorContext;

#[derive(Debug, Serialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct CompletionItem {
    pub label: String,
    pub kind: &'static str,
    pub detail: String,
}

/// Resolve completions from cursor context + optional snapshot.
pub fn resolve_completions(
    ctx: &CursorContext,
    snapshot: Option<&AnalysisSnapshot>,
) -> Vec<CompletionItem> {
    let Some(snapshot) = snapshot else {
        return keyword_completions();
    };
    let program = &snapshot.program;

    match ctx {
        CursorContext::DotAccess { receiver, .. } => {
            resolve_dot_access(program, receiver)
        }
        CursorContext::ConnectionRef { .. } => {
            // Connection names aren't in the snapshot — the server fills these in.
            vec![]
        }
        CursorContext::TopLevel { .. } => top_level_completions(program),
    }
}

fn resolve_dot_access(program: &IrProgram, receiver: &str) -> Vec<CompletionItem> {
    let Some(sym) = program.gcx.interner.lookup(receiver) else {
        return vec![];
    };

    // Try as module name first (e.g. "Rdf", "String", "Report")
    if let Some(def) = program.gcx.definitions.find_by_symbol(sym, |k| matches!(k, DefKind::Mod)) {
        return completions_for_module(program, def.id());
    }

    // Try to find a binding with this name and resolve its type
    for (expr_id, expr) in program.ir.exprs.iter() {
        if let fossil_lang::ir::ExprKind::Identifier(path) = &expr.kind {
            if path_matches_symbol(path, sym) {
                if let Some(type_id) = program.typeck_results.expr_types.get(&expr_id) {
                    let completions = completions_for_type(program, *type_id);
                    if !completions.is_empty() {
                        return completions;
                    }
                }
            }
        }
    }

    vec![]
}

fn path_matches_symbol(path: &fossil_lang::common::Path, sym: fossil_lang::context::Symbol) -> bool {
    match path {
        fossil_lang::common::Path::Simple(s) => *s == sym,
        fossil_lang::common::Path::Qualified(parts) => parts.last() == Some(&sym),
    }
}

fn completions_for_type(
    program: &IrProgram,
    type_id: fossil_lang::ir::TypeId,
) -> Vec<CompletionItem> {
    let ty = program.ir.types.get(type_id);

    match &ty.kind {
        TypeKind::Record(fields) => record_field_completions(program, fields),
        TypeKind::Named(def_id) => {
            // Check type_index first (user-defined types)
            if let Some(info) = program.type_index.get(*def_id) {
                let inner_ty = program.ir.types.get(info.ty);
                if let TypeKind::Record(fields) = &inner_ty.kind {
                    return record_field_completions(program, fields);
                }
                let interner = &program.gcx.interner;
                return info
                    .field_names
                    .iter()
                    .map(|sym| CompletionItem {
                        label: interner.resolve(*sym).to_string(),
                        kind: "field",
                        detail: String::new(),
                    })
                    .collect();
            }
            // Check registered_types (built-in types like from providers)
            if let Some(fields) = program.gcx.registered_types.get(def_id) {
                let interner = &program.gcx.interner;
                return fields
                    .iter()
                    .map(|(sym, field_type)| {
                        let type_str = format_builtin_field_type(field_type);
                        CompletionItem {
                            label: interner.resolve(*sym).to_string(),
                            kind: "field",
                            detail: type_str,
                        }
                    })
                    .collect();
            }
            vec![]
        }
        TypeKind::Optional(inner) => completions_for_type(program, *inner),
        _ => vec![],
    }
}

fn record_field_completions(
    program: &IrProgram,
    fields: &RecordFields,
) -> Vec<CompletionItem> {
    let interner = &program.gcx.interner;
    fields
        .fields
        .iter()
        .map(|(sym, type_id)| {
            let type_str = format_type(program, *type_id);
            CompletionItem {
                label: interner.resolve(*sym).to_string(),
                kind: "field",
                detail: type_str,
            }
        })
        .collect()
}

fn completions_for_module(
    program: &IrProgram,
    module_def_id: fossil_lang::context::DefId,
) -> Vec<CompletionItem> {
    let interner = &program.gcx.interner;
    program
        .gcx
        .definitions
        .iter()
        .filter(|def| def.parent() == Some(module_def_id))
        .map(|def| {
            let kind = match &def.kind {
                DefKind::Func(_) => "function",
                DefKind::Type => "type",
                _ => "property",
            };
            CompletionItem {
                label: interner.resolve(def.name).to_string(),
                kind,
                detail: String::new(),
            }
        })
        .collect()
}

fn top_level_completions(program: &IrProgram) -> Vec<CompletionItem> {
    let interner = &program.gcx.interner;
    let mut items = Vec::new();

    for def in program.gcx.definitions.iter() {
        if def.parent().is_some() {
            continue;
        }
        let (kind, detail) = match &def.kind {
            DefKind::Mod => ("module", "module"),
            DefKind::Let => ("variable", "let binding"),
            DefKind::Type => ("type", "type"),
            DefKind::Func(_) => ("function", "function"),
            DefKind::RecordConstructor => ("constructor", "constructor"),
            DefKind::Provider(_) => ("function", "provider"),
        };
        items.push(CompletionItem {
            label: interner.resolve(def.name).to_string(),
            kind,
            detail: detail.to_string(),
        });
    }

    items
}

fn keyword_completions() -> Vec<CompletionItem> {
    static KEYWORDS: &[&str] = &["let", "type", "each", "join"];
    KEYWORDS
        .iter()
        .map(|kw| CompletionItem {
            label: (*kw).to_string(),
            kind: "keyword",
            detail: "keyword".to_string(),
        })
        .collect()
}

fn format_type(program: &IrProgram, type_id: fossil_lang::ir::TypeId) -> String {
    let ty = program.ir.types.get(type_id);
    match &ty.kind {
        TypeKind::Primitive(p) => format_primitive(p).to_string(),
        TypeKind::Optional(inner) => format!("{}?", format_type(program, *inner)),
        TypeKind::Unit => "()".to_string(),
        TypeKind::Named(def_id) => {
            let def = program.gcx.definitions.get(*def_id);
            program.gcx.interner.resolve(def.name).to_string()
        }
        TypeKind::Record(_) => "record".to_string(),
        TypeKind::Function(_, _) => "fn".to_string(),
        TypeKind::Var(v) => v.to_string(),
    }
}

fn format_primitive(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::Int => "int",
        PrimitiveType::Float => "float",
        PrimitiveType::String => "string",
        PrimitiveType::Bool => "bool",
    }
}

fn format_builtin_field_type(ft: &BuiltInFieldType) -> String {
    match ft {
        BuiltInFieldType::Required(p) => format_primitive(p).to_string(),
        BuiltInFieldType::Optional(p) => format!("{}?", format_primitive(p)),
    }
}
