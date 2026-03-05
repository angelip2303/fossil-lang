use fossil_lang::context::{Interner, Symbol};
use polars::prelude::*;

use super::{slug, lower, upper, trim};

/// Apply a named filter to a Polars expression, reusing the same
/// transforms exposed by `String.slug`, `String.lower`, etc.
pub fn apply_filter(expr: Expr, filter: &str) -> Expr {
    match filter {
        "slug" => slug(expr),
        "lower" => lower(expr),
        "upper" => upper(expr),
        "trim" => trim(expr),
        _ => expr,
    }
}

/// Parse a template string, expanding `{param}` or `{param|filter}`
/// placeholders into Polars expressions from the positionally-matched param_exprs.
pub fn parse_template(
    template: &str,
    param_names: &[Symbol],
    param_exprs: &[Expr],
    interner: &Interner,
) -> Vec<Expr> {
    let mut parts = Vec::new();
    let mut literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if !literal.is_empty() {
                parts.push(lit(std::mem::take(&mut literal)));
            }
            let mut placeholder = String::new();
            for c in chars.by_ref() {
                if c == '}' { break; }
                placeholder.push(c);
            }
            let (name, filter) = match placeholder.split_once('|') {
                Some((n, f)) => (n, Some(f)),
                None => (placeholder.as_str(), None),
            };
            if let Some(idx) = param_names.iter().position(|sym| interner.resolve(*sym) == name) {
                if let Some(arg) = param_exprs.get(idx) {
                    let mut expr = arg.clone().cast(DataType::String);
                    if let Some(f) = filter {
                        expr = apply_filter(expr, f);
                    }
                    parts.push(expr);
                }
            }
        } else {
            literal.push(c);
        }
    }

    if !literal.is_empty() {
        parts.push(lit(literal));
    }

    parts
}

/// Extract the filter name from the first `{param|filter}` placeholder in a template.
///
/// Returns `None` if there is no placeholder or the first placeholder has no filter.
pub fn extract_first_filter(template: &str) -> Option<&str> {
    let start = template.find('{')? + 1;
    let end = template[start..].find('}')? + start;
    let placeholder = &template[start..end];
    let (_, filter) = placeholder.split_once('|')?;
    Some(filter)
}
