//! Constructors for `sqlparser::ast::Expr` nodes used by RQ lowering.
//!
//! Mirrors DataFusion's `unparser` helpers: small, focused builders that keep
//! the lowering pass readable and free of `sqlparser::ast::*` boilerplate.
//! Reference: `datafusion/sql/src/unparser/expr.rs`.

use sqlparser::ast::{
    CastKind, DataType, Expr, Function, FunctionArg, FunctionArgExpr, FunctionArgumentList,
    FunctionArguments, Ident, ObjectName, ObjectNamePart, Value, ValueWithSpan,
};

/// Column reference: `col_name`.
pub fn col(name: impl Into<String>) -> Expr {
    Expr::Identifier(Ident::new(name))
}

/// Single-quoted string literal: `'value'`.
pub fn string_lit(s: impl Into<String>) -> Expr {
    Expr::Value(ValueWithSpan::from(Value::SingleQuotedString(s.into())))
}

/// Integer literal: `42`.
pub fn int_lit(i: i64) -> Expr {
    Expr::Value(ValueWithSpan::from(Value::Number(i.to_string(), false)))
}

/// Boolean literal: `TRUE` / `FALSE`.
pub fn bool_lit(b: bool) -> Expr {
    Expr::Value(ValueWithSpan::from(Value::Boolean(b)))
}

/// `NULL` literal.
pub fn null_lit() -> Expr {
    Expr::Value(ValueWithSpan::from(Value::Null))
}

/// `CAST(expr AS VARCHAR)`. Used for stringification before concatenation.
pub fn cast_varchar(expr: Expr) -> Expr {
    Expr::Cast {
        kind: CastKind::Cast,
        expr: Box::new(expr),
        data_type: DataType::Varchar(None),
        format: None,
    }
}

/// Generic function call: `name(arg1, arg2, ...)`.
pub fn func(name: &str, args: Vec<Expr>) -> Expr {
    let fn_args: Vec<FunctionArg> = args
        .into_iter()
        .map(|a| FunctionArg::Unnamed(FunctionArgExpr::Expr(a)))
        .collect();

    Expr::Function(Function {
        name: ObjectName(vec![ObjectNamePart::Identifier(Ident::new(name))]),
        uses_odbc_syntax: false,
        parameters: FunctionArguments::None,
        args: FunctionArguments::List(FunctionArgumentList {
            duplicate_treatment: None,
            args: fn_args,
            clauses: vec![],
        }),
        filter: None,
        null_treatment: None,
        over: None,
        within_group: vec![],
    })
}

/// `COALESCE(a, b)`.
pub fn coalesce(a: Expr, b: Expr) -> Expr {
    func("COALESCE", vec![a, b])
}

/// `CONCAT(parts...)`.
pub fn concat(parts: Vec<Expr>) -> Expr {
    func("CONCAT", parts)
}

/// `expr IS NULL` (negated=false) or `expr IS NOT NULL` (negated=true).
pub fn is_null(expr: Expr, negated: bool) -> Expr {
    if negated {
        Expr::IsNotNull(Box::new(expr))
    } else {
        Expr::IsNull(Box::new(expr))
    }
}

/// Extract a single-quoted string literal value from an `Expr`, if present.
/// Used when lowering needs to thread a literal back into `ScanSource.params`.
pub fn expr_string_lit(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Value(vws) => match &vws.value {
            Value::SingleQuotedString(s) => Some(s.as_str()),
            _ => None,
        },
        _ => None,
    }
}

/// Stringify an `Expr` literal value for use as a `ScanSource` parameter.
/// Returns `None` for non-literal expressions.
pub fn expr_to_param_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Value(vws) => match &vws.value {
            Value::SingleQuotedString(s) => Some(s.clone()),
            Value::Number(n, _) => Some(n.clone()),
            Value::Boolean(b) => Some(b.to_string()),
            Value::Null => Some("NULL".to_string()),
            _ => None,
        },
        _ => None,
    }
}
