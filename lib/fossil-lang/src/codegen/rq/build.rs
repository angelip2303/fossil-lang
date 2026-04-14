//! Constructors for `sqlparser::ast` nodes used by RQ lowering and SQL
//! assembly. Every field required by sqlparser 0.58 is populated explicitly
//! in one place so downstream passes read like relational algebra instead
//! of struct literals.
//!
//! Mirrors DataFusion's `unparser` helpers: small, focused builders.
//! Reference: `datafusion/sql/src/unparser/expr.rs`,
//! `datafusion/sql/src/unparser/plan.rs`.

use sqlparser::ast::helpers::attached_token::AttachedToken;
use sqlparser::ast::{
    self, CastKind, Cte, DataType, Expr, Function, FunctionArg, FunctionArgExpr,
    FunctionArgumentList, FunctionArguments, Ident, ObjectName, ObjectNamePart, Query, Select,
    SelectItem, SetExpr, TableAlias, TableFactor, TableWithJoins, Value, ValueWithSpan,
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
/// Used when lowering needs to thread a literal back into `SourceRef.path`.
pub fn expr_string_lit(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Value(vws) => match &vws.value {
            Value::SingleQuotedString(s) => Some(s.as_str()),
            _ => None,
        },
        _ => None,
    }
}

/// Stringify an `Expr` literal value for use as a `SourceRef` parameter.
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

// ── statement-level AST builders ──────────────────────────────────────

pub fn cte(alias: Ident, query: Query) -> Cte {
    Cte {
        alias: TableAlias {
            name: alias,
            columns: vec![],
        },
        query: Box::new(query),
        from: None,
        materialized: None,
        closing_paren_token: AttachedToken::empty(),
    }
}

pub fn select_query(
    projection: Vec<SelectItem>,
    from: Vec<TableWithJoins>,
    selection: Option<Expr>,
) -> Query {
    query_from_body(SetExpr::Select(Box::new(select_node(
        projection, from, selection,
    ))))
}

pub fn query_from_body(body: SetExpr) -> Query {
    Query {
        with: None,
        body: Box::new(body),
        order_by: None,
        limit_clause: None,
        fetch: None,
        locks: vec![],
        for_clause: None,
        settings: None,
        format_clause: None,
        pipe_operators: vec![],
    }
}

pub fn select_node(
    projection: Vec<SelectItem>,
    from: Vec<TableWithJoins>,
    selection: Option<Expr>,
) -> Select {
    Select {
        select_token: AttachedToken::empty(),
        distinct: None,
        top: None,
        top_before_distinct: false,
        projection,
        exclude: None,
        into: None,
        from,
        lateral_views: vec![],
        prewhere: None,
        selection,
        group_by: ast::GroupByExpr::Expressions(vec![], vec![]),
        cluster_by: vec![],
        distribute_by: vec![],
        sort_by: vec![],
        having: None,
        named_window: vec![],
        qualify: None,
        window_before_qualify: false,
        value_table_mode: None,
        connect_by: None,
        flavor: ast::SelectFlavor::Standard,
    }
}

pub fn twj(relation: TableFactor) -> TableWithJoins {
    TableWithJoins {
        relation,
        joins: vec![],
    }
}

pub fn wildcard_default() -> ast::WildcardAdditionalOptions {
    ast::WildcardAdditionalOptions::default()
}

pub fn wildcard_item() -> SelectItem {
    SelectItem::Wildcard(wildcard_default())
}

/// `FROM <name>` — a plain catalog-resolved table name. Whether `name`
/// points at a host-registered view, a preprocessed temp table, or an
/// upstream CTE is none of fossil-lang's concern.
pub fn table_ref(name: &Ident) -> TableFactor {
    TableFactor::Table {
        name: ObjectName(vec![ObjectNamePart::Identifier(name.clone())]),
        alias: None,
        args: None,
        with_hints: vec![],
        version: None,
        with_ordinality: false,
        partitions: vec![],
        json_path: None,
        sample: None,
        index_hints: vec![],
    }
}
