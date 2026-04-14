//! SQL dialect abstraction — pluggable backend for SQL generation.
//!
//! Dialects return `sqlparser::ast::TableFactor` directly — no SQL strings.
//! The compiler builds an `ast::Query` and stringifies once at the boundary,
//! mirroring DataFusion's `Unparser` pattern.

use sqlparser::ast;

use crate::rq::ScanSource;

/// How the host should execute a source scan.
#[derive(Debug, Clone)]
pub enum ScanStrategy {
    /// Database reads the source directly via this `TableFactor` (e.g.
    /// `read_csv('path', delim=',')` as a table-valued function call).
    TableFactor(ast::TableFactor),
    /// Host must preprocess first (e.g. PDF extraction), creating a temp table.
    Preprocess {
        handler: String,
        output_table: String,
    },
}

/// Backend-specific source resolution. Implemented by the host.
pub trait SqlDialect {
    fn scan_strategy(&self, source: &ScanSource) -> ScanStrategy;
}

/// Convention-based dialect: `read_{format}('{path}', key=value, ...)` as
/// a table-valued function call. Works for most DuckDB formats out of the
/// box (parquet, json, csv, etc.).
pub struct DefaultDialect;

impl SqlDialect for DefaultDialect {
    fn scan_strategy(&self, source: &ScanSource) -> ScanStrategy {
        let fn_name = format!("read_{}", source.format);
        let mut args: Vec<ast::FunctionArg> = vec![ast::FunctionArg::Unnamed(
            ast::FunctionArgExpr::Expr(ast::Expr::Value(
                ast::Value::SingleQuotedString(source.path.clone()).into(),
            )),
        )];
        for (key, value) in &source.params {
            args.push(ast::FunctionArg::Named {
                name: ast::Ident::new(key.clone()),
                arg: ast::FunctionArgExpr::Expr(param_value_to_expr(value)),
                operator: ast::FunctionArgOperator::Equals,
            });
        }
        ScanStrategy::TableFactor(table_function(fn_name, args))
    }
}

/// Build a `TableFactor::Table` reference (e.g. `FROM cte_name` or
/// `FROM "preprocessed_table"`). Single-segment identifier; the dialect
/// chooses quoting via `ast::Ident::new`.
pub fn table_ref(name: impl Into<String>) -> ast::TableFactor {
    ast::TableFactor::Table {
        name: ast::ObjectName(vec![ast::ObjectNamePart::Identifier(ast::Ident::new(
            name.into(),
        ))]),
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

/// Build a `TableFactor::Table` whose underlying call is a table-valued
/// function (e.g. `read_csv('x.csv', delim=',')`). sqlparser 0.58 represents
/// TVFs as `Table { args: Some(...), .. }` rather than a separate variant.
pub fn table_function(name: String, args: Vec<ast::FunctionArg>) -> ast::TableFactor {
    ast::TableFactor::Table {
        name: ast::ObjectName(vec![ast::ObjectNamePart::Identifier(ast::Ident::new(name))]),
        alias: None,
        args: Some(ast::TableFunctionArgs {
            args,
            settings: None,
        }),
        with_hints: vec![],
        version: None,
        with_ordinality: false,
        partitions: vec![],
        json_path: None,
        sample: None,
        index_hints: vec![],
    }
}

/// Convert a parameter value string to its sqlparser expression. Matches the
/// previous string-based behavior (booleans / integers / quoted strings).
fn param_value_to_expr(value: &str) -> ast::Expr {
    match value {
        "true" => ast::Expr::Value(ast::Value::Boolean(true).into()),
        "false" => ast::Expr::Value(ast::Value::Boolean(false).into()),
        v if v.parse::<i64>().is_ok() => ast::Expr::Value(ast::Value::Number(v.into(), false).into()),
        v => ast::Expr::Value(ast::Value::SingleQuotedString(v.to_string()).into()),
    }
}
