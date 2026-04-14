//! Relational Query (RQ) — the compiled-SQL output of fossil-lang.
//!
//! ## Design: catalog-based source resolution
//!
//! The compiled SQL references tables **by name only**. It never inlines
//! format-specific reads like `read_csv('path', delim=',')`. Every source
//! used by the query is recorded in `sources` as a [`SourceRef`] — the
//! source manifest. The host is responsible for resolving each manifest
//! entry against its catalog (DuckDB views, preprocessed temp tables,
//! etc.) before executing the emitted SQL.
//!
//! This mirrors the reference designs:
//! - **DataFusion**: `LogicalPlan::TableScan` resolves against a
//!   `SchemaProvider`; the unparser emits `FROM table_name` only.
//! - **PRQL**: `TableRef` → compiled SQL uses the alias; the context
//!   resolves it at bind time.
//! - **Ibis**: `ops.UnboundTable` → backend resolves at execution.
//!
//! As a consequence, fossil-lang has **no dialect abstraction at all**.
//! It doesn't know whether the host is DuckDB, Postgres, or Polars. It
//! doesn't know which formats need preprocessing. Those decisions live
//! entirely in the host's catalog-resolution step, where they belong.
//!
//! ## Side-tables
//!
//! `emissions` and `outputs` describe how lowered CTEs feed the
//! host-side materialization plan (which CTE produces which RDF entity,
//! where to write the graph). These have no sqlparser equivalent.

pub mod build;
pub mod lower;

use std::collections::HashMap;

use sqlparser::ast::helpers::attached_token::AttachedToken;
use sqlparser::ast::{Cte, Expr, Ident, Query, SelectItem, SetExpr, With};

use build::{query_from_body, select_node, table_ref, twj, wildcard_item};

/// A complete relational query produced by lowering the Fossil IR.
///
/// **Not serializable.** Holds `sqlparser::ast::Cte` which embeds `Expr`
/// etc. — sqlparser does not enable `serde` by default. Hosts that need
/// to serialize compiled output should use `FossilPlan`, which embeds
/// only the emitted SQL string.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RelationalQuery {
    /// Source manifest: every named source this query reads from. Host
    /// resolves each entry against its catalog before executing the SQL.
    pub sources: Vec<SourceRef>,
    /// Ordered pipeline of CTEs. Later CTEs can reference earlier ones
    /// (and source aliases) by name.
    pub ctes: Vec<Cte>,
    /// Which CTE outputs map to RDF entity types.
    pub emissions: Vec<EmissionDecl>,
    /// Output materialization instructions.
    pub outputs: Vec<OutputDecl>,
}

/// A single named source referenced by the compiled SQL.
///
/// The compiled SQL contains `FROM <alias>` references; the host must
/// register each alias in its catalog (e.g. `CREATE OR REPLACE VIEW
/// <alias> AS SELECT * FROM read_csv(...)`) before executing the query.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceRef {
    pub alias: Ident,
    pub format: String,
    pub path: String,
    pub params: HashMap<String, String>,
}

/// Maps a CTE output to an RDF entity type.
#[derive(Debug, Clone, PartialEq)]
pub struct EmissionDecl {
    pub table: Ident,
    pub type_name: String,
    pub subject_template: Expr,
    pub fields: Vec<(String, Ident)>,
    pub identity_columns: Vec<Ident>,
}

/// Output materialization instruction.
#[derive(Debug, Clone, PartialEq)]
pub struct OutputDecl {
    pub emissions: Vec<usize>,
    pub format: String,
    pub path: String,
    pub params: HashMap<String, String>,
}

impl RelationalQuery {
    pub fn new() -> Self {
        Self::default()
    }

    /// Assemble the final `sqlparser::ast::Query`. Emits
    /// `WITH <ctes> SELECT * FROM <last_cte>` when the RQ has any CTE,
    /// or `SELECT 1` as a non-zero placeholder when it is empty.
    /// Callers obtain the SQL string via `.to_string()`.
    pub fn to_query(&self) -> Query {
        let Some(last) = self.ctes.last() else {
            return placeholder_select_one();
        };
        let outer = select_node(
            vec![wildcard_item()],
            vec![twj(table_ref(&last.alias.name))],
            None,
        );
        let mut q = query_from_body(SetExpr::Select(Box::new(outer)));
        q.with = Some(With {
            with_token: AttachedToken::empty(),
            recursive: false,
            cte_tables: self.ctes.clone(),
        });
        q
    }
}

/// Back-compat free function: forwards to [`RelationalQuery::to_query`].
pub fn rq_to_query(rq: &RelationalQuery) -> Query {
    rq.to_query()
}

pub fn validate_duckdb_sql(sql: &str) -> Result<Vec<sqlparser::ast::Statement>, String> {
    use sqlparser::{dialect::DuckDbDialect, parser::Parser};
    Parser::parse_sql(&DuckDbDialect {}, sql).map_err(|e| e.to_string())
}

pub fn expr_to_sql(expr: &Expr) -> String {
    expr.to_string()
}

fn placeholder_select_one() -> Query {
    query_from_body(SetExpr::Select(Box::new(select_node(
        vec![SelectItem::UnnamedExpr(Expr::Value(
            sqlparser::ast::Value::Number("1".into(), false).into(),
        ))],
        vec![],
        None,
    ))))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rq::build;

    #[test]
    fn empty_rq_is_placeholder() {
        let rq = RelationalQuery::new();
        let sql = rq.to_query().to_string();
        validate_duckdb_sql(&sql).expect("placeholder SQL must round-trip");
    }

    #[test]
    fn sql_references_sources_by_name_only() {
        // Simulate a lowering that registered a source and emitted one CTE
        // whose body references it by alias.
        let mut rq = RelationalQuery::new();
        rq.sources.push(SourceRef {
            alias: Ident::new("src_csv_1"),
            format: "csv".into(),
            path: "data.csv".into(),
            params: Default::default(),
        });
        let body = select_node(
            vec![wildcard_item()],
            vec![twj(table_ref(&Ident::new("src_csv_1")))],
            None,
        );
        let cte_query = query_from_body(SetExpr::Select(Box::new(body)));
        rq.ctes.push(build::cte(Ident::new("persons_1"), cte_query));
        let sql = rq.to_query().to_string();
        validate_duckdb_sql(&sql).expect("emitted SQL must round-trip");
        assert!(sql.contains("src_csv_1"));
        assert!(sql.contains("persons_1"));
        // No `read_csv`: source resolution is host-side.
        assert!(!sql.contains("read_csv"));
    }

    #[test]
    fn expr_helpers_emit_expected_sql() {
        assert_eq!(
            expr_to_sql(&build::coalesce(
                build::col("x"),
                build::string_lit("default"),
            )),
            "COALESCE(x, 'default')",
        );
        assert_eq!(
            expr_to_sql(&build::concat(vec![
                build::string_lit("http://example.org/"),
                build::cast_varchar(build::col("id")),
            ])),
            "CONCAT('http://example.org/', CAST(id AS VARCHAR))",
        );
        assert_eq!(
            expr_to_sql(&build::func(
                "SHA256",
                vec![build::cast_varchar(build::col("email"))],
            )),
            "SHA256(CAST(email AS VARCHAR))",
        );
        assert_eq!(expr_to_sql(&build::null_lit()), "NULL");
        assert_eq!(
            expr_to_sql(&build::is_null(build::col("v"), true)),
            "v IS NOT NULL",
        );
    }
}
