//! RQ → SQL emission.
//!
//! Assembles a `sqlparser::ast::Query` from the lowered CTEs and stringifies
//! it at the boundary. Mirrors DataFusion's `Unparser` pattern: build sqlparser
//! AST nodes, call `.to_string()` once (`datafusion/sql/src/unparser/plan.rs`).
//!
//! The compiled SQL references sources and CTEs by name only. Source
//! resolution (turning `FROM src_csv_1` into `FROM read_csv('…')` or a
//! host-preprocessed temp table) happens in the host's catalog layer,
//! reading the `sources` manifest on [`RelationalQuery`]. fossil-lang
//! has no dialect knowledge.

use sqlparser::ast::helpers::attached_token::AttachedToken;
use sqlparser::ast::{
    self, Expr, Ident, Query, Select, SelectItem, SetExpr, TableFactor, TableWithJoins, With,
};

use super::RelationalQuery;

pub fn validate_duckdb_sql(sql: &str) -> Result<Vec<sqlparser::ast::Statement>, String> {
    use sqlparser::{dialect::DuckDbDialect, parser::Parser};
    Parser::parse_sql(&DuckDbDialect {}, sql).map_err(|e| e.to_string())
}

pub fn expr_to_sql(expr: &Expr) -> String {
    expr.to_string()
}

/// Build a `sqlparser::ast::Query` from a [`RelationalQuery`].
///
/// If the RQ has CTEs, emits `WITH <ctes> SELECT * FROM <last_cte>`.
/// If the RQ is empty, emits `SELECT 1` as a non-zero placeholder.
/// Callers obtain the SQL string via `.to_string()`.
pub fn rq_to_query(rq: &RelationalQuery) -> Query {
    let Some(last) = rq.ctes.last() else {
        return placeholder_select_one();
    };
    let outer_body = select_node(
        vec![SelectItem::Wildcard(wildcard_default())],
        vec![twj(table_ref(&last.alias.name))],
        None,
    );
    Query {
        with: Some(With {
            with_token: AttachedToken::empty(),
            recursive: false,
            cte_tables: rq.ctes.clone(),
        }),
        body: Box::new(SetExpr::Select(Box::new(outer_body))),
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

// ── helpers ─────────────────────────────────────────────────────────

fn twj(relation: TableFactor) -> TableWithJoins {
    TableWithJoins {
        relation,
        joins: vec![],
    }
}

fn select_node(
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

fn wildcard_default() -> ast::WildcardAdditionalOptions {
    ast::WildcardAdditionalOptions::default()
}

fn table_ref(name: &Ident) -> TableFactor {
    TableFactor::Table {
        name: ast::ObjectName(vec![ast::ObjectNamePart::Identifier(name.clone())]),
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

fn placeholder_select_one() -> Query {
    Query {
        with: None,
        body: Box::new(SetExpr::Select(Box::new(select_node(
            vec![SelectItem::UnnamedExpr(Expr::Value(
                ast::Value::Number("1".into(), false).into(),
            ))],
            vec![],
            None,
        )))),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rq::{build, SourceRef};

    fn source(alias: &str, format: &str, path: &str) -> SourceRef {
        SourceRef {
            alias: Ident::new(alias),
            format: format.into(),
            path: path.into(),
            params: Default::default(),
        }
    }

    #[test]
    fn empty_rq_is_placeholder() {
        let rq = RelationalQuery::new();
        let sql = rq_to_query(&rq).to_string();
        validate_duckdb_sql(&sql).expect("placeholder SQL must round-trip");
    }

    #[test]
    fn single_source_name_flows_through_sql() {
        use sqlparser::ast::{Cte, TableAlias};
        // Simulate a lowering that registered a source and emitted one CTE
        // whose body references it by alias.
        let mut rq = RelationalQuery::new();
        rq.sources.push(source("src_csv_1", "csv", "data.csv"));
        let body = select_node(
            vec![SelectItem::Wildcard(wildcard_default())],
            vec![twj(table_ref(&Ident::new("src_csv_1")))],
            None,
        );
        let query = Query {
            with: None,
            body: Box::new(SetExpr::Select(Box::new(body))),
            order_by: None,
            limit_clause: None,
            fetch: None,
            locks: vec![],
            for_clause: None,
            settings: None,
            format_clause: None,
            pipe_operators: vec![],
        };
        rq.ctes.push(Cte {
            alias: TableAlias {
                name: Ident::new("persons_1"),
                columns: vec![],
            },
            query: Box::new(query),
            from: None,
            materialized: None,
            closing_paren_token: AttachedToken::empty(),
        });
        let sql = rq_to_query(&rq).to_string();
        validate_duckdb_sql(&sql).expect("emitted SQL must round-trip");
        assert!(sql.contains("src_csv_1"), "got: {sql}");
        assert!(sql.contains("persons_1"), "got: {sql}");
        // crucially, no read_csv: source resolution is host-side.
        assert!(!sql.contains("read_csv"), "got: {sql}");
    }

    #[test]
    fn expr_coalesce() {
        let expr = build::coalesce(build::col("x"), build::string_lit("default"));
        assert_eq!(expr_to_sql(&expr), "COALESCE(x, 'default')");
    }

    #[test]
    fn expr_concat() {
        let expr = build::concat(vec![
            build::string_lit("http://example.org/"),
            build::cast_varchar(build::col("id")),
        ]);
        assert_eq!(
            expr_to_sql(&expr),
            "CONCAT('http://example.org/', CAST(id AS VARCHAR))"
        );
    }

    #[test]
    fn expr_function() {
        let expr = build::func("SHA256", vec![build::cast_varchar(build::col("email"))]);
        assert_eq!(expr_to_sql(&expr), "SHA256(CAST(email AS VARCHAR))");
    }

    #[test]
    fn null_literal() {
        let expr = build::null_lit();
        assert_eq!(expr_to_sql(&expr), "NULL");
    }

    #[test]
    fn is_null_negated() {
        let expr = build::is_null(build::col("v"), true);
        assert_eq!(expr_to_sql(&expr), "v IS NOT NULL");
    }

    #[test]
    fn expr_round_trip_through_duckdb_dialect() {
        use sqlparser::{dialect::DuckDbDialect, parser::Parser};
        let expr = build::concat(vec![
            build::string_lit("prefix-"),
            build::cast_varchar(build::col("id")),
        ]);
        let sql = format!("SELECT {expr}");
        let parsed = Parser::parse_sql(&DuckDbDialect {}, &sql)
            .expect("emitted expression must round-trip through DuckDbDialect");
        assert_eq!(parsed.len(), 1);
    }
}
