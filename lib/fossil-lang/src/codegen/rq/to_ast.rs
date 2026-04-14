//! RQ → SQL emission.
//!
//! Builds a `sqlparser::ast::Query` from the `RelationalQuery` and stringifies
//! it once at the boundary. Cero `format!()` SQL building. Mirrors DataFusion's
//! `Unparser` pattern: `datafusion/sql/src/unparser/plan.rs::plan_to_sql`.

use sqlparser::ast::helpers::attached_token::AttachedToken;
use sqlparser::ast::{
    self, BinaryOperator, Cte, Expr, Ident, Join, JoinConstraint, JoinOperator, Query,
    Select, SelectItem, SetExpr, TableAlias, TableFactor, TableWithJoins, With,
};

use crate::dialect::{ScanStrategy, SqlDialect, table_ref};

use super::{JoinKind, RelationalQuery, Transform};

pub fn validate_duckdb_sql(sql: &str) -> Result<Vec<sqlparser::ast::Statement>, String> {
    use sqlparser::{dialect::DuckDbDialect, parser::Parser};
    Parser::parse_sql(&DuckDbDialect {}, sql).map_err(|e| e.to_string())
}

pub fn expr_to_sql(expr: &Expr, _rq: &RelationalQuery) -> String {
    expr.to_string()
}

/// Build a `sqlparser::ast::Query` from a RelationalQuery + dialect.
/// Each `Transform` becomes one `Cte`; the outer query selects from the last CTE.
/// Callers obtain the SQL string via `.to_string()` — there's no separate
/// `rq_to_sql` wrapper because that would just duplicate Display.
pub fn rq_to_query(rq: &RelationalQuery, dialect: &dyn SqlDialect) -> Query {
    if rq.transforms.is_empty() {
        return placeholder_select_one();
    }

    let mut ctes: Vec<Cte> = Vec::new();
    let mut last_alias: Option<Ident> = None;

    for transform in &rq.transforms {
        let (alias, query) = match transform {
            Transform::Scan { output, source } => {
                let factor = match dialect.scan_strategy(source) {
                    ScanStrategy::TableFactor(tf) => tf,
                    ScanStrategy::Preprocess { output_table, .. } => table_ref(output_table),
                };
                (
                    ident(rq.table_name(*output)),
                    select_star_from(vec![twj(factor)]),
                )
            }
            Transform::Project {
                input,
                output,
                columns,
            } => {
                let from = vec![twj(table_ref(rq.table_name(*input)))];
                let projection: Vec<SelectItem> = columns
                    .iter()
                    .map(|(col, expr)| {
                        let alias = ident(rq.col_name(*col));
                        if let Expr::Identifier(id) = expr {
                            if id.value == alias.value {
                                return SelectItem::UnnamedExpr(expr.clone());
                            }
                        }
                        SelectItem::ExprWithAlias {
                            expr: expr.clone(),
                            alias,
                        }
                    })
                    .collect();
                (ident(rq.table_name(*output)), select(projection, from, None))
            }
            Transform::Join {
                left,
                right,
                output,
                on,
                kind,
                suffix: _,
            } => {
                let join_op = match kind {
                    JoinKind::Inner => JoinOperator::Inner(JoinConstraint::On(join_on_expr(rq, *left, *right, on))),
                    JoinKind::Left => JoinOperator::LeftOuter(JoinConstraint::On(join_on_expr(rq, *left, *right, on))),
                };
                let from = vec![TableWithJoins {
                    relation: table_ref(rq.table_name(*left)),
                    joins: vec![Join {
                        relation: table_ref(rq.table_name(*right)),
                        global: false,
                        join_operator: join_op,
                    }],
                }];
                (
                    ident(rq.table_name(*output)),
                    select(vec![SelectItem::Wildcard(wildcard_default())], from, None),
                )
            }
            Transform::Filter {
                input,
                output,
                predicate,
            } => {
                let from = vec![twj(table_ref(rq.table_name(*input)))];
                (
                    ident(rq.table_name(*output)),
                    select(
                        vec![SelectItem::Wildcard(wildcard_default())],
                        from,
                        Some(predicate.clone()),
                    ),
                )
            }
        };
        last_alias = Some(alias.clone());
        ctes.push(Cte {
            alias: TableAlias {
                name: alias,
                columns: vec![],
            },
            query: Box::new(query),
            from: None,
            materialized: None,
            closing_paren_token: AttachedToken::empty(),
        });
    }

    let last = last_alias.expect("at least one transform");
    let outer_body = select_node(
        vec![SelectItem::Wildcard(wildcard_default())],
        vec![twj(table_ref(last.value))],
        None,
    );
    Query {
        with: Some(With {
            with_token: AttachedToken::empty(),
            recursive: false,
            cte_tables: ctes,
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

fn ident(name: &str) -> Ident {
    Ident::new(name.to_string())
}

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

fn select(projection: Vec<SelectItem>, from: Vec<TableWithJoins>, selection: Option<Expr>) -> Query {
    Query {
        with: None,
        body: Box::new(SetExpr::Select(Box::new(select_node(projection, from, selection)))),
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

fn select_star_from(from: Vec<TableWithJoins>) -> Query {
    select(vec![SelectItem::Wildcard(wildcard_default())], from, None)
}

fn wildcard_default() -> ast::WildcardAdditionalOptions {
    ast::WildcardAdditionalOptions::default()
}

fn placeholder_select_one() -> Query {
    select(
        vec![SelectItem::UnnamedExpr(Expr::Value(
            ast::Value::Number("1".into(), false).into(),
        ))],
        vec![],
        None,
    )
}

fn join_on_expr(
    rq: &RelationalQuery,
    left: super::TableId,
    right: super::TableId,
    on: &[(super::ColId, super::ColId)],
) -> Expr {
    let mut iter = on.iter().map(|(l, r)| {
        Expr::BinaryOp {
            left: Box::new(qualified_col(rq.table_name(left), rq.col_name(*l))),
            op: BinaryOperator::Eq,
            right: Box::new(qualified_col(rq.table_name(right), rq.col_name(*r))),
        }
    });
    let first = iter.next().unwrap_or(Expr::Value(ast::Value::Boolean(true).into()));
    iter.fold(first, |acc, next| Expr::BinaryOp {
        left: Box::new(acc),
        op: BinaryOperator::And,
        right: Box::new(next),
    })
}

fn qualified_col(table: &str, col: &str) -> Expr {
    Expr::CompoundIdentifier(vec![ident(table), ident(col)])
}

// Tests below only check that emitted SQL parses round-trip through
// DuckDbDialect — that invariant is now guaranteed by construction
// (we build via sqlparser AST nodes), but the tests still run as a safety net.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dialect::DefaultDialect;
    use crate::rq::{build, ColId, ScanSource, TableId};

    fn test_rq_parts() -> (RelationalQuery, TableId, ColId, ColId, ColId) {
        let mut rq = RelationalQuery::new();
        let id_col = rq.intern_col("id");
        let name_col = rq.intern_col("name");
        let age_col = rq.intern_col("age");
        let src = rq.alloc_table("src_1");
        rq.transforms.push(Transform::Scan {
            output: src,
            source: ScanSource {
                format: "csv".into(),
                path: "data.csv".into(),
                params: Default::default(),
            },
        });
        (rq, src, id_col, name_col, age_col)
    }

    #[test]
    fn single_scan_round_trips() {
        let (rq, ..) = test_rq_parts();
        let sql = rq_to_query(&rq, &DefaultDialect).to_string();
        validate_duckdb_sql(&sql).expect("emitted SQL must parse round-trip");
        assert!(sql.contains("read_csv"));
    }

    #[test]
    fn project_round_trips() {
        let (mut rq, src, _id, name, age) = test_rq_parts();
        let persons = rq.alloc_table("persons");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![(name, build::col("name")), (age, build::col("age"))],
        });
        let sql = rq_to_query(&rq, &DefaultDialect).to_string();
        validate_duckdb_sql(&sql).expect("project SQL must round-trip");
    }

    #[test]
    fn join_round_trips() {
        let (mut rq, src, id, ..) = test_rq_parts();
        let lookup = rq.alloc_table("lookup_1");
        let lookup_id = rq.intern_col("lookup_id");
        rq.transforms.push(Transform::Scan {
            output: lookup,
            source: ScanSource {
                format: "csv".into(),
                path: "lookup.csv".into(),
                params: Default::default(),
            },
        });
        let joined = rq.alloc_table("joined_1");
        rq.transforms.push(Transform::Join {
            left: src,
            right: lookup,
            output: joined,
            on: vec![(id, lookup_id)],
            kind: JoinKind::Inner,
            suffix: None,
        });
        let sql = rq_to_query(&rq, &DefaultDialect).to_string();
        assert!(sql.contains("INNER JOIN") || sql.contains("JOIN"));
        validate_duckdb_sql(&sql).expect("join SQL must round-trip");
    }

    #[test]
    fn expr_coalesce() {
        let mut rq = RelationalQuery::new();
        rq.intern_col("x");
        let expr = build::coalesce(build::col("x"), build::string_lit("default"));
        assert_eq!(expr_to_sql(&expr, &rq), "COALESCE(x, 'default')");
    }

    #[test]
    fn expr_concat() {
        let mut rq = RelationalQuery::new();
        rq.intern_col("base");
        rq.intern_col("id");
        let expr = build::concat(vec![
            build::string_lit("http://example.org/"),
            build::cast_varchar(build::col("id")),
        ]);
        assert_eq!(
            expr_to_sql(&expr, &rq),
            "CONCAT('http://example.org/', CAST(id AS VARCHAR))"
        );
    }

    #[test]
    fn expr_function() {
        let mut rq = RelationalQuery::new();
        rq.intern_col("email");
        let expr = build::func("SHA256", vec![build::cast_varchar(build::col("email"))]);
        assert_eq!(expr_to_sql(&expr, &rq), "SHA256(CAST(email AS VARCHAR))");
    }

    #[test]
    fn null_literal() {
        let rq = RelationalQuery::new();
        let expr = build::null_lit();
        assert_eq!(expr_to_sql(&expr, &rq), "NULL");
    }

    #[test]
    fn is_null_negated() {
        let mut rq = RelationalQuery::new();
        rq.intern_col("v");
        let expr = build::is_null(build::col("v"), true);
        assert_eq!(expr_to_sql(&expr, &rq), "v IS NOT NULL");
    }

    #[test]
    fn scan_with_project_round_trips() {
        let (mut rq, src, _id, name, age) = test_rq_parts();
        let persons = rq.alloc_table("persons");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![(name, build::col("name")), (age, build::col("age"))],
        });
        let sql = rq_to_query(&rq, &DefaultDialect).to_string();
        validate_duckdb_sql(&sql).expect("round-trip");
        assert!(sql.contains("persons"), "got: {sql}");
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

    #[test]
    fn single_scan_is_parseable_by_duckdb_dialect() {
        let (rq, ..) = test_rq_parts();
        let sql = rq_to_query(&rq, &DefaultDialect).to_string();
        validate_duckdb_sql(&sql).expect("must round-trip");
    }
}
