//! RQ → SQL emission.
//!
//! Converts a RelationalQuery into a single SQL string with CTEs.
//! Each Transform becomes one CTE. The result is a single query
//! that DuckDB executes and optimizes.
//!
//! Column expressions are stored as `sqlparser::ast::Expr` and serialized
//! via their `Display` impl — no manual SQL string building. This matches
//! DataFusion's `Unparser` pattern: `datafusion/sql/src/unparser/expr.rs`.
//!
//! Uses `sqlparser-rs` for round-trip validation: every emitted SQL
//! string is parseable by DuckDbDialect, guaranteeing structural correctness.

use sqlparser::ast::Expr;

use crate::dialect::{ScanStrategy, SqlDialect};

use super::{JoinKind, RelationalQuery, Transform};

/// Validate that a SQL string is parseable by DuckDB dialect.
/// Returns the parsed statements on success. Used as a safety check
/// after emission: if this fails, the emitter has a bug.
///
/// This is the same pattern DataFusion uses: construct a sqlparser AST,
/// serialize to string, optionally re-parse for round-trip validation.
pub fn validate_duckdb_sql(sql: &str) -> Result<Vec<sqlparser::ast::Statement>, String> {
    use sqlparser::{dialect::DuckDbDialect, parser::Parser};
    Parser::parse_sql(&DuckDbDialect {}, sql).map_err(|e| e.to_string())
}

/// Convert a sqlparser `Expr` to its SQL string form via `Display`.
/// Thin wrapper so the rest of the codebase has one canonical name.
pub fn expr_to_sql(expr: &Expr, _rq: &RelationalQuery) -> String {
    expr.to_string()
}

/// Convert a RelationalQuery to a SQL string with CTEs.
///
/// The `dialect` maps each `ScanSource` to SQL or preprocessing instructions.
pub fn rq_to_sql(rq: &RelationalQuery, dialect: &dyn SqlDialect) -> String {
    if rq.transforms.is_empty() {
        return "SELECT 1".to_string();
    }

    let mut ctes: Vec<(String, String)> = Vec::new();

    for transform in &rq.transforms {
        match transform {
            Transform::Scan { output, source } => {
                let sql = match dialect.scan_strategy(source) {
                    ScanStrategy::Sql(s) => s,
                    ScanStrategy::Preprocess { output_table, .. } => {
                        format!("SELECT * FROM {output_table}")
                    }
                };
                ctes.push((rq.table_name(*output).to_string(), sql));
            }

            Transform::Project {
                input,
                output,
                columns,
            } => {
                let items: Vec<String> = columns
                    .iter()
                    .map(|(col, expr)| {
                        let sql = expr.to_string();
                        let name = rq.col_name(*col);
                        if sql == name {
                            name.to_string()
                        } else {
                            format!("{sql} AS {name}")
                        }
                    })
                    .collect();
                let sql = format!(
                    "SELECT {} FROM {}",
                    items.join(", "),
                    rq.table_name(*input)
                );
                ctes.push((rq.table_name(*output).to_string(), sql));
            }

            Transform::Join {
                left,
                right,
                output,
                on,
                kind,
                suffix: _,
            } => {
                let join_kw = match kind {
                    JoinKind::Inner => "INNER JOIN",
                    JoinKind::Left => "LEFT JOIN",
                };
                let on_clause: Vec<String> = on
                    .iter()
                    .map(|(l, r)| {
                        format!(
                            "{}.{} = {}.{}",
                            rq.table_name(*left),
                            rq.col_name(*l),
                            rq.table_name(*right),
                            rq.col_name(*r),
                        )
                    })
                    .collect();
                let sql = format!(
                    "SELECT * FROM {} {join_kw} {} ON {}",
                    rq.table_name(*left),
                    rq.table_name(*right),
                    on_clause.join(" AND "),
                );
                ctes.push((rq.table_name(*output).to_string(), sql));
            }

            Transform::Filter {
                input,
                output,
                predicate,
            } => {
                let sql = format!(
                    "SELECT * FROM {} WHERE {}",
                    rq.table_name(*input),
                    predicate,
                );
                ctes.push((rq.table_name(*output).to_string(), sql));
            }

            Transform::ApplyTransforms {
                input,
                output,
                ops,
            } => {
                // Use DuckDB's `SELECT * REPLACE (expr AS col, ...)` clause to
                // replace transformed columns in-place without duplication.
                let sql = if ops.is_empty() {
                    format!("SELECT * FROM {}", rq.table_name(*input))
                } else {
                    let replace_items: Vec<String> = ops
                        .iter()
                        .map(|(col, sql_expr)| {
                            format!("{sql_expr} AS {}", rq.col_name(*col))
                        })
                        .collect();
                    format!(
                        "SELECT * REPLACE ({}) FROM {}",
                        replace_items.join(", "),
                        rq.table_name(*input)
                    )
                };
                ctes.push((rq.table_name(*output).to_string(), sql));
            }
        }
    }

    let last_name = &ctes.last().expect("at least one transform").0;
    let cte_defs: Vec<String> = ctes
        .iter()
        .map(|(name, sql)| format!("{name} AS (\n  {sql}\n)"))
        .collect();

    format!("WITH\n{}\nSELECT * FROM {last_name}", cte_defs.join(",\n"))
}

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
    fn single_scan() {
        let (rq, ..) = test_rq_parts();
        let sql = rq_to_sql(&rq, &DefaultDialect);
        assert!(sql.contains("src_1 AS ("));
        assert!(sql.contains("read_csv('data.csv')"));
        assert!(sql.contains("SELECT * FROM src_1"));
    }

    #[test]
    fn single_scan_is_parseable_by_duckdb_dialect() {
        let (rq, ..) = test_rq_parts();
        let sql = rq_to_sql(&rq, &DefaultDialect);
        validate_duckdb_sql(&sql).expect("emitted SQL must be parseable by DuckDB");
    }

    #[test]
    fn project_sql_is_parseable() {
        let (mut rq, src, _id, name, age) = test_rq_parts();
        let persons = rq.alloc_table("persons");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![
                (name, build::col("name")),
                (age, build::col("age")),
            ],
        });
        let sql = rq_to_sql(&rq, &DefaultDialect);
        validate_duckdb_sql(&sql).expect("emitted SQL must be parseable by DuckDB");
    }

    #[test]
    fn apply_transforms_uses_select_star_replace() {
        let (mut rq, src, _id, name, _age) = test_rq_parts();
        let out = rq.alloc_table("cleaned");
        rq.transforms.push(Transform::ApplyTransforms {
            input: src,
            output: out,
            ops: vec![(name, "TRIM(name)".to_string())],
        });
        let sql = rq_to_sql(&rq, &DefaultDialect);
        assert!(
            sql.contains("SELECT * REPLACE"),
            "ApplyTransforms must use SELECT * REPLACE(...) to avoid column ambiguity, got: {sql}"
        );
        validate_duckdb_sql(&sql).expect("SELECT * REPLACE must be parseable by DuckDB dialect");
    }

    #[test]
    fn apply_transforms_empty_ops_falls_back_to_select_star() {
        let (mut rq, src, ..) = test_rq_parts();
        let out = rq.alloc_table("passthrough");
        rq.transforms.push(Transform::ApplyTransforms {
            input: src,
            output: out,
            ops: vec![],
        });
        let sql = rq_to_sql(&rq, &DefaultDialect);
        assert!(sql.contains("SELECT * FROM"));
        validate_duckdb_sql(&sql).expect("passthrough SQL must be parseable");
    }

    #[test]
    fn scan_with_project() {
        let (mut rq, src, _id, name, age) = test_rq_parts();
        let persons = rq.alloc_table("persons");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![
                (name, build::col("name")),
                (age, build::col("age")),
            ],
        });
        let sql = rq_to_sql(&rq, &DefaultDialect);
        assert!(sql.contains("persons AS ("));
        assert!(sql.contains("SELECT name, age FROM src_1"));
        assert!(sql.contains("SELECT * FROM persons"));
    }

    #[test]
    fn join_two_tables() {
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
        let sql = rq_to_sql(&rq, &DefaultDialect);
        assert!(sql.contains("INNER JOIN"));
        assert!(sql.contains("src_1.id = lookup_1.lookup_id"));
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
        let expr = build::func(
            "SHA256",
            vec![build::cast_varchar(build::col("email"))],
        );
        assert_eq!(expr_to_sql(&expr, &rq), "SHA256(CAST(email AS VARCHAR))");
    }

    #[test]
    fn expr_round_trip_through_duckdb_dialect() {
        // Build a non-trivial expression and verify sqlparser can re-parse
        // its `Display` output. This is the key invariant of using sqlparser
        // as the backbone: emit → parse → emit yields equivalent ASTs.
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
}
