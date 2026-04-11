//! RQ → SQL emission.
//!
//! Converts a RelationalQuery into a single SQL string with CTEs.
//! Each Transform becomes one CTE. The result is a single query
//! that DuckDB executes and optimizes.
//!
//! Reference: PRQL's rq_to_sql, Ibis's SQLGlot emission.

use crate::dialect::{ScanStrategy, SqlDialect};

use super::{
    ColId, JoinKind, RelationalQuery, RqExpr, RqLiteral, Transform,
};

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
                        let sql = expr_to_sql(expr, rq);
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
                    expr_to_sql(predicate, rq),
                );
                ctes.push((rq.table_name(*output).to_string(), sql));
            }

            Transform::ApplyTransforms {
                input,
                output,
                ops,
            } => {
                let items: Vec<String> = ops
                    .iter()
                    .map(|(col, sql_expr)| {
                        format!("{sql_expr} AS {}", rq.col_name(*col))
                    })
                    .collect();
                // Select transformed columns + all others via *
                // TODO: proper column tracking to avoid SELECT * ambiguity
                let sql = if items.is_empty() {
                    format!("SELECT * FROM {}", rq.table_name(*input))
                } else {
                    format!(
                        "SELECT *, {} FROM {}",
                        items.join(", "),
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

/// Convert an RqExpr to a SQL string.
pub fn expr_to_sql(expr: &RqExpr, rq: &RelationalQuery) -> String {
    match expr {
        RqExpr::Col(id) => rq.col_name(*id).to_string(),

        RqExpr::Lit(lit) => match lit {
            RqLiteral::Integer(i) => i.to_string(),
            RqLiteral::String(s) => format!("'{}'", s.replace('\'', "''")),
            RqLiteral::Boolean(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
            RqLiteral::Null => "NULL".to_string(),
        },

        RqExpr::Coalesce(a, b) => {
            format!(
                "COALESCE({}, {})",
                expr_to_sql(a, rq),
                expr_to_sql(b, rq)
            )
        }

        RqExpr::Concat(parts) => {
            let args: Vec<String> = parts.iter().map(|p| expr_to_sql(p, rq)).collect();
            format!("CONCAT({})", args.join(", "))
        }

        RqExpr::Cast(e, ty) => {
            format!("CAST({} AS {ty})", expr_to_sql(e, rq))
        }

        RqExpr::Func { name, args } => {
            let arg_strs: Vec<String> = args.iter().map(|a| expr_to_sql(a, rq)).collect();
            format!("{name}({})", arg_strs.join(", "))
        }

        RqExpr::IsNull(e, negated) => {
            let not = if *negated { "NOT " } else { "" };
            format!("{} IS {not}NULL", expr_to_sql(e, rq))
        }
    }
}

// ── Table ID helper for expr_to_sql ──────────────────────────────────

impl RqExpr {
    /// Create a column reference.
    pub fn col(id: ColId) -> Self {
        Self::Col(id)
    }

    /// Create a string literal.
    pub fn str_lit(s: impl Into<String>) -> Self {
        Self::Lit(RqLiteral::String(s.into()))
    }

    /// Create an integer literal.
    pub fn int_lit(i: i64) -> Self {
        Self::Lit(RqLiteral::Integer(i))
    }

    /// Create COALESCE(self, other).
    pub fn coalesce(self, other: Self) -> Self {
        Self::Coalesce(Box::new(self), Box::new(other))
    }

    /// Create CAST(self AS ty).
    pub fn cast(self, ty: impl Into<String>) -> Self {
        Self::Cast(Box::new(self), ty.into())
    }

    /// Create a function call.
    pub fn func(name: impl Into<String>, args: Vec<Self>) -> Self {
        Self::Func {
            name: name.into(),
            args,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dialect::DefaultDialect;
    use crate::rq::{ScanSource, TableId, ColId};

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
    fn scan_with_project() {
        let (mut rq, src, _id, name, age) = test_rq_parts();
        let persons = rq.alloc_table("persons");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![
                (name, RqExpr::col(name)),
                (age, RqExpr::col(age)),
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
        let rq = RelationalQuery::new();
        let expr = RqExpr::col(ColId(0)).coalesce(RqExpr::str_lit("default"));
        // Need a column name for ColId(0)
        let mut rq = rq;
        rq.intern_col("x");
        assert_eq!(expr_to_sql(&expr, &rq), "COALESCE(x, 'default')");
    }

    #[test]
    fn expr_concat() {
        let mut rq = RelationalQuery::new();
        rq.intern_col("base");
        rq.intern_col("id");
        let expr = RqExpr::Concat(vec![
            RqExpr::str_lit("http://example.org/"),
            RqExpr::col(ColId(1)).cast("VARCHAR"),
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
        let expr = RqExpr::func("SHA256", vec![RqExpr::col(ColId(0)).cast("VARCHAR")]);
        assert_eq!(
            expr_to_sql(&expr, &rq),
            "SHA256(CAST(email AS VARCHAR))"
        );
    }
}
