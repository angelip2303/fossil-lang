use std::collections::HashSet;

use polars::prelude::*;
use spargebra::algebra::{AggregateExpression, AggregateFunction, GraphPattern, OrderExpression};
use spargebra::term::{NamedNodePattern, TermPattern, TriplePattern, Variable};

use crate::error::RdfGraphError;
use crate::graph::RdfGraph;

use super::expr::{translate_expr, var_name};

/// Translate a SPARQL GraphPattern into a Polars LazyFrame.
pub fn translate_pattern(
    graph: &RdfGraph,
    pattern: &GraphPattern,
) -> Result<LazyFrame, RdfGraphError> {
    match pattern {
        GraphPattern::Bgp { patterns } => translate_bgp(graph, patterns),

        GraphPattern::Filter { expr, inner } => {
            let lf = translate_pattern(graph, inner)?;
            let filter = translate_expr(expr)?;
            Ok(lf.filter(filter))
        }

        GraphPattern::Join { left, right } => {
            let left_lf = translate_pattern(graph, left)?;
            let right_lf = translate_pattern(graph, right)?;
            join_on_shared_vars(left_lf, right_lf, left, right, JoinType::Inner)
        }

        GraphPattern::LeftJoin { left, right, expression } => {
            let left_lf = translate_pattern(graph, left)?;
            let right_lf = translate_pattern(graph, right)?;
            let mut joined = join_on_shared_vars(left_lf, right_lf, left, right, JoinType::Left)?;
            if let Some(expr) = expression {
                let filter = translate_expr(expr)?;
                joined = joined.filter(filter);
            }
            Ok(joined)
        }

        GraphPattern::Union { left, right } => {
            let left_lf = translate_pattern(graph, left)?;
            let right_lf = translate_pattern(graph, right)?;
            concat([left_lf, right_lf], UnionArgs { rechunk: true, to_supertypes: true, ..Default::default() })
                .map_err(Into::into)
        }

        GraphPattern::Group { inner, variables, aggregates } => {
            let lf = translate_pattern(graph, inner)?;
            let group_cols: Vec<Expr> = variables.iter().map(|v| col(var_name(v))).collect();

            if group_cols.is_empty() && aggregates.is_empty() {
                return Ok(lf);
            }

            let agg_exprs: Vec<Expr> = aggregates
                .iter()
                .map(|(var, agg)| translate_aggregate(agg, var))
                .collect::<Result<_, _>>()?;

            if group_cols.is_empty() {
                // No GROUP BY vars, just aggregate the whole frame
                Ok(lf.select(agg_exprs))
            } else {
                Ok(lf.group_by(group_cols).agg(agg_exprs))
            }
        }

        GraphPattern::Project { inner, variables } => {
            let lf = translate_pattern(graph, inner)?;
            let cols: Vec<Expr> = variables.iter().map(|v| col(var_name(v))).collect();
            Ok(lf.select(cols))
        }

        GraphPattern::OrderBy { inner, expression } => {
            let lf = translate_pattern(graph, inner)?;
            let mut sort_cols = Vec::new();
            let mut descending = Vec::new();
            for order_expr in expression {
                match order_expr {
                    OrderExpression::Asc(e) => {
                        sort_cols.push(translate_expr(e)?);
                        descending.push(false);
                    }
                    OrderExpression::Desc(e) => {
                        sort_cols.push(translate_expr(e)?);
                        descending.push(true);
                    }
                }
            }
            Ok(lf.sort_by_exprs(
                sort_cols,
                SortMultipleOptions {
                    descending,
                    ..Default::default()
                },
            ))
        }

        GraphPattern::Slice { inner, start, length } => {
            let lf = translate_pattern(graph, inner)?;
            match (start, length) {
                (0, Some(len)) => Ok(lf.limit(*len as u32)),
                (start, Some(len)) => Ok(lf.slice(*start as i64, *len as u32)),
                (start, None) if *start > 0 => Ok(lf.slice(*start as i64, u32::MAX)),
                _ => Ok(lf),
            }
        }

        GraphPattern::Distinct { inner } => {
            let lf = translate_pattern(graph, inner)?;
            Ok(lf.unique_stable(None, UniqueKeepStrategy::First))
        }

        GraphPattern::Extend { inner, variable, expression } => {
            let lf = translate_pattern(graph, inner)?;
            let expr = translate_expr(expression)?.alias(var_name(variable));
            Ok(lf.with_column(expr))
        }

        _ => Err(RdfGraphError::UnsupportedSparql(format!(
            "GraphPattern: {:?}",
            std::mem::discriminant(pattern)
        ))),
    }
}

/// Translate a Basic Graph Pattern (set of triple patterns).
fn translate_bgp(
    graph: &RdfGraph,
    patterns: &[TriplePattern],
) -> Result<LazyFrame, RdfGraphError> {
    if patterns.is_empty() {
        return Err(RdfGraphError::Other("Empty BGP".into()));
    }

    let mut result: Option<LazyFrame> = None;

    for tp in patterns {
        let lf = translate_triple_pattern(graph, tp)?;

        result = Some(match result {
            None => lf,
            Some(prev) => {
                let prev_vars = collect_pattern_vars_from_triple_patterns_before(patterns, tp);
                let current_vars = triple_pattern_vars(tp);
                let shared: Vec<String> = current_vars
                    .intersection(&prev_vars)
                    .cloned()
                    .collect();

                if shared.is_empty() {
                    prev.join(lf, vec![lit(1).alias("__k")], vec![lit(1).alias("__k")], JoinArgs::new(JoinType::Cross))
                } else {
                    let on: Vec<Expr> = shared.iter().map(|v| col(v.as_str())).collect();
                    prev.join(lf, on.clone(), on, JoinArgs::new(JoinType::Inner))
                }
            }
        });
    }

    result.ok_or_else(|| RdfGraphError::Other("Empty BGP".into()))
}

/// Translate a single triple pattern into a LazyFrame.
fn translate_triple_pattern(
    graph: &RdfGraph,
    tp: &TriplePattern,
) -> Result<LazyFrame, RdfGraphError> {
    let s_var = match &tp.subject {
        TermPattern::Variable(v) => Some(var_name(v)),
        _ => None,
    };
    let p_iri = match &tp.predicate {
        NamedNodePattern::NamedNode(n) => Some(n.as_str()),
        _ => None,
    };
    let o_var = match &tp.object {
        TermPattern::Variable(v) => Some(var_name(v)),
        _ => None,
    };
    let o_iri = match &tp.object {
        TermPattern::NamedNode(n) => Some(n.as_str()),
        TermPattern::Literal(l) => Some(l.value()),
        _ => None,
    };

    // Determine base scan based on what's known
    if let Some(pred_iri) = p_iri {
        // Known predicate → use PSO index
        let mut lf = graph.predicate(pred_iri)?;

        // Add predicate column (PSO files don't include it)
        // PSO files only have subject + object columns

        // Rename subject/object to variable names
        if let Some(s) = s_var {
            if s != "subject" {
                lf = lf.rename(["subject"], [s], true);
            }
        } else {
            // Subject is a constant
            if let TermPattern::NamedNode(n) = &tp.subject {
                lf = lf.filter(col("subject").eq(lit(n.as_str())));
            }
        }

        if let Some(o) = o_var {
            if o != "object" {
                lf = lf.rename(["object"], [o], true);
            }
        } else if let Some(o_val) = o_iri {
            // Object is a constant
            lf = lf.filter(col("object").eq(lit(o_val)));
        }

        Ok(lf)
    } else {
        // Variable predicate → need full scan
        let mut lf = graph.all_triples()?;

        if let Some(s) = s_var {
            if s != "subject" {
                lf = lf.rename(["subject"], [s], true);
            }
        } else if let TermPattern::NamedNode(n) = &tp.subject {
            lf = lf.filter(col("subject").eq(lit(n.as_str())));
        }

        if let NamedNodePattern::Variable(v) = &tp.predicate {
            let pv = var_name(v);
            if pv != "predicate" {
                lf = lf.rename(["predicate"], [pv], true);
            }
        }

        if let Some(o) = o_var {
            if o != "object" {
                lf = lf.rename(["object"], [o], true);
            }
        } else if let Some(o_val) = o_iri {
            lf = lf.filter(col("object").eq(lit(o_val)));
        }

        Ok(lf)
    }
}

/// Collect variables from a triple pattern.
fn triple_pattern_vars(tp: &TriplePattern) -> HashSet<String> {
    let mut vars = HashSet::new();
    if let TermPattern::Variable(v) = &tp.subject {
        vars.insert(var_name(v).to_string());
    }
    if let NamedNodePattern::Variable(v) = &tp.predicate {
        vars.insert(var_name(v).to_string());
    }
    if let TermPattern::Variable(v) = &tp.object {
        vars.insert(var_name(v).to_string());
    }
    vars
}

/// Collect all variables from triple patterns before the given one.
fn collect_pattern_vars_from_triple_patterns_before(
    patterns: &[TriplePattern],
    current: &TriplePattern,
) -> HashSet<String> {
    let mut vars = HashSet::new();
    for tp in patterns {
        if std::ptr::eq(tp, current) {
            break;
        }
        vars.extend(triple_pattern_vars(tp));
    }
    vars
}

/// Collect variables mentioned in a GraphPattern (for join detection).
fn pattern_variables(pattern: &GraphPattern) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_vars_recursive(pattern, &mut vars);
    vars
}

fn collect_vars_recursive(pattern: &GraphPattern, vars: &mut HashSet<String>) {
    match pattern {
        GraphPattern::Bgp { patterns } => {
            for tp in patterns {
                vars.extend(triple_pattern_vars(tp));
            }
        }
        GraphPattern::Join { left, right }
        | GraphPattern::LeftJoin { left, right, .. }
        | GraphPattern::Union { left, right } => {
            collect_vars_recursive(left, vars);
            collect_vars_recursive(right, vars);
        }
        GraphPattern::Filter { inner, .. }
        | GraphPattern::Project { inner, .. }
        | GraphPattern::OrderBy { inner, .. }
        | GraphPattern::Slice { inner, .. }
        | GraphPattern::Distinct { inner }
        | GraphPattern::Group { inner, .. } => {
            collect_vars_recursive(inner, vars);
        }
        GraphPattern::Extend { inner, variable, .. } => {
            collect_vars_recursive(inner, vars);
            vars.insert(var_name(variable).to_string());
        }
        _ => {}
    }
}

/// Join two LazyFrames on shared variables between two patterns.
fn join_on_shared_vars(
    left_lf: LazyFrame,
    right_lf: LazyFrame,
    left_pattern: &GraphPattern,
    right_pattern: &GraphPattern,
    join_type: JoinType,
) -> Result<LazyFrame, RdfGraphError> {
    let left_vars = pattern_variables(left_pattern);
    let right_vars = pattern_variables(right_pattern);
    let shared: Vec<String> = left_vars.intersection(&right_vars).cloned().collect();

    if shared.is_empty() {
        Ok(left_lf.join(right_lf, vec![lit(1).alias("__k")], vec![lit(1).alias("__k")], JoinArgs::new(JoinType::Cross)))
    } else {
        let on: Vec<Expr> = shared.iter().map(|v| col(v.as_str())).collect();
        Ok(left_lf.join(right_lf, on.clone(), on, JoinArgs::new(join_type)))
    }
}

/// Translate a SPARQL aggregate expression.
fn translate_aggregate(
    agg: &AggregateExpression,
    result_var: &Variable,
) -> Result<Expr, RdfGraphError> {
    let alias = var_name(result_var);

    match agg {
        AggregateExpression::CountSolutions { distinct } => {
            let counted = if *distinct {
                lit(1).n_unique()
            } else {
                lit(1).count()
            };
            Ok(counted.alias(alias))
        }
        AggregateExpression::FunctionCall { name, expr, distinct } => {
            let e = translate_expr(expr)?;
            let result = match name {
                AggregateFunction::Count => {
                    if *distinct { e.n_unique() } else { e.count() }
                }
                AggregateFunction::Sum => e.cast(DataType::Float64).sum(),
                AggregateFunction::Avg => e.cast(DataType::Float64).mean(),
                AggregateFunction::Min => e.min(),
                AggregateFunction::Max => e.max(),
                _ => {
                    return Err(RdfGraphError::UnsupportedSparql(format!(
                        "Aggregate function: {:?}",
                        name
                    )));
                }
            };
            Ok(result.alias(alias))
        }
    }
}
