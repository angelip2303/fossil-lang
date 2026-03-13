use polars::prelude::*;
use spargebra::algebra::Expression;
use spargebra::term::{NamedNode, Variable};

use crate::error::RdfGraphError;

/// Translate a spargebra Expression into a Polars Expr.
pub fn translate_expr(expression: &Expression) -> Result<Expr, RdfGraphError> {
    match expression {
        Expression::Equal(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.eq(r))
        }
        Expression::Greater(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.gt(r))
        }
        Expression::GreaterOrEqual(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.gt_eq(r))
        }
        Expression::Less(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.lt(r))
        }
        Expression::LessOrEqual(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.lt_eq(r))
        }
        Expression::And(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.and(r))
        }
        Expression::Or(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l.or(r))
        }
        Expression::Not(inner) => {
            let e = translate_expr(inner)?;
            Ok(e.not())
        }
        Expression::Add(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l + r)
        }
        Expression::Subtract(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l - r)
        }
        Expression::Multiply(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l * r)
        }
        Expression::Divide(left, right) => {
            let l = translate_expr(left)?;
            let r = translate_expr(right)?;
            Ok(l / r)
        }
        Expression::Variable(var) => Ok(col(var.as_str())),
        Expression::NamedNode(node) => Ok(lit(node.as_str())),
        Expression::Literal(literal) => {
            let value = literal.value();
            let datatype = literal.datatype().as_str();
            match datatype {
                "http://www.w3.org/2001/XMLSchema#integer"
                | "http://www.w3.org/2001/XMLSchema#int"
                | "http://www.w3.org/2001/XMLSchema#long" => {
                    if let Ok(n) = value.parse::<i64>() {
                        Ok(lit(n))
                    } else {
                        Ok(lit(value))
                    }
                }
                "http://www.w3.org/2001/XMLSchema#double"
                | "http://www.w3.org/2001/XMLSchema#float"
                | "http://www.w3.org/2001/XMLSchema#decimal" => {
                    if let Ok(n) = value.parse::<f64>() {
                        Ok(lit(n))
                    } else {
                        Ok(lit(value))
                    }
                }
                "http://www.w3.org/2001/XMLSchema#boolean" => {
                    Ok(lit(value == "true" || value == "1"))
                }
                _ => Ok(lit(value)),
            }
        }
        Expression::Bound(var) => Ok(col(var.as_str()).is_not_null()),
        Expression::FunctionCall(function, args) => {
            translate_function_call(function, args)
        }
        _ => Err(RdfGraphError::UnsupportedSparql(format!(
            "Expression type not supported"
        ))),
    }
}

fn translate_function_call(
    function: &spargebra::algebra::Function,
    args: &[Expression],
) -> Result<Expr, RdfGraphError> {
    use spargebra::algebra::Function;

    match function {
        Function::Str => {
            let inner = translate_expr(&args[0])?;
            Ok(inner.cast(DataType::String))
        }
        Function::LCase => {
            let inner = translate_expr(&args[0])?;
            Ok(inner.str().to_lowercase())
        }
        Function::UCase => {
            let inner = translate_expr(&args[0])?;
            Ok(inner.str().to_uppercase())
        }
        Function::Contains => {
            let haystack = translate_expr(&args[0])?;
            let needle = translate_expr(&args[1])?;
            Ok(haystack.str().contains(needle, true))
        }
        Function::StrLen => {
            let inner = translate_expr(&args[0])?;
            Ok(inner.str().len_chars())
        }
        _ => Err(RdfGraphError::UnsupportedSparql(format!(
            "Function not supported"
        ))),
    }
}

/// Get the variable name from a SPARQL Variable.
pub fn var_name(var: &Variable) -> &str {
    var.as_str()
}

/// Get the IRI string from a NamedNode.
pub fn node_iri(node: &NamedNode) -> &str {
    node.as_str()
}
