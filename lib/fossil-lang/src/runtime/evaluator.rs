//! THIR Runtime Evaluator
//!
//! This module provides runtime evaluation of THIR (Typed High-level IR) expressions.
//! It enables stdlib functions to execute user-provided lambdas and evaluate field access.

use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::ast::thir::{ExprId, ExprKind, StmtId, TypedHir};
use crate::context::Symbol;
use crate::error::RuntimeError;
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

// Re-export Environment for convenience
pub use crate::runtime::value::Environment as ThirEnvironment;

/// THIR Expression Evaluator
///
/// Evaluates typed HIR expressions at runtime. This is used by stdlib functions
/// to execute user-provided lambdas and evaluate field access on runtime values.
pub struct ThirEvaluator<'a> {
    /// Reference to the typed HIR being evaluated
    thir: &'a TypedHir,

    /// Reference to global context for function lookup
    gcx: &'a GlobalContext,

    /// Current environment for variable bindings
    env: ThirEnvironment,
}

impl<'a> ThirEvaluator<'a> {
    /// Create a new evaluator with the given context and environment
    pub fn new(thir: &'a TypedHir, gcx: &'a GlobalContext, env: ThirEnvironment) -> Self {
        Self { thir, gcx, env }
    }

    /// Evaluate an expression and return its value
    pub fn eval(&mut self, expr_id: ExprId) -> Result<Value, RuntimeError> {
        let expr = self.thir.exprs.get(expr_id);

        match &expr.kind {
            ExprKind::Literal(lit) => {
                use crate::ast::ast::Literal;
                match lit {
                    Literal::Integer(i) => Ok(Value::Int(*i)),
                    Literal::String(s) => {
                        let str_val = self.gcx.interner.resolve(*s);
                        Ok(Value::String(Arc::from(str_val)))
                    }
                    Literal::Boolean(b) => Ok(Value::Bool(*b)),
                }
            }

            ExprKind::Identifier(def_id) => {
                // In THIR, identifiers can be:
                // 1. Local variables (resolved to their DefId)
                // 2. Functions (registered stdlib functions)
                // 3. Module references

                // First check if it's a local variable in the environment
                // We'll use a heuristic: try to get the symbol name from the def
                let def = self.gcx.definitions.get(*def_id);

                // Try to look up by symbol in environment first (for local variables)
                if let Some(val) = self.env.lookup(def.name) {
                    return Ok(val.clone());
                }

                // Otherwise, it's a function or module binding
                use crate::context::DefKind;
                match &def.kind {
                    DefKind::Func(Some(func)) => Ok(Value::BuiltinFunction(*def_id, func.clone())),
                    DefKind::Let => {
                        // Local variable not in environment - error
                        Err(self.make_error(format!(
                            "Variable {} not found in environment",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                    _ => Err(self.make_error("Invalid identifier reference")),
                }
            }

            ExprKind::Record(fields) => self.eval_record(fields),

            ExprKind::List(items) => self.eval_list(items),

            ExprKind::Function { params, body } => Ok(Value::Closure {
                params: params.clone(),
                body: *body,
                env: Rc::new(self.env.clone()),
            }),

            ExprKind::Application { callee, args } => self.eval_application(*callee, args),

            ExprKind::FieldAccess { expr, field } => self.eval_field_access(*expr, *field),

            ExprKind::Block { stmts } => self.eval_block(stmts),

            ExprKind::Unit => Ok(Value::Unit),
        }
    }

    /// Evaluate a record expression
    fn eval_record(&mut self, fields: &[(Symbol, ExprId)]) -> Result<Value, RuntimeError> {
        if fields.is_empty() {
            return Ok(Value::LazyFrame(LazyFrame::default()));
        }

        let mut series_vec = Vec::new();

        for (name, expr_id) in fields {
            let value = self.eval(*expr_id)?;
            let name_str = PlSmallStr::from_str(self.gcx.interner.resolve(*name));

            let series = match value {
                Value::Int(i) => Series::new(name_str, &[i]).into_column(),
                Value::String(s) => Series::new(name_str, &[s.as_ref()]).into_column(),
                Value::Bool(b) => Series::new(name_str, &[b]).into_column(),
                Value::Series(mut s) => {
                    s.rename(name_str);
                    s.into_column()
                }
                _ => {
                    return Err(self.make_error("Unsupported value type in record field"));
                }
            };

            series_vec.push(series);
        }

        let df = DataFrame::new(series_vec)
            .map_err(|e| self.make_error(&format!("Failed to create DataFrame: {}", e)))?;

        Ok(Value::LazyFrame(df.lazy()))
    }

    /// Evaluate a list expression
    fn eval_list(&mut self, items: &[ExprId]) -> Result<Value, RuntimeError> {
        if items.is_empty() {
            return Ok(Value::Series(Series::default()));
        }

        let values: Vec<Value> = items
            .iter()
            .map(|item| self.eval(*item))
            .collect::<Result<Vec<_>, _>>()?;

        match &values[0] {
            Value::Int(_) => {
                let ints: Vec<i64> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Int(i) => i,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                Ok(Value::Series(Series::from_iter(ints)))
            }

            Value::String(_) => {
                let strings: Vec<&str> = values
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.as_ref(),
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                Ok(Value::Series(Series::from_iter(strings)))
            }

            Value::Bool(_) => {
                let bools: Vec<bool> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Bool(b) => b,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                Ok(Value::Series(Series::from_iter(bools)))
            }

            Value::LazyFrame(_) => {
                let dfs: Vec<LazyFrame> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::LazyFrame(lf) => lf,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                let concatenated = concat(dfs, UnionArgs::default()).map_err(|e| {
                    self.make_error(&format!("Failed to concatenate LazyFrames: {}", e))
                })?;
                Ok(Value::LazyFrame(concatenated))
            }

            Value::Extension { .. } => {
                // For extensions (like Entity), return as List
                // Serialization layer will handle batch optimization
                Ok(Value::List(values))
            }

            _ => Err(self.make_error("Unsupported list element type")),
        }
    }

    /// Evaluate a function application
    fn eval_application(&mut self, callee: ExprId, args: &[ExprId]) -> Result<Value, RuntimeError> {
        let callee_val = self.eval(callee)?;

        // Evaluate arguments
        let arg_values: Vec<Value> = args
            .iter()
            .map(|arg| self.eval(*arg))
            .collect::<Result<Vec<_>, _>>()?;

        match callee_val {
            Value::Closure { params, body, env } => {
                // Execute closure with new environment
                let mut closure_env = (*env).clone();

                // Bind parameters to arguments
                for (param, arg) in params.iter().zip(arg_values.iter()) {
                    closure_env.bind(param.name, arg.clone());
                }

                // Evaluate body with closure environment
                let saved_env = std::mem::replace(&mut self.env, closure_env);
                let result = self.eval(body)?;
                self.env = saved_env;

                Ok(result)
            }

            Value::BuiltinFunction(_def_id, func) => {
                // Call builtin function with type context
                let mut ctx = RuntimeContext::new(self.gcx, self.thir);

                // Try to extract DefId from first argument's type (for functions like Entity::with_id)
                // This handles cases with explicit type annotations
                if !args.is_empty() {
                    let first_arg_expr = self.thir.exprs.get(args[0]);
                    let arg_type = self.thir.types.get(first_arg_expr.ty);

                    // If it's a Named type, extract the DefId
                    if let crate::ast::thir::TypeKind::Named(def_id) = &arg_type.kind {
                        ctx = ctx.with_type(*def_id);
                    }
                }

                func.call(arg_values, &ctx)
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    /// Evaluate field access (e.g., `row.id`)
    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, RuntimeError> {
        let value = self.eval(expr)?;

        match value {
            Value::LazyFrame(lf) => {
                // Extract field from LazyFrame
                let field_name = self.gcx.interner.resolve(field);

                // Optimization: Use select() to only collect the column we need
                // This avoids materializing all columns when we only need one
                let lf_selected = lf.select([col(field_name)]).with_projection_pushdown(true);

                // Collect only the selected column
                let df = lf_selected
                    .collect()
                    .map_err(|e| self.make_error(&format!("Failed to collect LazyFrame: {}", e)))?;

                // Get the column (should be the only one)
                let column = df.column(field_name).map_err(|e| {
                    self.make_error(&format!("Field '{}' not found: {}", field_name, e))
                })?;

                // Convert Column to Series
                let series = column.as_materialized_series().clone();

                // If it's a single row, return scalar value
                if df.height() == 1 {
                    self.series_to_scalar(&series)
                } else {
                    // Multiple rows, return Series
                    Ok(Value::Series(series))
                }
            }

            _ => Err(self.make_error("Field access on non-record value")),
        }
    }

    /// Evaluate a block of statements
    fn eval_block(&mut self, stmts: &[StmtId]) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Unit;

        for stmt_id in stmts {
            let stmt = self.thir.stmts.get(*stmt_id);

            match &stmt.kind {
                crate::ast::thir::StmtKind::Let { name, value } => {
                    let val = self.eval(*value)?;
                    self.env.bind(*name, val);
                }

                crate::ast::thir::StmtKind::Expr(expr_id) => {
                    last_value = self.eval(*expr_id)?;
                }

                crate::ast::thir::StmtKind::Import { .. } => {
                    // Imports are handled at compile time, skip at runtime
                }

                crate::ast::thir::StmtKind::Type { .. } => {
                    // Type declarations are compile time only, skip at runtime
                }
            }
        }

        Ok(last_value)
    }

    /// Convert a single-value Series to a scalar Value
    fn series_to_scalar(&self, series: &Series) -> Result<Value, RuntimeError> {
        use polars::datatypes::AnyValue;

        let any_value = series
            .get(0)
            .map_err(|e| self.make_error(&format!("Failed to get value from series: {}", e)))?;

        match any_value {
            AnyValue::Int64(i) => Ok(Value::Int(i)),
            AnyValue::String(s) => Ok(Value::String(Arc::from(s))),
            AnyValue::Boolean(b) => Ok(Value::Bool(b)),
            AnyValue::UInt32(u) => Ok(Value::Int(u as i64)),
            AnyValue::UInt64(u) => Ok(Value::Int(u as i64)),
            AnyValue::Int32(i) => Ok(Value::Int(i as i64)),
            _ => Err(self.make_error(&format!("Unsupported series type: {:?}", any_value))),
        }
    }

    /// Helper to create runtime errors
    fn make_error(&self, msg: impl Into<String>) -> RuntimeError {
        use crate::error::{CompileError, CompileErrorKind};
        CompileError::new(CompileErrorKind::Runtime(msg.into()), Loc::generated())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::thir::Expr;

    #[test]
    fn test_eval_literal() {
        let gcx = GlobalContext::new();
        let mut thir = TypedHir::default();

        // Create a simple integer literal
        let expr_id = thir.exprs.alloc(Expr {
            loc: Loc::generated(),
            kind: ExprKind::Literal(crate::ast::ast::Literal::Integer(42)),
            ty: thir.types.alloc(crate::ast::thir::Type {
                loc: Loc::generated(),
                kind: crate::ast::thir::TypeKind::Primitive(crate::ast::ast::PrimitiveType::Int),
            }),
        });

        let env = ThirEnvironment::new();
        let mut eval = ThirEvaluator::new(&thir, &gcx, env);

        let result = eval.eval(expr_id).unwrap();
        assert!(matches!(result, Value::Int(42)));
    }

    #[test]
    fn test_eval_variable() {
        use crate::context::DefKind;

        let mut gcx = GlobalContext::new();
        let thir = TypedHir::default();

        let var_name = gcx.interner.intern("x");

        // Create a DefId for the variable
        let var_def_id = gcx.definitions.insert(None, var_name, DefKind::Let);

        let mut env = ThirEnvironment::new();
        env.bind(var_name, Value::Int(100));

        // Create variable reference using Identifier with DefId
        let mut thir_mut = TypedHir::default();
        let expr_id = thir_mut.exprs.alloc(Expr {
            loc: Loc::generated(),
            kind: ExprKind::Identifier(var_def_id),
            ty: thir_mut.types.alloc(crate::ast::thir::Type {
                loc: Loc::generated(),
                kind: crate::ast::thir::TypeKind::Primitive(crate::ast::ast::PrimitiveType::Int),
            }),
        });

        let mut eval = ThirEvaluator::new(&thir_mut, &gcx, env);
        let result = eval.eval(expr_id).unwrap();
        assert!(matches!(result, Value::Int(100)));
    }
}
