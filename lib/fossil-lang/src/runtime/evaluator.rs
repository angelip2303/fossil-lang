//! THIR Runtime Evaluator
//!
//! This module provides runtime evaluation of THIR (Typed High-level IR) expressions.
//! It enables stdlib functions to execute user-provided lambdas and evaluate field access.

use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::ast::thir::{ExprId, ExprKind, StmtId, TypedHir};
use crate::context::{DefId, Symbol};
use crate::error::RuntimeError;
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

// Re-export Environment for convenience
pub use crate::runtime::value::Environment as ThirEnvironment;

/// Maximum call stack depth to prevent stack overflow
const MAX_CALL_DEPTH: usize = 1000;

/// Stack frame representing a function call
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub call_site: Loc,
    pub def_id: Option<DefId>,
}

/// Call stack for tracking function execution
#[derive(Debug, Clone, Default)]
pub struct CallStack {
    frames: Vec<StackFrame>,
}

impl CallStack {
    /// Create a new empty call stack
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
        }
    }

    /// Push a new stack frame
    pub fn push(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }

    /// Pop the topmost stack frame
    pub fn pop(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    /// Get all frames (most recent last)
    pub fn frames(&self) -> &[StackFrame] {
        &self.frames
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Get current depth
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Format call stack for error messages
    pub fn format(&self, _interner: &crate::context::Interner) -> String {
        if self.is_empty() {
            return String::new();
        }

        let mut output = String::from("Call stack (most recent call last):\n");

        for (i, frame) in self.frames.iter().enumerate() {
            output.push_str(&format!(
                "  #{}: {} at source {} ({}..{})\n",
                i,
                frame.function_name,
                frame.call_site.source,
                frame.call_site.span.start,
                frame.call_site.span.end
            ));
        }

        output
    }
}

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

    /// Call stack for error reporting and recursion tracking
    call_stack: CallStack,
}

impl<'a> ThirEvaluator<'a> {
    /// Create a new evaluator with the given context and environment
    pub fn new(thir: &'a TypedHir, gcx: &'a GlobalContext, env: ThirEnvironment) -> Self {
        Self {
            thir,
            gcx,
            env,
            call_stack: CallStack::new(),
        }
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

            ExprKind::StringInterpolation { parts, exprs } => {
                self.eval_string_interpolation(parts, exprs)
            }

            ExprKind::FieldSelector { field, .. } => {
                // Field selector evaluates to the field name as a string
                let field_str = self.gcx.interner.resolve(*field);
                Ok(Value::FieldSelector(Arc::from(field_str)))
            }
        }
    }

    /// Evaluate a string interpolation expression
    fn eval_string_interpolation(
        &mut self,
        parts: &[Symbol],
        exprs: &[ExprId],
    ) -> Result<Value, RuntimeError> {
        let mut result = String::new();

        // Invariant: parts.len() == exprs.len() + 1
        for (i, part) in parts.iter().enumerate() {
            // Add the literal part
            result.push_str(self.gcx.interner.resolve(*part));

            // If there's a corresponding expression, evaluate and convert to string
            if i < exprs.len() {
                let value = self.eval(exprs[i])?;
                let string_value = self.value_to_string(&value)?;
                result.push_str(&string_value);
            }
        }

        Ok(Value::String(Arc::from(result)))
    }

    /// Convert a Value to its string representation for interpolation
    fn value_to_string(&self, value: &Value) -> Result<String, RuntimeError> {
        match value {
            Value::Int(i) => Ok(i.to_string()),
            Value::String(s) => Ok(s.to_string()),
            Value::Bool(b) => Ok(b.to_string()),
            Value::Unit => Ok("()".to_string()),
            Value::Column(series) => {
                // For a single-element series, return the value
                if series.len() == 1 {
                    use polars::datatypes::AnyValue;
                    match series.get(0).map_err(|e| self.make_error(format!("Series access error: {}", e)))? {
                        AnyValue::Int64(i) => Ok(i.to_string()),
                        AnyValue::String(s) => Ok(s.to_string()),
                        AnyValue::Boolean(b) => Ok(b.to_string()),
                        _ => Ok(format!("{:?}", series)),
                    }
                } else {
                    Ok(format!("{:?}", series))
                }
            }
            Value::Records(lf) => {
                // Collect and format the DataFrame
                match lf.clone().collect() {
                    Ok(df) => Ok(format!("{}", df)),
                    Err(_) => Ok("<records>".to_string()),
                }
            }
            Value::List(items) => {
                let strs: Result<Vec<_>, _> = items.iter().map(|v| self.value_to_string(v)).collect();
                Ok(format!("[{}]", strs?.join(", ")))
            }
            Value::Closure { .. } => Ok("<function>".to_string()),
            Value::Function(_) => Ok("<function>".to_string()),
            Value::BuiltinFunction(_, _) => Ok("<builtin>".to_string()),
            Value::Record(df, idx) => {
                // Format a single row
                Ok(format!("row {} of {}", idx, df.height()))
            }
            Value::Extension { metadata, .. } => Ok(format!("<{}>", metadata.type_name())),
            Value::FieldSelector(field) => Ok(field.to_string()),
        }
    }

    /// Evaluate a record expression
    fn eval_record(&mut self, fields: &[(Symbol, ExprId)]) -> Result<Value, RuntimeError> {
        if fields.is_empty() {
            return Ok(Value::Records(LazyFrame::default()));
        }

        let mut series_vec = Vec::new();

        for (name, expr_id) in fields {
            let value = self.eval(*expr_id)?;
            let name_str = PlSmallStr::from_str(self.gcx.interner.resolve(*name));

            let series = match value {
                Value::Int(i) => Series::new(name_str, &[i]).into_column(),
                Value::String(s) => Series::new(name_str, &[s.as_ref()]).into_column(),
                Value::Bool(b) => Series::new(name_str, &[b]).into_column(),
                Value::Column(mut s) => {
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
            .map_err(|e| self.make_error(format!("Failed to create DataFrame: {}", e)))?;

        Ok(Value::Records(df.lazy()))
    }

    /// Evaluate a list expression
    fn eval_list(&mut self, items: &[ExprId]) -> Result<Value, RuntimeError> {
        if items.is_empty() {
            return Ok(Value::Column(Series::default()));
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
                Ok(Value::Column(Series::from_iter(ints)))
            }

            Value::String(_) => {
                let strings: Vec<&str> = values
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.as_ref(),
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                Ok(Value::Column(Series::from_iter(strings)))
            }

            Value::Bool(_) => {
                let bools: Vec<bool> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Bool(b) => b,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                Ok(Value::Column(Series::from_iter(bools)))
            }

            Value::Records(_) => {
                let dfs: Vec<LazyFrame> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Records(lf) => lf,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                let concatenated = concat(dfs, UnionArgs::default()).map_err(|e| {
                    self.make_error(format!("Failed to concatenate lists: {}", e))
                })?;
                Ok(Value::Records(concatenated))
            }

            Value::Extension { .. } => {
                // For extensions (like Entity), return as List
                // Serialization layer will handle batch optimization
                Ok(Value::List(values))
            }

            _ => Err(self.make_error("Unsupported list element type")),
        }
    }

    /// Check if we've exceeded the maximum recursion depth
    fn check_recursion_depth(&self) -> Result<(), RuntimeError> {
        if self.call_stack.depth() >= MAX_CALL_DEPTH {
            Err(self.make_error_with_stack(format!(
                "Stack overflow: maximum recursion depth ({}) exceeded",
                MAX_CALL_DEPTH
            )))
        } else {
            Ok(())
        }
    }

    /// Evaluate a function application
    fn eval_application(&mut self, callee: ExprId, args: &[crate::ast::thir::Argument]) -> Result<Value, RuntimeError> {
        // Check recursion depth before proceeding
        self.check_recursion_depth()?;

        let callee_expr = self.thir.exprs.get(callee);
        let callee_loc = callee_expr.loc.clone();
        let callee_val = self.eval(callee)?;

        // Evaluate arguments (extracting values from Argument enum)
        // TODO: Full named parameter support would require looking up param names
        // from callee's signature and reordering. For now, we just use positional order.
        let arg_values: Vec<Value> = args
            .iter()
            .map(|arg| self.eval(arg.value()))
            .collect::<Result<Vec<_>, _>>()?;

        match callee_val {
            Value::Closure { params, body, env } => {
                // Get function name for stack trace
                let function_name = if let ExprKind::Identifier(def_id) = callee_expr.kind {
                    let def = self.gcx.definitions.get(def_id);
                    self.gcx.interner.resolve(def.name).to_string()
                } else {
                    "<anonymous>".to_string()
                };

                // Push stack frame
                self.call_stack.push(StackFrame {
                    function_name,
                    call_site: callee_loc,
                    def_id: if let ExprKind::Identifier(def_id) = callee_expr.kind {
                        Some(def_id)
                    } else {
                        None
                    },
                });

                // Execute closure with new environment
                let mut closure_env = (*env).clone();

                // Bind parameters to arguments
                for (param, arg) in params.iter().zip(arg_values.iter()) {
                    closure_env.bind(param.name, arg.clone());
                }

                // Evaluate body with closure environment
                let saved_env = std::mem::replace(&mut self.env, closure_env);
                let result = self.eval(body);
                self.env = saved_env;

                // Pop stack frame
                self.call_stack.pop();

                result
            }

            Value::BuiltinFunction(def_id, func) => {
                // Get function name for stack trace
                let def = self.gcx.definitions.get(def_id);
                let function_name = self.gcx.interner.resolve(def.name).to_string();

                // Push stack frame for builtin
                self.call_stack.push(StackFrame {
                    function_name,
                    call_site: callee_loc,
                    def_id: Some(def_id),
                });

                // Call builtin function with type context
                let mut ctx = RuntimeContext::new(self.gcx, self.thir);

                // Try to extract DefId from first argument's type (for functions like Entity::with_id)
                // This handles cases with explicit type annotations
                if !args.is_empty() {
                    let first_arg_expr = self.thir.exprs.get(args[0].value());
                    let arg_type = self.thir.types.get(first_arg_expr.ty);

                    // If it's a Named type, extract the DefId
                    if let crate::ast::thir::TypeKind::Named(def_id) = &arg_type.kind {
                        ctx = ctx.with_type(*def_id);
                    }
                }

                let result = func.call(arg_values, &ctx);

                // Pop stack frame
                self.call_stack.pop();

                result
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    /// Evaluate field access (e.g., `row.id`)
    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, RuntimeError> {
        let value = self.eval(expr)?;

        match value {
            // Optimized path: direct row access without collection
            Value::Record(df, row_idx) => {
                let field_name = self.gcx.interner.resolve(field);

                let column = df.column(field_name).map_err(|e| {
                    self.make_error(format!("Field '{}' not found: {}", field_name, e))
                })?;

                let series = column.as_materialized_series();
                self.series_value_at(series, row_idx)
            }

            Value::Records(lf) => {
                // Extract field from LazyFrame
                let field_name = self.gcx.interner.resolve(field);

                // Optimization: Use select() to only collect the column we need
                // This avoids materializing all columns when we only need one
                let lf_selected = lf.select([col(field_name)]).with_projection_pushdown(true);

                // Collect only the selected column
                let df = lf_selected
                    .collect()
                    .map_err(|e| self.make_error(format!("Failed to access field: {}", e)))?;

                // Get the column (should be the only one)
                let column = df.column(field_name).map_err(|e| {
                    self.make_error(format!("Field '{}' not found: {}", field_name, e))
                })?;

                // Convert Column to Series
                let series = column.as_materialized_series().clone();

                // If it's a single row, return scalar value
                if df.height() == 1 {
                    self.series_to_scalar(&series)
                } else {
                    // Multiple rows, return Series
                    Ok(Value::Column(series))
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
        self.series_value_at(series, 0)
    }

    /// Get a scalar Value from a Series at a specific index
    fn series_value_at(&self, series: &Series, idx: usize) -> Result<Value, RuntimeError> {
        use polars::datatypes::AnyValue;

        let any_value = series
            .get(idx)
            .map_err(|e| self.make_error(format!("Failed to get value from series at index {}: {}", idx, e)))?;

        match any_value {
            AnyValue::Int64(i) => Ok(Value::Int(i)),
            AnyValue::String(s) => Ok(Value::String(Arc::from(s))),
            AnyValue::Boolean(b) => Ok(Value::Bool(b)),
            AnyValue::UInt32(u) => Ok(Value::Int(u as i64)),
            AnyValue::UInt64(u) => Ok(Value::Int(u as i64)),
            AnyValue::Int32(i) => Ok(Value::Int(i as i64)),
            _ => Err(self.make_error(format!("Unsupported series type: {:?}", any_value))),
        }
    }

    /// Helper to create runtime errors
    fn make_error(&self, msg: impl Into<String>) -> RuntimeError {
        self.make_error_with_stack(msg)
    }

    /// Create a runtime error with the current call stack
    fn make_error_with_stack(&self, msg: impl Into<String>) -> RuntimeError {
        use crate::error::{CompileError, CompileErrorKind};

        let mut error = CompileError::new(
            CompileErrorKind::Runtime(msg.into()),
            Loc::generated(),
        );

        // Add call stack if not empty
        if !self.call_stack.is_empty() {
            error = error.with_context(self.call_stack.format(&self.gcx.interner));
        }

        error
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

    #[test]
    fn test_call_stack_operations() {
        let mut stack = CallStack::new();
        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);

        stack.push(StackFrame {
            function_name: "foo".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        assert!(!stack.is_empty());
        assert_eq!(stack.depth(), 1);

        stack.push(StackFrame {
            function_name: "bar".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        assert_eq!(stack.depth(), 2);

        let frame = stack.pop();
        assert!(frame.is_some());
        assert_eq!(frame.unwrap().function_name, "bar");
        assert_eq!(stack.depth(), 1);
    }

    #[test]
    fn test_call_stack_format() {
        let mut stack = CallStack::new();
        let gcx = GlobalContext::new();

        stack.push(StackFrame {
            function_name: "main".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        stack.push(StackFrame {
            function_name: "helper".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        let formatted = stack.format(&gcx.interner);
        assert!(formatted.contains("Call stack"));
        assert!(formatted.contains("main"));
        assert!(formatted.contains("helper"));
    }

    #[test]
    fn test_recursion_depth_check() {
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let env = ThirEnvironment::new();
        let mut eval = ThirEvaluator::new(&thir, &gcx, env);

        // Manually fill stack to near limit
        for i in 0..MAX_CALL_DEPTH {
            eval.call_stack.push(StackFrame {
                function_name: format!("func_{}", i),
                call_site: Loc::generated(),
                def_id: None,
            });
        }

        // Should fail recursion check
        let result = eval.check_recursion_depth();
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(error.message().contains("Stack overflow"));
        assert!(error.message().contains("maximum recursion depth"));
    }

    #[test]
    fn test_make_error_with_stack() {
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let env = ThirEnvironment::new();
        let mut eval = ThirEvaluator::new(&thir, &gcx, env);

        // Add some stack frames
        eval.call_stack.push(StackFrame {
            function_name: "outer".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        eval.call_stack.push(StackFrame {
            function_name: "inner".to_string(),
            call_site: Loc::generated(),
            def_id: None,
        });

        let error = eval.make_error("Test error");
        assert!(error.context.is_some());

        let context = error.context.as_ref().unwrap();
        assert!(context.contains("Call stack"));
        assert!(context.contains("outer"));
        assert!(context.contains("inner"));
    }
}
