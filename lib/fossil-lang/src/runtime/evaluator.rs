//! IR Runtime Evaluator
//!
//! This module provides runtime evaluation of IR (Intermediate Representation) expressions.
//! It enables stdlib functions to execute user-provided lambdas and evaluate field access.

use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, Symbol};
use crate::error::RuntimeError;
use crate::ir::{Argument, ExprId, ExprKind, Ident, Ir, StmtId, StmtKind, TypeKind, TypeRef};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

// Re-export Environment for convenience
pub use crate::runtime::value::Environment as IrEnvironment;

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
        Self { frames: Vec::new() }
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

/// IR Expression Evaluator
///
/// Evaluates typed IR expressions at runtime. This is used by stdlib functions
/// to execute user-provided lambdas and evaluate field access on runtime values.
pub struct IrEvaluator<'a> {
    /// Reference to the IR being evaluated
    ir: &'a Ir,

    /// Reference to global context for function lookup
    gcx: &'a GlobalContext,

    /// Current environment for variable bindings
    env: IrEnvironment,

    /// Call stack for error reporting and recursion tracking
    call_stack: CallStack,
}

impl<'a> IrEvaluator<'a> {
    /// Create a new evaluator with the given context and environment
    pub fn new(ir: &'a Ir, gcx: &'a GlobalContext, env: IrEnvironment) -> Self {
        Self {
            ir,
            gcx,
            env,
            call_stack: CallStack::new(),
        }
    }

    /// Evaluate an expression and return its value
    pub fn eval(&mut self, expr_id: ExprId) -> Result<Value, RuntimeError> {
        let expr = self.ir.exprs.get(expr_id);

        match &expr.kind {
            ExprKind::Literal(lit) => {
                use crate::ir::Literal;
                match lit {
                    Literal::Integer(i) => Ok(Value::Int(*i)),
                    Literal::String(s) => {
                        let str_val = self.gcx.interner.resolve(*s);
                        Ok(Value::String(Arc::from(str_val)))
                    }
                    Literal::Boolean(b) => Ok(Value::Bool(*b)),
                }
            }

            ExprKind::Identifier(ident) => {
                // In IR, identifiers can be:
                // 1. Local variables (resolved to their DefId)
                // 2. Functions (registered stdlib functions)
                // 3. Module references
                let def_id = match ident {
                    Ident::Resolved(id) => *id,
                    Ident::Unresolved(_) => {
                        return Err(self.make_error("Unresolved identifier at runtime"))
                    }
                };

                // First check if it's a local variable in the environment
                let def = self.gcx.definitions.get(def_id);

                // Try to look up by symbol in environment first (for local variables)
                if let Some(val) = self.env.lookup(def.name) {
                    return Ok(val.clone());
                }

                // Otherwise, it's a function or module binding
                use crate::context::DefKind;
                match &def.kind {
                    DefKind::Func(Some(func)) => Ok(Value::BuiltinFunction(def_id, func.clone())),
                    DefKind::Func(None) => {
                        // Record constructor - return a special value that represents the constructor
                        Ok(Value::RecordConstructor(def_id))
                    }
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

            ExprKind::Pipe { .. } => {
                // Pipe should have been desugared during resolution
                Err(self.make_error("Pipe expression should be desugared before evaluation"))
            }

            ExprKind::Placeholder => {
                // Placeholder should never reach evaluation
                Err(self.make_error("Placeholder expression should not be evaluated directly"))
            }
        }
    }

    /// Evaluate a string interpolation expression
    ///
    /// When interpolating Expr values (from RowContext tracing), preserves the
    /// column reference as a template `${column_name}` for lazy processing.
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
                let expr_id = exprs[i];
                let value = self.eval(expr_id)?;

                // Special handling for Expr values: preserve as template
                // This allows Entity::with_id to extract column names for lazy processing
                if let Value::Expr(ref expr) = value {
                    // Extract column name from col("name") expression
                    if let Some(col_name) = extract_column_name(expr) {
                        result.push_str("${");
                        result.push_str(&col_name);
                        result.push('}');
                        continue;
                    }
                }

                // Dispatch through the registered string conversion trait (if any)
                let string_value = if let Some(trait_def_id) = self.gcx.builtin_traits.to_string {
                    self.dispatch_trait_to_string(&value, expr_id, trait_def_id)?
                } else {
                    self.value_to_string(&value)?
                };
                result.push_str(&string_value);
            }
        }

        Ok(Value::String(Arc::from(result)))
    }

    /// Dispatch a trait method call for string conversion.
    /// Uses the general trait dispatch mechanism: looks up the trait impl for the
    /// expression's type and calls the first method. Falls back to built-in conversion
    /// for primitives and types without a user-defined impl.
    fn dispatch_trait_to_string(
        &mut self,
        value: &Value,
        expr_id: ExprId,
        trait_def_id: DefId,
    ) -> Result<String, RuntimeError> {
        if let Some(result) = self.dispatch_trait_method(value, expr_id, trait_def_id)? {
            self.value_to_string(&result)
        } else {
            self.value_to_string(value)
        }
    }

    /// General trait method dispatch: given a value, its expression (for type info),
    /// and a trait, finds and calls the user-defined impl method.
    /// Returns None if no user-defined impl exists (primitives, unresolved types).
    /// This is the core extensibility mechanism — any trait registered in GCX can
    /// be dispatched at runtime without modifying the evaluator.
    pub fn dispatch_trait_method(
        &mut self,
        value: &Value,
        expr_id: ExprId,
        trait_def_id: DefId,
    ) -> Result<Option<Value>, RuntimeError> {
        // Get the expression's type DefId from IR
        let type_def_id = match self.get_expr_type_def_id(expr_id) {
            Some(id) => id,
            None => return Ok(None),
        };

        // Find the impl method expression in IR
        let method_expr_id = match self.find_trait_impl_method(trait_def_id, type_def_id) {
            Some(id) => id,
            None => return Ok(None),
        };

        // Evaluate the method expression to get its closure
        let value_clone = value.clone();
        let method_val = self.eval(method_expr_id)?;

        // Call the closure with self = value
        match method_val {
            Value::Closure { params, body, env } => {
                let mut closure_env = (*env).clone();
                // Bind self parameter to the value
                if let Some(param) = params.first() {
                    closure_env.bind(param.name, value_clone);
                }
                let saved_env = std::mem::replace(&mut self.env, closure_env);
                let result = self.eval(body);
                self.env = saved_env;
                Ok(Some(result?))
            }
            _ => Ok(None),
        }
    }

    /// Extract the type DefId from an expression's IR type.
    fn get_expr_type_def_id(&self, expr_id: ExprId) -> Option<DefId> {
        let expr = self.ir.exprs.get(expr_id);
        let ty_id = match &expr.ty {
            TypeRef::Known(id) => *id,
            TypeRef::Unknown => return None,
        };
        let ty = self.ir.types.get(ty_id);
        match &ty.kind {
            TypeKind::Named(ident) => match ident {
                Ident::Resolved(def_id) => Some(*def_id),
                _ => None,
            },
            TypeKind::App { ctor, .. } => match ctor {
                Ident::Resolved(def_id) => Some(*def_id),
                _ => None,
            },
            _ => None,
        }
    }

    /// Find the first method ExprId for a trait impl by searching IR Impl statements.
    /// This is a general lookup: it finds any impl for the given (trait, type) pair.
    fn find_trait_impl_method(&self, trait_def_id: DefId, type_def_id: DefId) -> Option<ExprId> {
        for stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(*stmt_id);
            if let StmtKind::Impl {
                trait_name,
                type_name,
                methods,
            } = &stmt.kind
            {
                // Check if both trait and type are resolved to the expected DefIds
                let trait_matches = matches!(trait_name, Ident::Resolved(id) if *id == trait_def_id);
                let type_matches = matches!(type_name, Ident::Resolved(id) if *id == type_def_id);

                if trait_matches && type_matches {
                    // Return the first method's expression
                    if let Some((_name, expr_id)) = methods.first() {
                        return Some(*expr_id);
                    }
                }
            }
        }
        None
    }

    /// Convert a Value to its string representation for interpolation
    fn value_to_string(&self, value: &Value) -> Result<String, RuntimeError> {
        match value {
            Value::Int(i) => Ok(i.to_string()),
            Value::String(s) => Ok(s.to_string()),
            Value::Bool(b) => Ok(b.to_string()),
            Value::Unit => Ok("()".to_string()),
            Value::Expr(expr) => Ok(format!("{:?}", expr)),
            Value::Records(plan) => {
                // Collect and format the DataFrame
                match plan.lf.clone().collect() {
                    Ok(df) => Ok(format!("{}", df)),
                    Err(_) => Ok("<records>".to_string()),
                }
            }
            Value::RowContext { .. } => Ok("<row-context>".to_string()),
            Value::Closure { .. } => Ok("<function>".to_string()),
            Value::Function(_) => Ok("<function>".to_string()),
            Value::BuiltinFunction(_, _) => Ok("<builtin>".to_string()),
            Value::FieldSelector(field) => Ok(field.to_string()),
            Value::RecordConstructor(_) => Ok("<record-constructor>".to_string()),
        }
    }

    /// Evaluate a record expression
    ///
    /// When fields are Expr values, returns a record of expressions (lazy).
    /// When fields are primitives, creates a single-row DataFrame.
    fn eval_record(&mut self, fields: &[(Symbol, ExprId)]) -> Result<Value, RuntimeError> {
        use crate::runtime::value::RecordsPlan;

        if fields.is_empty() {
            return Ok(Value::Records(RecordsPlan::new(LazyFrame::default())));
        }

        // Evaluate all fields first
        let mut field_values: Vec<(Symbol, Value)> = Vec::with_capacity(fields.len());
        for (name, expr_id) in fields {
            let value = self.eval(*expr_id)?;
            field_values.push((*name, value));
        }

        // Check if any field is an expression (lazy path)
        let has_exprs = field_values.iter().any(|(_, v)| matches!(v, Value::Expr(_)));

        if has_exprs {
            // LAZY PATH: Build select expressions
            // This creates a record of named expressions for lazy transformation
            let select_exprs: Vec<Expr> = field_values
                .into_iter()
                .map(|(name, value)| {
                    let name_str = self.gcx.interner.resolve(name);
                    match value {
                        Value::Expr(expr) => expr.alias(name_str),
                        Value::Int(i) => lit(i).alias(name_str),
                        Value::String(s) => lit(s.as_ref()).alias(name_str),
                        Value::Bool(b) => lit(b).alias(name_str),
                        _ => lit(NULL).alias(name_str), // Fallback for unsupported
                    }
                })
                .collect();

            // Store the expressions as a lazy plan that will be applied later
            // We need a base LazyFrame - use an empty one with the expressions
            // The actual data comes from the source in List::map
            let lf = LazyFrame::default().select(select_exprs);
            Ok(Value::Records(RecordsPlan::new(lf)))
        } else {
            // CONCRETE PATH: All primitives - create single-row DataFrame
            let mut series_vec = Vec::new();

            for (name, value) in field_values {
                let name_str = PlSmallStr::from_str(self.gcx.interner.resolve(name));

                let series = match value {
                    Value::Int(i) => Series::new(name_str, &[i]).into_column(),
                    Value::String(s) => Series::new(name_str, &[s.as_ref()]).into_column(),
                    Value::Bool(b) => Series::new(name_str, &[b]).into_column(),
                    _ => {
                        return Err(self.make_error("Unsupported value type in record field"));
                    }
                };

                series_vec.push(series);
            }

            let df = DataFrame::new(series_vec)
                .map_err(|e| self.make_error(format!("Failed to create DataFrame: {}", e)))?;

            Ok(Value::Records(RecordsPlan::new(df.lazy())))
        }
    }

    /// Evaluate a list expression
    ///
    /// Lists of primitives become a single-column DataFrame.
    /// Lists of records get concatenated lazily.
    fn eval_list(&mut self, items: &[ExprId]) -> Result<Value, RuntimeError> {
        use crate::runtime::value::RecordsPlan;

        if items.is_empty() {
            return Ok(Value::Records(RecordsPlan::new(LazyFrame::default())));
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
                let series = Series::from_iter(ints);
                let df = DataFrame::new(vec![series.into_column()])
                    .map_err(|e| self.make_error(format!("Failed to create list: {}", e)))?;
                Ok(Value::Records(RecordsPlan::new(df.lazy())))
            }

            Value::String(_) => {
                let strings: Vec<&str> = values
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.as_ref(),
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                let series = Series::from_iter(strings);
                let df = DataFrame::new(vec![series.into_column()])
                    .map_err(|e| self.make_error(format!("Failed to create list: {}", e)))?;
                Ok(Value::Records(RecordsPlan::new(df.lazy())))
            }

            Value::Bool(_) => {
                let bools: Vec<bool> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Bool(b) => b,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();
                let series = Series::from_iter(bools);
                let df = DataFrame::new(vec![series.into_column()])
                    .map_err(|e| self.make_error(format!("Failed to create list: {}", e)))?;
                Ok(Value::Records(RecordsPlan::new(df.lazy())))
            }

            Value::Records(_) => {
                let plans: Vec<_> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::Records(plan) => plan,
                        _ => unreachable!("Type checker ensures homogeneous lists"),
                    })
                    .collect();

                // Concatenate all LazyFrames, preserving metadata from first plan
                let lfs: Vec<LazyFrame> = plans.iter().map(|p| p.lf.clone()).collect();
                let concatenated = concat(lfs, UnionArgs::default())
                    .map_err(|e| self.make_error(format!("Failed to concatenate lists: {}", e)))?;

                // Preserve metadata from first plan
                let mut result_plan = RecordsPlan::new(concatenated);
                if let Some(first) = plans.first() {
                    result_plan.type_def_id = first.type_def_id;
                    result_plan.subject_pattern = first.subject_pattern.clone();
                }

                Ok(Value::Records(result_plan))
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
    fn eval_application(
        &mut self,
        callee: ExprId,
        args: &[Argument],
    ) -> Result<Value, RuntimeError> {
        // Check recursion depth before proceeding
        self.check_recursion_depth()?;

        let callee_expr = self.ir.exprs.get(callee);
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
                let function_name =
                    if let ExprKind::Identifier(Ident::Resolved(def_id)) = &callee_expr.kind {
                        let def = self.gcx.definitions.get(*def_id);
                        self.gcx.interner.resolve(def.name).to_string()
                    } else {
                        "<anonymous>".to_string()
                    };

                // Push stack frame
                self.call_stack.push(StackFrame {
                    function_name,
                    call_site: callee_loc,
                    def_id: if let ExprKind::Identifier(Ident::Resolved(def_id)) = &callee_expr.kind
                    {
                        Some(*def_id)
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
                let mut ctx = RuntimeContext::new(self.gcx, self.ir);

                // Try to extract DefId from first argument's type (for functions like Entity::with_id)
                // This handles cases with explicit type annotations
                if !args.is_empty() {
                    let first_arg_expr = self.ir.exprs.get(args[0].value());
                    if let TypeRef::Known(arg_type_id) = first_arg_expr.ty {
                        let arg_type = self.ir.types.get(arg_type_id);

                        // If it's a Named type, extract the DefId
                        if let TypeKind::Named(Ident::Resolved(def_id)) = &arg_type.kind {
                            ctx = ctx.with_type(*def_id);
                        }
                    }
                }

                let result = func.call(arg_values, &ctx);

                // Pop stack frame
                self.call_stack.pop();

                result
            }

            Value::RecordConstructor(ctor_def_id) => {
                use crate::runtime::value::RecordsPlan;

                // Get the constructor's definition to find the parent (type) DefId
                let ctor_def = self.gcx.definitions.get(ctor_def_id);
                let type_name = ctor_def.name;
                // The parent DefId is the type's DefId (where metadata is stored)
                let type_def_id = ctor_def.parent().unwrap_or(ctor_def_id);

                // Find the type definition with this name in the IR
                let field_names = self.get_record_field_names(type_name)?;

                // Build the record by matching positional args with field names
                if arg_values.len() != field_names.len() {
                    return Err(self.make_error(format!(
                        "Record constructor expects {} arguments, got {}",
                        field_names.len(),
                        arg_values.len()
                    )));
                }

                // Check if any argument is an expression (lazy path)
                let has_exprs = arg_values.iter().any(|v| matches!(v, Value::Expr(_)));

                if has_exprs {
                    // LAZY PATH: Build select expressions for transformation
                    let select_exprs: Vec<Expr> = field_names
                        .iter()
                        .zip(arg_values.into_iter())
                        .map(|(field_name, value)| {
                            let name_str = self.gcx.interner.resolve(*field_name);
                            match value {
                                Value::Expr(expr) => expr.alias(name_str),
                                Value::Int(i) => lit(i).alias(name_str),
                                Value::String(s) => lit(s.as_ref()).alias(name_str),
                                Value::Bool(b) => lit(b).alias(name_str),
                                _ => lit(NULL).alias(name_str),
                            }
                        })
                        .collect();

                    // Store expressions in the plan - they'll be applied to source by List::map
                    Ok(Value::Records(RecordsPlan::from_exprs(select_exprs, type_def_id)))
                } else {
                    // CONCRETE PATH: All primitives - create single-row DataFrame
                    let mut series_vec = Vec::new();
                    for (field_name, value) in field_names.iter().zip(arg_values.into_iter()) {
                        let name_str = PlSmallStr::from_str(self.gcx.interner.resolve(*field_name));

                        let series = match value {
                            Value::Int(i) => Series::new(name_str, &[i]).into_column(),
                            Value::String(s) => Series::new(name_str, &[s.as_ref()]).into_column(),
                            Value::Bool(b) => Series::new(name_str, &[b]).into_column(),
                            _ => {
                                return Err(
                                    self.make_error("Unsupported value type in record constructor")
                                );
                            }
                        };

                        series_vec.push(series);
                    }

                    let df = DataFrame::new(series_vec)
                        .map_err(|e| self.make_error(format!("Failed to create record: {}", e)))?;

                    // Create plan with type information (use type's DefId for metadata lookup)
                    Ok(Value::Records(RecordsPlan::with_type(df.lazy(), type_def_id)))
                }
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    /// Evaluate field access (e.g., `row.id`)
    ///
    /// On RowContext: returns Value::Expr(col("field")) for lazy column access
    /// On Records: returns Value::Expr(col("field")) - field access is lazy
    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, RuntimeError> {
        let value = self.eval(expr)?;
        let field_name = self.gcx.interner.resolve(field);

        match value {
            // LAZY: RowContext field access returns an expression
            Value::RowContext { schema } => {
                // Validate field exists in schema
                if !schema.contains(field_name) {
                    return Err(self.make_error(format!(
                        "Field '{}' not found in schema. Available: {:?}",
                        field_name,
                        schema.iter_names().collect::<Vec<_>>()
                    )));
                }
                // Return lazy column expression
                Ok(Value::Expr(col(field_name)))
            }

            // LAZY: Records field access returns an expression
            // The actual selection happens at sink time (serialize)
            Value::Records(_plan) => {
                // Return lazy column expression
                Ok(Value::Expr(col(field_name)))
            }

            _ => Err(self.make_error("Field access on non-record value")),
        }
    }

    /// Evaluate a block of statements
    fn eval_block(&mut self, stmts: &[StmtId]) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Unit;

        for stmt_id in stmts {
            let stmt = self.ir.stmts.get(*stmt_id);

            match &stmt.kind {
                StmtKind::Let { name, value, .. } => {
                    let val = self.eval(*value)?;
                    self.env.bind(*name, val);
                }

                StmtKind::Const { name, value, .. } => {
                    let val = self.eval(*value)?;
                    self.env.bind(*name, val);
                }

                StmtKind::Expr(expr_id) => {
                    last_value = self.eval(*expr_id)?;
                }

                StmtKind::Type { .. } => {
                    // Type declarations are compile time only, skip at runtime
                }

                StmtKind::Trait { .. } => {
                    // Trait declarations are compile time only, skip at runtime
                }

                StmtKind::Impl { .. } => {
                    // Impl declarations are compile time only, skip at runtime
                }
            }
        }

        Ok(last_value)
    }

    /// Get the field names for a record type by its name
    fn get_record_field_names(&self, type_name: Symbol) -> Result<Vec<Symbol>, RuntimeError> {
        use crate::ir::RecordRow;

        // Find the type definition in the IR root statements
        for stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(*stmt_id);
            if let StmtKind::Type { name, ty } = &stmt.kind {
                if *name == type_name {
                    // Found the type, extract field names from the RecordRow
                    let ty = self.ir.types.get(*ty);
                    if let TypeKind::Record(row) = &ty.kind {
                        let mut fields = Vec::new();
                        let mut current_row = row.clone();
                        loop {
                            match current_row {
                                RecordRow::Empty => break,
                                RecordRow::Var(_) => break,
                                RecordRow::Extend { field, rest, .. } => {
                                    fields.push(field);
                                    current_row = *rest;
                                }
                            }
                        }
                        return Ok(fields);
                    } else {
                        return Err(self.make_error(format!(
                            "Type '{}' is not a record type",
                            self.gcx.interner.resolve(type_name)
                        )));
                    }
                }
            }
        }

        Err(self.make_error(format!(
            "Record type '{}' not found",
            self.gcx.interner.resolve(type_name)
        )))
    }

    /// Helper to create runtime errors
    fn make_error(&self, msg: impl Into<String>) -> RuntimeError {
        self.make_error_with_stack(msg)
    }

    /// Create a runtime error with the current call stack
    fn make_error_with_stack(&self, msg: impl Into<String>) -> RuntimeError {
        use crate::error::{CompileError, CompileErrorKind};

        let mut error = CompileError::new(CompileErrorKind::Runtime(msg.into()), Loc::generated());

        // Add call stack if not empty
        if !self.call_stack.is_empty() {
            error = error.with_context(self.call_stack.format(&self.gcx.interner));
        }

        error
    }
}

/// Extract column name from a Polars Expr if it's a simple column reference
///
/// Returns Some("column_name") for col("column_name") expressions,
/// None for complex expressions that can't be represented as a template.
fn extract_column_name(expr: &polars::prelude::Expr) -> Option<String> {
    use polars::prelude::Expr;

    match expr {
        Expr::Column(name) => Some(name.to_string()),
        Expr::Alias(inner, _) => extract_column_name(inner),
        _ => None,
    }
}
