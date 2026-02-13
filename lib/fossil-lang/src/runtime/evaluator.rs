use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, Symbol};
use crate::error::FossilError;
use crate::ir::{Argument, ExprId, ExprKind, Ident, Ir, StmtKind, TypeKind, TypeRef};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

pub use crate::runtime::value::Environment as IrEnvironment;

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
    pub fn push(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }

    pub fn pop(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    pub fn depth(&self) -> usize {
        self.frames.len()
    }
}

pub struct IrEvaluator<'a> {
    ir: &'a Ir,
    gcx: &'a GlobalContext,
    env: IrEnvironment,
    call_stack: CallStack,
}

impl<'a> IrEvaluator<'a> {
    pub fn new(ir: &'a Ir, gcx: &'a GlobalContext, env: IrEnvironment) -> Self {
        Self {
            ir,
            gcx,
            env,
            call_stack: Default::default(),
        }
    }

    /// Allows the executor to add top-level bindings without
    /// recreating the evaluator (avoiding environment clones).
    pub fn bind(&mut self, name: Symbol, value: Value) {
        self.env.bind(name, value);
    }

    /// Evaluate an expression and return its value
    pub fn eval(&mut self, expr_id: ExprId) -> Result<Value, FossilError> {
        let expr = self.ir.exprs.get(expr_id);

        match &expr.kind {
            ExprKind::Literal(literal) => {
                use crate::ir::Literal;
                Ok(Value::Expr(match literal {
                    Literal::Integer(i) => lit(*i),
                    Literal::String(s) => {
                        let str_val = self.gcx.interner.resolve(*s);
                        lit(str_val)
                    }
                    Literal::Boolean(b) => lit(*b),
                }))
            }

            ExprKind::Identifier(ident) => {
                // In IR, identifiers can be:
                // 1. Local variables (resolved to their DefId)
                // 2. Functions (registered stdlib functions)
                // 3. Module references
                let def_id = match ident {
                    Ident::Resolved(id) => *id,
                    Ident::Unresolved(_) => {
                        return Err(self.make_error("Unresolved identifier at runtime"));
                    }
                };

                // First check if it's a local variable in the environment
                let def = self.gcx.definitions.get(def_id);

                // Try to look up by symbol in environment first (for local variables)
                if let Some(val) = self.env.lookup(def.name) {
                    return Ok(val.clone());
                }

                use crate::context::DefKind;
                match &def.kind {
                    DefKind::Func(func) => Ok(Value::BuiltinFunction(def_id, func.clone())),
                    DefKind::RecordConstructor => Ok(Value::RecordConstructor(def_id)),
                    DefKind::Let => Err(self.make_error(format!(
                        "Variable '{}' not found in environment",
                        self.gcx.interner.resolve(def.name)
                    ))),
                    DefKind::Type => Err(self.make_error(format!(
                        "Type '{}' cannot be used as a value",
                        self.gcx.interner.resolve(def.name)
                    ))),
                    DefKind::Mod => Err(self.make_error(format!(
                        "Module '{}' cannot be used as a value",
                        self.gcx.interner.resolve(def.name)
                    ))),
                    DefKind::Provider(_) => Err(self.make_error(format!(
                        "Provider '{}' cannot be used as a value",
                        self.gcx.interner.resolve(def.name)
                    ))),
                }
            }

            ExprKind::RecordInstance { type_ident, ctor_args, fields } => {
                self.eval_named_record(type_ident, ctor_args, fields)
            }

            ExprKind::Application { callee, args } => self.eval_application(*callee, args),

            ExprKind::Projection { source, binding, outputs, .. } => {
                self.eval_projection(*source, *binding, outputs)
            }

            ExprKind::FieldAccess { expr, field } => self.eval_field_access(*expr, *field),

            ExprKind::Unit => Ok(Value::Unit),

            ExprKind::StringInterpolation { parts, exprs } => {
                self.eval_string_interpolation(parts, exprs)
            }
        }
    }

    /// Evaluate a string interpolation expression
    ///
    /// All values are Expr, so we always build a concat_str expression.
    /// This keeps everything lazy until execution at the sink.
    fn eval_string_interpolation(
        &mut self,
        parts: &[Symbol],
        exprs: &[ExprId],
    ) -> Result<Value, FossilError> {
        let mut concat_parts: Vec<Expr> = Vec::new();

        for (i, part) in parts.iter().enumerate() {
            let part_str = self.gcx.interner.resolve(*part);
            if !part_str.is_empty() {
                concat_parts.push(lit(part_str));
            }

            if i < exprs.len() {
                let value = self.eval(exprs[i])?;
                match value {
                    Value::Expr(e) => {
                        // Cast to string for concatenation
                        concat_parts.push(e.cast(polars::prelude::DataType::String));
                    }
                    _ => {
                        concat_parts.push(lit("<value>"));
                    }
                }
            }
        }

        // Build concat_str expression
        let concat_expr = concat_str(concat_parts, "", true);
        Ok(Value::Expr(concat_expr))
    }

    /// Evaluate a named record construction `TypeName { field = value, ... }`
    ///
    /// All field values are Expr, so we always build a lazy plan.
    /// The type_ident provides the DefId for metadata lookup.
    fn eval_named_record(
        &mut self,
        type_ident: &Ident,
        ctor_args: &[Argument],
        fields: &[(Symbol, ExprId)],
    ) -> Result<Value, FossilError> {
        use crate::runtime::value::Plan;

        // Get the DefId from the resolved type identifier
        let type_def_id = match type_ident {
            Ident::Resolved(def_id) => Some(*def_id),
            Ident::Unresolved(_) => {
                return Err(self.make_error("Unresolved type identifier in record construction"));
            }
        };

        // Evaluate constructor arguments to Polars expressions
        let ctor_exprs: Vec<Expr> = if !ctor_args.is_empty() {
            let type_def_id = type_def_id.unwrap();
            let type_def = self.gcx.definitions.get(type_def_id);
            let type_name = type_def.name;

            // Look up ctor param names from the type statement
            let ctor_param_names = self.get_type_ctor_param_names(type_name);

            ctor_args
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                    let value = self.eval(arg.value())?;
                    let alias = ctor_param_names
                        .get(i)
                        .map(|name| self.gcx.interner.resolve(*name).to_string())
                        .unwrap_or_else(|| format!("_ctor_{}", i));
                    match value {
                        Value::Expr(expr) => Ok(expr.alias(&*alias)),
                        _ => Ok(lit(NULL).alias(&*alias)),
                    }
                })
                .collect::<Result<Vec<_>, FossilError>>()?
        } else {
            vec![]
        };

        if fields.is_empty() && ctor_args.is_empty() {
            let mut plan = Plan::empty(Schema::default());
            plan.type_def_id = type_def_id;
            return Ok(Value::Plan(plan));
        }

        // Build select expressions from all fields
        let select_exprs: Vec<Expr> = fields
            .iter()
            .map(|(name, expr_id)| {
                let value = self.eval(*expr_id)?;
                let name_str = self.gcx.interner.resolve(*name);
                match value {
                    Value::Expr(expr) => Ok(expr.alias(name_str)),
                    _ => Ok(lit(NULL).alias(name_str)),
                }
            })
            .collect::<Result<Vec<_>, FossilError>>()?;

        // Combine ctor_exprs + select_exprs for schema
        let all_exprs: Vec<Expr> = ctor_exprs.iter().chain(select_exprs.iter()).cloned().collect();
        let schema = build_schema_from_exprs(&all_exprs);

        // Store as pending transformation with type info
        Ok(Value::Plan(Plan {
            source: None,
            transforms: Vec::new(),
            type_def_id,
            schema: std::sync::Arc::new(schema),
            outputs: Vec::new(),
            pending_exprs: Some(select_exprs),
            ctor_exprs,
        }))
    }

    /// Evaluate a projection: source |> fn param -> outputs end
    fn eval_projection(
        &mut self,
        source: ExprId,
        binding: Symbol,
        outputs: &[ExprId],
    ) -> Result<Value, FossilError> {
        use crate::runtime::value::OutputSpec;

        let source_val = self.eval(source)?;
        let Value::Plan(mut plan) = source_val else {
            return Err(self.make_error("Projection source must be a Plan"));
        };

        // Bind param to source plan (for field access in body)
        self.env.bind(binding, Value::Plan(plan.clone()));

        // Evaluate each output
        for &output_expr in outputs {
            let output_val = self.eval(output_expr)?;
            if let Value::Plan(output_plan) = output_val {
                if output_plan.is_pending() {
                    plan.outputs.push(OutputSpec {
                        type_def_id: output_plan.type_def_id.unwrap(),
                        select_exprs: output_plan.pending_exprs.unwrap(),
                        schema: output_plan.schema,
                        ctor_args: output_plan.ctor_exprs,
                    });
                }
            }
        }
        Ok(Value::Plan(plan))
    }

    /// Find a type statement by name, returning its TypeId
    fn find_type_by_name(&self, type_name: Symbol) -> Option<crate::ir::TypeId> {
        for &stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(stmt_id);
            if let StmtKind::Type { name, ty, .. } = &stmt.kind {
                if *name == type_name {
                    return Some(*ty);
                }
            }
        }
        None
    }

    /// Check if we've exceeded the maximum recursion depth
    fn check_recursion_depth(&self) -> Result<(), FossilError> {
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
    ) -> Result<Value, FossilError> {
        // Check recursion depth before proceeding
        self.check_recursion_depth()?;

        let callee_expr = self.ir.exprs.get(callee);
        let callee_loc = callee_expr.loc;
        let callee_val = self.eval(callee)?;

        // Evaluate arguments (extracting values from Argument enum)
        // TODO: Full named parameter support would require looking up param names
        // from callee's signature and reordering. For now, we just use positional order.
        let arg_values: Vec<Value> = args
            .iter()
            .map(|arg| self.eval(arg.value()))
            .collect::<Result<Vec<_>, _>>()?;

        match callee_val {
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
                use crate::runtime::value::Plan;

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

                // All values are Expr - build select expressions for lazy transformation
                let select_exprs: Vec<Expr> = field_names
                    .iter()
                    .zip(arg_values)
                    .map(|(field_name, value)| {
                        let name_str = self.gcx.interner.resolve(*field_name);
                        match value {
                            Value::Expr(expr) => expr.alias(name_str),
                            _ => lit(NULL).alias(name_str),
                        }
                    })
                    .collect();

                // Build schema from expressions
                let schema = build_schema_from_exprs(&select_exprs);

                // Store expressions in the plan - they'll be applied to source by List::map
                Ok(Value::Plan(Plan::pending(
                    select_exprs,
                    type_def_id,
                    schema,
                )))
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    /// Evaluate field access (e.g., `row.field`)
    ///
    /// Returns Value::Expr(col("field")) for lazy column access.
    /// The actual selection happens at sink time (e.g., Rdf::serialize).
    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, FossilError> {
        let value = self.eval(expr)?;
        let field_name = self.gcx.interner.resolve(field);

        match value {
            Value::Plan(plan) => {
                // Validate field exists in schema
                if !plan.schema.contains(field_name) {
                    return Err(self.make_error(format!(
                        "Field '{}' not found in schema. Available: {:?}",
                        field_name,
                        plan.schema.iter_names().collect::<Vec<_>>()
                    )));
                }
                // Return lazy column expression
                Ok(Value::Expr(col(field_name)))
            }

            _ => Err(self.make_error("Field access on non-record value")),
        }
    }

    /// Get the constructor parameter names for a type
    fn get_type_ctor_param_names(&self, type_name: Symbol) -> Vec<Symbol> {
        for &stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(stmt_id);
            if let StmtKind::Type { name, ctor_params, .. } = &stmt.kind {
                if *name == type_name {
                    return ctor_params.iter().map(|p| p.name).collect();
                }
            }
        }
        vec![]
    }

    /// Get the field names for a record type by its name
    fn get_record_field_names(&self, type_name: Symbol) -> Result<Vec<Symbol>, FossilError> {
        let Some(ty_id) = self.find_type_by_name(type_name) else {
            return Err(self.make_error(format!(
                "Record type '{}' not found",
                self.gcx.interner.resolve(type_name)
            )));
        };

        let ty = self.ir.types.get(ty_id);
        let TypeKind::Record(fields) = &ty.kind else {
            return Err(self.make_error(format!(
                "Type '{}' is not a record type",
                self.gcx.interner.resolve(type_name)
            )));
        };

        Ok(fields.field_names())
    }

    /// Helper to create runtime errors
    fn make_error(&self, msg: impl Into<String>) -> FossilError {
        self.make_error_with_stack(msg)
    }

    /// Create a runtime error with the current call stack
    fn make_error_with_stack(&self, msg: impl Into<String>) -> FossilError {
        FossilError::evaluation(msg.into(), Loc::generated())
    }
}

/// Build a schema from select expressions
///
/// Extracts field names from aliased expressions. Types are inferred as Unknown
/// since the actual types come from the source data at runtime.
fn build_schema_from_exprs(exprs: &[Expr]) -> Schema {
    let fields: Vec<_> = exprs
        .iter()
        .filter_map(|expr| {
            // Extract alias name from expression
            if let Expr::Alias(_, name) = expr {
                Some(Field::new(
                    name.clone(),
                    DataType::Unknown(Default::default()),
                ))
            } else {
                None
            }
        })
        .collect();

    Schema::from_iter(fields)
}
