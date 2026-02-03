use std::rc::Rc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, Symbol};
use crate::error::FossilError;
use crate::ir::{Argument, ExprId, ExprKind, Ident, Ir, StmtId, StmtKind, TypeKind, TypeRef};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

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

    /// Bind a value in the evaluator's environment
    ///
    /// This allows the executor to add top-level bindings without
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
                    DefKind::Let | DefKind::Const => {
                        Err(self.make_error(format!(
                            "Variable '{}' not found in environment",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                    DefKind::Type => {
                        Err(self.make_error(format!(
                            "Type '{}' cannot be used as a value",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                    DefKind::Mod { .. } => {
                        Err(self.make_error(format!(
                            "Module '{}' cannot be used as a value",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                    DefKind::Provider(_) => {
                        Err(self.make_error(format!(
                            "Provider '{}' cannot be used as a value",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                }
            }

            ExprKind::NamedRecordConstruction {
                type_ident,
                fields,
                meta_fields,
            } => self.eval_named_record(type_ident, fields, meta_fields),

            ExprKind::List(items) => self.eval_list(items),

            ExprKind::Function {
                params,
                body,
                attrs,
            } => Ok(Value::Closure {
                params: params.clone(),
                body: *body,
                env: Rc::new(self.env.clone()),
                attrs: attrs.clone(),
            }),

            ExprKind::Application { callee, args } => self.eval_application(*callee, args),

            ExprKind::FieldAccess { expr, field } => self.eval_field_access(*expr, *field),

            ExprKind::Block { stmts } => self.eval_block(stmts),

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

    /// Evaluate a named record construction `TypeName { @id = ..., field = value, ... }`
    ///
    /// All field values are Expr, so we always build a lazy plan.
    /// The type_ident provides the DefId for metadata lookup.
    /// Meta-fields (@name = expr) are evaluated and stored separately.
    fn eval_named_record(
        &mut self,
        type_ident: &Ident,
        fields: &[(Symbol, ExprId)],
        meta_fields: &[(Symbol, ExprId)],
    ) -> Result<Value, FossilError> {
        use crate::runtime::value::{MetaFields, Plan};

        // Get the DefId from the resolved type identifier
        let type_def_id = match type_ident {
            Ident::Resolved(def_id) => Some(*def_id),
            Ident::Unresolved(_) => {
                return Err(self.make_error("Unresolved type identifier in record construction"));
            }
        };

        // Evaluate meta-field expressions
        let evaluated_meta_fields: MetaFields = meta_fields
            .iter()
            .filter_map(|(name, expr_id)| {
                let val = self.eval(*expr_id).ok()?;
                if let Value::Expr(e) = val {
                    Some((*name, e))
                } else {
                    None
                }
            })
            .collect();

        if fields.is_empty() {
            let mut plan = Plan::empty(Schema::default());
            plan.type_def_id = type_def_id;
            plan.meta_fields = evaluated_meta_fields;
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

        // Build schema from expressions
        let schema = build_schema_from_exprs(&select_exprs);

        // Store as pending transformation with type info and meta-fields
        Ok(Value::Plan(Plan {
            source: None,
            transforms: Vec::new(),
            type_def_id,
            schema: std::sync::Arc::new(schema),
            meta_fields: evaluated_meta_fields,
            outputs: Vec::new(),
            pending_exprs: Some(select_exprs),
        }))
    }

    /// Evaluate a list expression
    ///
    /// Lists of primitives evaluate to Polars list expressions.
    /// Lists of records/plans are not supported - use type providers.
    fn eval_list(&mut self, items: &[ExprId]) -> Result<Value, FossilError> {
        use polars::prelude::concat_list;

        if items.is_empty() {
            // Empty list → empty Polars list literal
            return Ok(Value::Expr(lit(polars::prelude::Series::new_empty(
                PlSmallStr::EMPTY,
                &DataType::Null,
            ))));
        }

        let mut exprs = Vec::with_capacity(items.len());

        for item in items {
            match self.eval(*item)? {
                Value::Expr(e) => exprs.push(e),
                Value::Plan(_) => {
                    return Err(self.make_error(
                        "Lists of records are not supported. Use type providers to load data.",
                    ));
                }
                _ => {
                    return Err(self.make_error("Invalid list element"));
                }
            }
        }

        // Create a Polars list expression from the elements
        Ok(Value::Expr(concat_list(exprs).map_err(|e| {
            self.make_error(&format!("Failed to create list expression: {}", e))
        })?))
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
            Value::Closure {
                params, body, env, ..
            } => {
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

    /// Evaluate a block of statements
    fn eval_block(&mut self, stmts: &[StmtId]) -> Result<Value, FossilError> {
        let mut last_value = Value::Unit;

        for stmt_id in stmts {
            let stmt = self.ir.stmts.get(*stmt_id);

            match &stmt.kind {
                StmtKind::Let { name, value, .. } | StmtKind::Const { name, value, .. } => {
                    let val = self.eval(*value)?;
                    self.env.bind(*name, val);
                }

                StmtKind::Expr(expr_id) => {
                    last_value = self.eval(*expr_id)?;
                }

                StmtKind::Type { .. } => {}
            }
        }

        Ok(last_value)
    }

    /// Get the field names for a record type by its name
    fn get_record_field_names(&self, type_name: Symbol) -> Result<Vec<Symbol>, FossilError> {
        // Find the type definition in the IR root statements
        let type_def = self.ir.root.iter().find_map(|stmt_id| {
            let stmt = self.ir.stmts.get(*stmt_id);
            match &stmt.kind {
                StmtKind::Type { name, ty, .. } if *name == type_name => Some(*ty),
                _ => None,
            }
        });

        let Some(ty_id) = type_def else {
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
