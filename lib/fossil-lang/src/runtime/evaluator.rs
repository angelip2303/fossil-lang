use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, DefKind, Symbol};
use crate::context::global::BuiltInFieldType;
use crate::error::FossilError;
use crate::ir::{Argument, ExprId, ExprKind, Ir, Resolutions, TypeIndex, TypeKind, TypeckResults};
use crate::passes::GlobalContext;
use crate::runtime::output::OutputResolver;
use crate::runtime::value::Value;
use crate::traits::function::RuntimeContext;

pub use crate::runtime::value::Environment as IrEnvironment;

const MAX_CALL_DEPTH: usize = 1000;

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub call_site: Loc,
    pub def_id: Option<DefId>,
}

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

    pub fn depth(&self) -> usize {
        self.frames.len()
    }
}

pub struct IrEvaluator<'a> {
    ir: &'a Ir,
    gcx: &'a GlobalContext,
    type_index: &'a TypeIndex,
    resolutions: &'a Resolutions,
    typeck_results: &'a TypeckResults,
    env: IrEnvironment,
    call_stack: CallStack,
    output_resolver: Arc<dyn OutputResolver>,
    storage: Arc<crate::runtime::storage::StorageConfig>,
}

impl<'a> IrEvaluator<'a> {
    pub fn new(
        ir: &'a Ir,
        gcx: &'a GlobalContext,
        type_index: &'a TypeIndex,
        resolutions: &'a Resolutions,
        typeck_results: &'a TypeckResults,
        env: IrEnvironment,
        output_resolver: Arc<dyn OutputResolver>,
        storage: Arc<crate::runtime::storage::StorageConfig>,
    ) -> Self {
        Self {
            ir,
            gcx,
            type_index,
            resolutions,
            typeck_results,
            env,
            call_stack: Default::default(),
            output_resolver,
            storage,
        }
    }

    pub fn bind(&mut self, name: Symbol, value: Value) {
        self.env.bind(name, value);
    }

    pub fn eval(&mut self, expr_id: ExprId) -> Result<Value, FossilError> {
        let expr_id = self.resolutions.expr_rewrites.get(&expr_id).copied().unwrap_or(expr_id);
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

            ExprKind::Identifier(_) => {
                let &def_id = self.resolutions.expr_defs.get(&expr_id)
                    .ok_or_else(|| self.make_error("Unresolved identifier at runtime"))?;

                let def = self.gcx.definitions.get(def_id);

                if let Some(val) = self.env.lookup(def.name) {
                    return Ok(val.clone());
                }

                match &def.kind {
                    DefKind::Func(func) => Ok(Value::Function(def_id, func.clone())),
                    DefKind::RecordConstructor => Ok(Value::RecordConstructor(def_id)),
                    DefKind::Let => Err(self.make_error(format!(
                        "Variable '{}' not found in environment",
                        self.gcx.interner.resolve(def.name)
                    ))),
                    DefKind::Type | DefKind::Mod | DefKind::Provider(_) => {
                        Err(self.make_error(format!(
                            "'{}' cannot be used as a value",
                            self.gcx.interner.resolve(def.name)
                        )))
                    }
                }
            }

            ExprKind::RecordInstance {
                ctor_args,
                fields,
                ..
            } => self.eval_named_record(expr_id, ctor_args, fields),

            ExprKind::Application { callee, args } => self.eval_application(expr_id, *callee, args),

            ExprKind::Projection {
                source,
                binding,
                outputs,
            } => self.eval_projection(*source, *binding, outputs),

            ExprKind::Join { left, right, left_on, right_on, how, suffix } => {
                self.eval_join(*left, *right, left_on, right_on, *how, suffix.as_ref().copied())
            }

            ExprKind::FieldAccess { expr, field } => self.eval_field_access(*expr, *field),

            ExprKind::Unit => Ok(Value::Unit),

            ExprKind::Reference { ctor_args, .. } => {
                let value = if let Some(first_arg) = ctor_args.first() {
                    self.eval(first_arg.value())?
                } else {
                    return Ok(Value::Expr(lit(NULL)));
                };
                if let Some(ref resolver) = self.gcx.ref_resolver {
                    if let Some(&def_id) = self.resolutions.expr_defs.get(&expr_id) {
                        if let Some(base) = resolver(def_id, self.gcx) {
                            if let Value::Expr(expr) = value {
                                return Ok(Value::Expr(concat_str([lit(base.as_str()), expr], "", true)));
                            }
                        }
                    }
                }
                Ok(value)
            }

            ExprKind::StringInterpolation { parts, exprs } => {
                self.eval_string_interpolation(parts, exprs)
            }
        }
    }

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
                        concat_parts.push(e.cast(polars::prelude::DataType::String));
                    }
                    _ => {
                        concat_parts.push(lit("<value>"));
                    }
                }
            }
        }

        let concat_expr = concat_str(concat_parts, "", true);
        Ok(Value::Expr(concat_expr))
    }

    fn eval_named_record(
        &mut self,
        expr_id: ExprId,
        ctor_args: &[Argument],
        fields: &[(Symbol, ExprId)],
    ) -> Result<Value, FossilError> {
        use crate::runtime::value::{Plan, PendingOutput};

        let type_def_id = self.resolutions.expr_defs.get(&expr_id).copied();

        let ctor_exprs: Vec<Expr> = if !ctor_args.is_empty() {
            let type_def_id = type_def_id.unwrap();
            let info = self.type_index.get(type_def_id);
            let ctor_param_names = info.map(|i| &i.ctor_param_names[..]).unwrap_or(&[]);

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
            return Ok(Value::Plan(Plan::empty(Schema::default())));
        }

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

        let all_exprs: Vec<Expr> = ctor_exprs
            .iter()
            .chain(select_exprs.iter())
            .cloned()
            .collect();
        let schema = build_schema(&all_exprs, type_def_id, self.gcx);

        Ok(Value::PendingOutput(PendingOutput {
            type_def_id: type_def_id.expect("named record must resolve to a type"),
            select_exprs,
            ctor_exprs,
            schema: std::sync::Arc::new(schema),
        }))
    }

    fn eval_projection(
        &mut self,
        source: ExprId,
        binding: Symbol,
        outputs: &[ExprId],
    ) -> Result<Value, FossilError> {
        let source_val = self.eval(source)?;
        let Value::Plan(plan) = source_val else {
            return Err(self.make_error("Projection source must be a Plan"));
        };

        self.env.bind(binding, Value::Plan(plan.clone()));

        let output_specs: Vec<_> = outputs
            .iter()
            .filter_map(|&expr| match self.eval(expr) {
                Ok(Value::PendingOutput(po)) => Some(Ok(po.into_output_spec())),
                Ok(_) => None,
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::Plan(plan.project(output_specs)))
    }

    fn eval_join(
        &mut self,
        left_id: ExprId,
        right_id: ExprId,
        left_on: &[crate::context::Symbol],
        right_on: &[crate::context::Symbol],
        how: crate::common::JoinHow,
        suffix: Option<crate::context::Symbol>,
    ) -> Result<Value, FossilError> {
        let left_val = self.eval(left_id)?;
        let right_val = self.eval(right_id)?;

        let Value::Plan(left_plan) = left_val else {
            return Err(self.make_error("Join left side must be a Plan"));
        };
        let Value::Plan(right_plan) = right_val else {
            return Err(self.make_error("Join right side must be a Plan"));
        };

        let suffix_str = suffix
            .map(|s| self.gcx.interner.resolve(s).to_string())
            .unwrap_or_else(|| "_right".to_string());

        let left_on_exprs: Vec<Expr> = left_on
            .iter()
            .map(|s| col(self.gcx.interner.resolve(*s)))
            .collect();
        let right_on_exprs: Vec<Expr> = right_on
            .iter()
            .map(|s| col(self.gcx.interner.resolve(*s)))
            .collect();

        Ok(Value::Plan(left_plan.join(
            right_plan,
            left_on_exprs,
            right_on_exprs,
            how,
            suffix_str,
        )))
    }

    fn check_recursion_depth(&self) -> Result<(), FossilError> {
        if self.call_stack.depth() >= MAX_CALL_DEPTH {
            Err(self.make_error(format!(
                "Stack overflow: maximum recursion depth ({}) exceeded",
                MAX_CALL_DEPTH
            )))
        } else {
            Ok(())
        }
    }

    fn eval_application(
        &mut self,
        _app_expr_id: ExprId,
        callee: ExprId,
        args: &[Argument],
    ) -> Result<Value, FossilError> {
        self.check_recursion_depth()?;

        let callee_expr = self.ir.exprs.get(callee);
        let callee_loc = callee_expr.loc;
        let callee_val = self.eval(callee)?;

        let arg_values: Vec<Value> = args
            .iter()
            .map(|arg| self.eval(arg.value()))
            .collect::<Result<Vec<_>, _>>()?;

        match callee_val {
            Value::Function(def_id, func) => {
                let def = self.gcx.definitions.get(def_id);
                let function_name = self.gcx.interner.resolve(def.name).to_string();

                self.call_stack.push(StackFrame {
                    function_name,
                    call_site: callee_loc,
                    def_id: Some(def_id),
                });

                let mut ctx = RuntimeContext::new(self.gcx, self.ir, self.type_index)
                    .with_output_resolver(self.output_resolver.clone())
                    .with_storage(self.storage.clone());

                if !args.is_empty() {
                    let first_arg_expr_id = args[0].value();
                    if let Some(&arg_type_id) = self.typeck_results.expr_types.get(&first_arg_expr_id) {
                        let arg_type = self.ir.types.get(arg_type_id);
                        if let TypeKind::Named(def_id) = &arg_type.kind {
                            ctx = ctx.with_type(*def_id);
                        }
                    }
                }

                let result = func.call(arg_values, &ctx);

                self.call_stack.pop();

                result
            }

            Value::RecordConstructor(ctor_def_id) => {
                use crate::runtime::value::PendingOutput;

                let ctor_def = self.gcx.definitions.get(ctor_def_id);
                let type_def_id = ctor_def.parent().unwrap_or(ctor_def_id);

                let info = self.type_index.get(type_def_id).ok_or_else(|| {
                    self.make_error(format!(
                        "Record type '{}' not found",
                        self.gcx.interner.resolve(ctor_def.name)
                    ))
                })?;
                let field_names = &info.field_names;

                if arg_values.len() != field_names.len() {
                    return Err(self.make_error(format!(
                        "Record constructor expects {} arguments, got {}",
                        field_names.len(),
                        arg_values.len()
                    )));
                }

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

                let schema = build_schema(&select_exprs, Some(type_def_id), self.gcx);

                Ok(Value::PendingOutput(PendingOutput {
                    type_def_id,
                    select_exprs,
                    ctor_exprs: vec![],
                    schema: std::sync::Arc::new(schema),
                }))
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, FossilError> {
        let value = self.eval(expr)?;
        let field_name = self.gcx.interner.resolve(field);

        match value {
            Value::Plan(plan) => {
                if !plan.schema.contains(field_name) {
                    return Err(self.make_error(format!(
                        "Field '{}' not found in schema. Available: {:?}",
                        field_name,
                        plan.schema.iter_names().collect::<Vec<_>>()
                    )));
                }

                Ok(Value::Expr(col(field_name)))
            }

            _ => Err(self.make_error("Field access on non-record value")),
        }
    }

    fn make_error(&self, msg: impl Into<String>) -> FossilError {
        FossilError::evaluation(msg.into(), Loc::generated())
    }
}

fn build_schema(
    exprs: &[Expr],
    type_def_id: Option<DefId>,
    gcx: &GlobalContext,
) -> Schema {
    use std::collections::HashMap;

    // Build name→DataType map from registered types if available
    let type_map: HashMap<&str, DataType> = type_def_id
        .and_then(|def_id| gcx.registered_types.get(&def_id))
        .map(|fields| {
            fields.iter().map(|(sym, ft)| {
                let name = gcx.interner.resolve(*sym);
                let prim = match ft {
                    BuiltInFieldType::Required(p) | BuiltInFieldType::Optional(p) => *p,
                };
                (name, prim.to_polars_dtype())
            }).collect()
        })
        .unwrap_or_default();

    let fields: Vec<_> = exprs
        .iter()
        .filter_map(|expr| {
            if let Expr::Alias(_, name) = expr {
                let dtype = type_map
                    .get(name.as_str())
                    .cloned()
                    .unwrap_or(DataType::Unknown(Default::default()));
                Some(Field::new(name.clone(), dtype))
            } else {
                None
            }
        })
        .collect();

    Schema::from_iter(fields)
}
