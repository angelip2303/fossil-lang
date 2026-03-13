use std::sync::{Arc, Mutex};

use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, DefKind, Symbol};
use crate::error::FossilError;
use crate::ir::{Argument, ExprId, ExprKind, Ir, Resolutions, TypeIndex, TypeKind, TypeckResults};
use crate::passes::GlobalContext;
use crate::runtime::executor::OutputRecord;
use crate::runtime::value::{Emission, EmissionDef, Value};
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
    storage: Arc<std::collections::HashMap<String, String>>,
    outputs: Arc<Mutex<Vec<OutputRecord>>>,
}

impl<'a> IrEvaluator<'a> {
    pub fn new(
        ir: &'a Ir,
        gcx: &'a GlobalContext,
        type_index: &'a TypeIndex,
        resolutions: &'a Resolutions,
        typeck_results: &'a TypeckResults,
        env: IrEnvironment,
        storage: Arc<std::collections::HashMap<String, String>>,
        outputs: Arc<Mutex<Vec<OutputRecord>>>,
    ) -> Self {
        Self {
            ir,
            gcx,
            type_index,
            resolutions,
            typeck_results,
            env,
            call_stack: Default::default(),
            storage,
            outputs,
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
                spread,
                fields,
                ..
            } => self.eval_named_record(expr_id, ctor_args, spread.as_ref().copied(), fields),

            ExprKind::Application { callee, args } => self.eval_application(expr_id, *callee, args),

            ExprKind::Projection {
                source,
                binding,
                outputs,
            } => self.eval_projection(*source, *binding, outputs),

            ExprKind::Join { left, right, left_on, right_on, suffix } => {
                self.eval_join(*left, *right, left_on, right_on, suffix.as_ref().copied())
            }

            ExprKind::FieldAccess { expr, field } => self.eval_field_access(*expr, *field),

            ExprKind::Unit => Ok(Value::Unit),

            ExprKind::StringInterpolation { parts, exprs } => {
                self.eval_string_interpolation(parts, exprs)
            }

            ExprKind::Coalesce { value, default } => {
                let value_val = self.eval(*value)?;
                let default_val = self.eval(*default)?;
                match (value_val, default_val) {
                    (Value::Expr(v), Value::Expr(d)) => {
                        Ok(Value::Expr(
                            when(v.clone().is_not_null())
                                .then(v)
                                .otherwise(d),
                        ))
                    }
                    (v, _) => Ok(v),
                }
            }

            ExprKind::Ref { args, .. } => {
                let type_def_id = self.resolutions.expr_defs.get(&expr_id).copied()
                    .ok_or_else(|| self.make_error("Unresolved type in ref expression"))?;
                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|&arg| self.eval(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::Reference { def_id: type_def_id, args: arg_values })
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
        spread: Option<ExprId>,
        fields: &[(Symbol, ExprId)],
    ) -> Result<Value, FossilError> {
        use std::collections::HashSet;

        let type_def_id = self.resolutions.expr_defs.get(&expr_id).copied();

        if fields.is_empty() && ctor_args.is_empty() && spread.is_none() {
            return Ok(Value::Frame(LazyFrame::default()));
        }

        // Reference-like usage: ctor_args present but no fields/spread.
        if fields.is_empty() && spread.is_none() && !ctor_args.is_empty() {
            let ctor_values: Vec<Value> = ctor_args
                .iter()
                .map(|arg| self.eval(arg.value()))
                .collect::<Result<Vec<_>, _>>()?;

            if let Some(type_def_id) = type_def_id {
                return Ok(Value::Reference { def_id: type_def_id, args: ctor_values });
            }
            if let Some(first) = ctor_values.into_iter().next() {
                return Ok(first);
            }
        }

        let resolved_type_def_id = type_def_id.ok_or_else(|| self.make_error("Named record must resolve to a type definition"))?;

        // Evaluate fields: each produces an Expr + optionally nested EmissionDefs
        let mut select_exprs: Vec<Expr> = Vec::with_capacity(fields.len());
        let mut child_specs: Vec<EmissionDef> = Vec::new();

        for &(name, expr_id) in fields {
            let value = self.eval(expr_id)?;
            let name_str = self.gcx.interner.resolve(name);

            let field_expr = match value {
                Value::Expr(expr) => expr,
                Value::Reference { args, .. } => {
                    let exprs: Vec<Expr> = args.into_iter()
                        .map(|v| match v { Value::Expr(e) => e, _ => lit(NULL) })
                        .collect();
                    identity_from_exprs(&exprs)
                }
                Value::Emission(nested) => {
                    let identity = nested.specs.first()
                        .map(|s| identity_from_exprs(&s.ctor_args))
                        .unwrap_or_else(|| lit(NULL));
                    child_specs.extend(nested.specs);
                    identity
                }
                _ => lit(NULL),
            };

            select_exprs.push(field_expr.alias(name_str));
        }

        // Expand spread
        if spread.is_some() {
            if let Some(type_def_id) = type_def_id {
                let explicit: HashSet<Symbol> = fields.iter().map(|(n, _)| *n).collect();
                if let Some(info) = self.type_index.get(type_def_id) {
                    let spread_exprs: Vec<Expr> = info
                        .field_names
                        .iter()
                        .filter(|n| !explicit.contains(n))
                        .map(|n| {
                            let s = self.gcx.interner.resolve(*n);
                            col(s).alias(s)
                        })
                        .collect();
                    select_exprs = [spread_exprs, select_exprs].concat();
                }
            }
        }

        // Build ctor_exprs
        let ctor_exprs: Vec<Expr> = if !ctor_args.is_empty() {
            let info = self.type_index.get(resolved_type_def_id);
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

        // This record's own spec, followed by any child specs from nested constructors
        let mut specs = Vec::with_capacity(1 + child_specs.len());
        specs.push(EmissionDef {
            type_def_id: resolved_type_def_id,
            select_exprs,
            ctor_args: ctor_exprs,
        });
        specs.extend(child_specs);

        Ok(Value::Emission(Emission {
            frame: LazyFrame::default(), // placeholder — projection sets the real frame
            specs,
        }))
    }

    fn eval_projection(
        &mut self,
        source: ExprId,
        binding: Symbol,
        outputs: &[ExprId],
    ) -> Result<Value, FossilError> {
        let source_val = self.eval(source)?;
        let frame = source_val.into_frame()
            .ok_or_else(|| self.make_error("Projection source must be a Frame"))?;

        self.env.bind(binding, Value::Frame(frame.clone()));

        let mut specs: Vec<EmissionDef> = Vec::new();

        for &expr_id in outputs {
            let val = self.eval(expr_id)?;
            match val {
                Value::Emission(emission) => {
                    specs.extend(emission.specs);
                }
                _ => {} // non-emission outputs ignored
            }
        }

        if specs.len() == 1 && specs[0].ctor_args.is_empty() {
            // Single output with no ctor_args: bake select into pipeline
            let select = specs[0].select_exprs.clone();
            Ok(Value::Emission(Emission {
                frame: frame.select(select),
                specs,
            }))
        } else {
            Ok(Value::Emission(Emission { frame, specs }))
        }
    }

    fn eval_join(
        &mut self,
        left_id: ExprId,
        right_id: ExprId,
        left_on: &[crate::context::Symbol],
        right_on: &[crate::context::Symbol],
        suffix: Option<crate::context::Symbol>,
    ) -> Result<Value, FossilError> {
        let left_val = self.eval(left_id)?;
        let right_val = self.eval(right_id)?;

        let left_frame = left_val.into_frame()
            .ok_or_else(|| self.make_error("Join left side must be a Frame"))?;
        let right_frame = right_val.into_frame()
            .ok_or_else(|| self.make_error("Join right side must be a Frame"))?;

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

        let args = JoinArgs::new(JoinType::Inner)
            .with_suffix(Some(suffix_str.into()));
        let joined = left_frame.join(right_frame, left_on_exprs, right_on_exprs, args);

        Ok(Value::Frame(joined))
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
                    .with_storage(self.storage.clone())
                    .with_outputs(self.outputs.clone());

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
                let ctor_def = self.gcx.definitions.get(ctor_def_id);
                let type_def_id = ctor_def.parent().unwrap_or(ctor_def_id);

                let info = self.type_index.get(type_def_id).ok_or_else(|| {
                    self.make_error(format!(
                        "Record type '{}' not found",
                        self.gcx.interner.resolve(ctor_def.name)
                    ))
                })?;
                let field_names = &info.field_names;
                let ctor_param_count = info.ctor_param_names.len();
                let ctor_param_names = info.ctor_param_names.clone();

                // Identity construction: args match ctor param count (not field count).
                if ctor_param_count > 0
                    && arg_values.len() == ctor_param_count
                    && arg_values.len() != field_names.len()
                {
                    return Ok(Value::Reference { def_id: type_def_id, args: arg_values });
                }

                // Full construction: args match field count.
                if arg_values.len() != field_names.len() {
                    return Err(self.make_error(format!(
                        "Record constructor expects {} arguments (or {} for identity), got {}",
                        field_names.len(),
                        ctor_param_count,
                        arg_values.len()
                    )));
                }

                let select_exprs: Vec<Expr> = field_names
                    .iter()
                    .zip(arg_values.iter())
                    .map(|(field_name, value)| {
                        let name_str = self.gcx.interner.resolve(*field_name);
                        match value {
                            Value::Expr(expr) => expr.clone().alias(name_str),
                            _ => lit(NULL).alias(name_str),
                        }
                    })
                    .collect();

                // Build ctor_exprs from the ctor_param_names
                let ctor_exprs: Vec<Expr> = if !ctor_param_names.is_empty() {
                    ctor_param_names
                        .iter()
                        .filter_map(|param_name| {
                            let idx = field_names.iter().position(|f| f == param_name)?;
                            let alias = self.gcx.interner.resolve(*param_name).to_string();
                            match &arg_values[idx] {
                                Value::Expr(expr) => Some(expr.clone().alias(&*alias)),
                                _ => Some(lit(NULL).alias(&*alias)),
                            }
                        })
                        .collect()
                } else {
                    vec![]
                };

                Ok(Value::Emission(Emission {
                    frame: LazyFrame::default(),
                    specs: vec![EmissionDef {
                        type_def_id,
                        select_exprs,
                        ctor_args: ctor_exprs,
                    }],
                }))
            }

            _ => Err(self.make_error("Attempt to call non-function value")),
        }
    }

    fn eval_field_access(&mut self, expr: ExprId, field: Symbol) -> Result<Value, FossilError> {
        let value = self.eval(expr)?;
        let field_name = self.gcx.interner.resolve(field);

        let mut frame = match value {
            Value::Frame(f) => f,
            Value::Emission(e) => e.frame,
            _ => return Err(self.make_error("Field access on non-record value")),
        };

        let schema = frame.collect_schema()
            .map_err(|e| self.make_error(format!("Failed to collect schema: {}", e)))?;
        if !schema.contains(field_name) {
            return Err(self.make_error(format!(
                "Field '{}' not found in schema. Available: {:?}",
                field_name,
                schema.iter_names().collect::<Vec<_>>()
            )));
        }

        Ok(Value::Expr(col(field_name)))
    }

    fn make_error(&self, msg: impl Into<String>) -> FossilError {
        FossilError::evaluation(msg.into(), Loc::generated())
    }
}

/// Build a single identity expression from ctor args.
/// Single arg: used as-is (stripped of alias). Multiple: concatenated with "_".
pub fn identity_from_exprs(exprs: &[Expr]) -> Expr {
    match exprs.len() {
        0 => lit(NULL),
        1 => strip_alias(&exprs[0]),
        _ => {
            let parts: Vec<Expr> = exprs.iter()
                .map(|e| strip_alias(e).cast(DataType::String))
                .collect();
            concat_str(parts, "_", true)
        }
    }
}

pub fn strip_alias(expr: &Expr) -> Expr {
    match expr {
        Expr::Alias(inner, _) => inner.as_ref().clone(),
        other => other.clone(),
    }
}
