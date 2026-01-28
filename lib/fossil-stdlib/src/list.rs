//! List iteration and transformation functions
//!
//! This module provides functions for iterating over records in a list
//! and applying transformations, enabling functional data processing pipelines.
//!
//! # Lazy Streaming Architecture
//!
//! All transformations are compiled to Polars expressions, enabling true streaming.
//! The closure is "traced" with a Records context that returns Expr values for field
//! accesses, building a lazy transformation plan instead of materializing rows.

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::evaluator::IrEvaluator;
use fossil_lang::runtime::value::{RecordsPlan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

/// Map function implementation
///
/// Signature: forall T, U. (List<T>, (T -> U)) -> List<U>
///
/// Traces the transformation function with a Records context to build Polars expressions.
/// The result is a lazy plan that transforms the source without materializing rows.
///
/// # Example
/// ```fossil
/// let people = csv::load("people.csv")
/// let entities = map(people, fn(row) ->
///     Person(row.name, row.age) |> Entity::with_id("http://example.com/person/${row.id}")
/// )
/// ```
pub struct MapFunction;

impl FunctionImpl for MapFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        // forall T, U. (List<T>, (T -> U)) -> List<U>
        let t_var = next_type_var();
        let u_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        let u_ty = ir.var_type(u_var);

        let list_ctor = gcx.list_type_ctor.expect("List type constructor not registered");
        let list_t = ir.list_type(t_ty, list_ctor);
        let list_u = ir.list_type(u_ty, list_ctor);
        let mapper_fn = ir.fn_type(vec![t_ty], u_ty);

        Polytype::poly(vec![t_var, u_var], ir.fn_type(vec![list_t, mapper_fn], list_u))
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};
        use fossil_lang::runtime::value::Transform;

        // Extract RecordsPlan
        let source_plan = match &args[0] {
            Value::Records(plan) => plan.clone(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("List::map expects a list as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Extract transformation function
        let transform_fn = &args[1];

        // LAZY TRACING: Evaluate the closure with a tracing context
        // Field accesses will return Expr values, building a lazy transformation
        let row_context = Value::Records(RecordsPlan::new(
            fossil_lang::runtime::value::SourceDescriptor::Empty,
            source_plan.schema.as_ref().clone(),
        ));

        let traced_result = trace_transform(transform_fn, row_context, ctx)?;

        match traced_result {
            Value::Records(traced_plan) => {
                // Check if traced_plan is a Pending transformation
                if let Some(select_exprs) = traced_plan.source.take_select_exprs() {
                    // Apply the pending transformation to our source
                    let mut final_plan = source_plan.clone();
                    if !select_exprs.is_empty() {
                        final_plan.transforms.push(Transform::Select(select_exprs));
                    }
                    final_plan.type_def_id = traced_plan.type_def_id;
                    final_plan.schema = traced_plan.schema;
                    Ok(Value::Records(final_plan))
                } else {
                    // Not a pending transformation - use the traced plan directly
                    // This handles cases where the transformation returns a concrete value
                    Ok(Value::Records(traced_plan))
                }
            }
            _ => Err(CompileError::new(
                CompileErrorKind::Runtime(
                    "List::map transform must return a record type".to_string(),
                ),
                Loc::generated(),
            )),
        }
    }
}

/// Trace a transformation function to discover the output structure
fn trace_transform(
    transform_fn: &Value,
    row_context: Value,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    match transform_fn {
        Value::Closure { params, body, env } => {
            let mut closure_env = (**env).clone();

            if let Some(first_param) = params.first() {
                closure_env.bind(first_param.name, row_context);
            } else {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "map function requires at least one parameter".to_string(),
                    ),
                    Loc::generated(),
                ));
            }

            let mut evaluator = IrEvaluator::new(ctx.ir, ctx.gcx, closure_env);
            evaluator.eval(*body)
        }

        Value::BuiltinFunction(_, func) => func.call(vec![row_context], ctx),

        _ => Err(CompileError::new(
            CompileErrorKind::Runtime("map expects a function as second argument".to_string()),
            Loc::generated(),
        )),
    }
}

/// Join two LazyFrames on a common column
///
/// Signature: forall T, U, V. (List<T>, List<U>, String, String) -> List<V>
pub struct JoinFunction;

impl FunctionImpl for JoinFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        // forall T, U, V. (List<T>, List<U>, String, String) -> V
        let t_var = next_type_var();
        let u_var = next_type_var();
        let v_var = next_type_var();

        let t_ty = ir.var_type(t_var);
        let u_ty = ir.var_type(u_var);
        let v_ty = ir.var_type(v_var);

        let list_ctor = gcx.list_type_ctor.expect("List type constructor not registered");
        let left_list = ir.list_type(t_ty, list_ctor);
        let right_list = ir.list_type(u_ty, list_ctor);
        let string_ty = ir.string_type();

        Polytype::poly(
            vec![t_var, u_var, v_var],
            ir.fn_type(vec![left_list, right_list, string_ty, string_ty], v_ty),
        )
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let left_plan = match &args[0] {
            Value::Records(plan) => plan,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a list as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        let right_plan = match &args[1] {
            Value::Records(plan) => plan,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a list as second argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        let left_on = args[2].as_literal_string().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "List::join expects a string literal as third argument (left column name)"
                        .to_string(),
                ),
                Loc::generated(),
            )
        })?;

        let right_on = args[3].as_literal_string().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "List::join expects a string literal as fourth argument (right column name)"
                        .to_string(),
                ),
                Loc::generated(),
            )
        })?;

        use fossil_lang::runtime::value::SourceDescriptor;

        // Build combined schema (left + right, excluding duplicate join column)
        let mut combined_schema = left_plan.schema.as_ref().clone();
        for (name, dtype) in right_plan.schema.iter() {
            if name.as_str() != right_on {
                combined_schema.insert_at_index(combined_schema.len(), name.clone(), dtype.clone())
                    .ok(); // Ignore if already exists
            }
        }

        // Create lazy join - NO materialization!
        let result_plan = RecordsPlan {
            source: SourceDescriptor::Join {
                left: Box::new(left_plan.clone()),
                right: Box::new(right_plan.clone()),
                left_on,
                right_on,
            },
            transforms: Vec::new(),
            type_def_id: None,
            schema: std::sync::Arc::new(combined_schema),
        };

        Ok(Value::Records(result_plan))
    }
}
