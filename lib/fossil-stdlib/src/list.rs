//! List iteration and transformation functions
//!
//! This module provides functions for iterating over records in a list
//! and applying transformations, enabling functional data processing pipelines.
//!
//! # Lazy Streaming Architecture
//!
//! All transformations are compiled to Polars expressions, enabling true streaming.
//! The closure is "traced" with a RowContext that returns Expr values for field
//! accesses, building a lazy transformation plan instead of materializing rows.

use std::sync::Arc;

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ident, Ir, Polytype, Type, TypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::evaluator::IrEvaluator;
use fossil_lang::runtime::value::{RecordsPlan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::*;

/// Map function implementation
///
/// Signature: forall T, U. (List<T>, (T -> U)) -> List<U>
///
/// Traces the transformation function with a RowContext to build Polars expressions.
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

        // T - element type
        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // U - result element type
        let u_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        // First parameter: List<T>
        let list_ctor = gcx
            .list_type_ctor
            .expect("List type constructor not registered");

        let list_t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![t_ty],
            },
        });

        // Second parameter: (T -> U) function
        let fn_param_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![t_ty], u_ty),
        });

        // Return type: List<U>
        let return_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![u_ty],
            },
        });

        // Function type: (List<T>, (T -> U)) -> List<U>
        let map_fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_t_ty, fn_param_ty], return_ty),
        });

        // Polymorphic: forall T, U. (List<T>, (T -> U)) -> List<U>
        Polytype::poly(vec![t_var, u_var], map_fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        // Extract RecordsPlan (LazyFrame + metadata)
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

        // Get schema from source to create RowContext
        let schema = source_plan.lf.clone().collect_schema().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to get schema: {}", e)),
                Loc::generated(),
            )
        })?;

        // LAZY TRACING: Evaluate the closure with RowContext
        // Field accesses will return Expr values, building a lazy transformation
        let row_context = Value::RowContext {
            schema: Arc::new((*schema).clone()),
        };

        let traced_result = trace_transform(transform_fn, row_context, ctx)?;

        match traced_result {
            Value::Records(traced_plan) => {
                // Get the select expressions from the traced plan
                let select_exprs = traced_plan.select_exprs.clone().unwrap_or_default();

                // Apply the transformation to the source LazyFrame
                let transformed_lf = if select_exprs.is_empty() {
                    source_plan.lf.clone()
                } else {
                    source_plan.lf.clone().select(select_exprs)
                };

                let mut final_plan = RecordsPlan::new(transformed_lf);
                final_plan.type_def_id = traced_plan.type_def_id;
                final_plan.subject_pattern = traced_plan.subject_pattern;

                Ok(Value::Records(final_plan))
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

/// Trace a transformation function with RowContext to discover the output structure
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
/// Signature: forall T, U, V, F1, F2. (List<T>, List<U>, FieldSelector<T, F1>, FieldSelector<U, F2>) -> List<V>
pub struct JoinFunction;

impl FunctionImpl for JoinFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        let t_var = next_type_var();
        let u_var = next_type_var();
        let v_var = next_type_var();
        let f1_var = next_type_var();
        let f2_var = next_type_var();

        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let u_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        let f1_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(f1_var),
        });

        let f2_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(f2_var),
        });

        let list_ctor = gcx.list_type_ctor.expect("List type constructor not registered");

        let left_list_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![t_ty],
            },
        });

        let right_list_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![u_ty],
            },
        });

        let left_selector_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::FieldSelector {
                record_ty: t_ty,
                field_ty: f1_ty,
                field: gcx.wildcard_symbol(),
            },
        });

        let right_selector_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::FieldSelector {
                record_ty: u_ty,
                field_ty: f2_ty,
                field: gcx.wildcard_symbol(),
            },
        });

        let return_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(v_var),
        });

        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(
                vec![left_list_ty, right_list_ty, left_selector_ty, right_selector_ty],
                return_ty,
            ),
        });

        Polytype::poly(vec![t_var, u_var, v_var, f1_var, f2_var], fn_ty)
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

        let left_on = match &args[2] {
            Value::FieldSelector(s) => s.as_ref().to_string(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a field selector as third argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        let right_on = match &args[3] {
            Value::FieldSelector(s) => s.as_ref().to_string(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a field selector as fourth argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Perform inner join (lazy)
        let result = left_plan
            .lf
            .clone()
            .inner_join(right_plan.lf.clone(), col(&left_on), col(&right_on));

        Ok(Value::Records(RecordsPlan::new(result)))
    }
}
