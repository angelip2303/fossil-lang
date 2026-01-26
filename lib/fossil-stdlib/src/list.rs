//! List iteration and transformation functions
//!
//! This module provides functions for iterating over records in a list
//! and applying transformations, enabling functional data processing pipelines.

use fossil_lang::ast::Loc;
use fossil_lang::ir::{Ident, Ir, Polytype, Type, TypeKind, TypeVar};
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::evaluator::IrEvaluator;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::*;

/// Map function implementation
///
/// Signature: forall T, U. (List<T>, (T -> U)) -> List<U>
///
/// Iterates over records in a list, applying a transformation function to each record.
/// The results are collected into a list.
///
/// # Example
/// ```fossil
/// let people = csv::load("people.csv")
/// let entities = map(people, fn(row) ->
///     row |> Entity::with_id(String::concat("http://example.com/person/", to_string(row.id)))
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
        // list_type_ctor should always be available (registered in GlobalContext::new)
        let list_ctor = gcx.list_type_ctor
            .expect("List type constructor not registered. This is a bug - GlobalContext::new() should register it.");

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
        use std::sync::Arc;

        // Extract LazyFrame
        let lf = match &args[0] {
            Value::Records(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::map expects a list as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Extract transformation function (Closure or BuiltinFunction)
        let transform_fn = &args[1];

        // STREAMING: Use slice(0,1) for pattern detection, keep LazyFrame lazy
        // Memory: O(chunk_size) during serialization instead of O(n)
        let sample_df = lf.clone().slice(0, 1).collect().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to get sample: {}", e)),
                Loc::generated(),
            )
        })?;

        // Try streaming approach (works for Entity transformations)
        if sample_df.height() > 0
            && let Some(lazy_stream) =
                try_create_lazy_streaming_batch(lf.clone(), &sample_df, transform_fn, ctx)?
        {
            return Ok(lazy_stream);
        }

        // Fallback for non-Entity transformations (e.g., map to strings)
        let df = lf.clone().collect().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to collect: {}", e)),
                Loc::generated(),
            )
        })?;

        let df_arc = Arc::new(df);
        let row_count = df_arc.height();
        let mut results = Vec::with_capacity(row_count);

        for row_idx in 0..row_count {
            let row_value = Value::Record(Arc::clone(&df_arc), row_idx);
            let result = apply_transform(transform_fn, row_value, ctx)?;
            results.push(result);
        }

        Ok(Value::List(results))
    }
}

/// Apply a transformation function to a value
fn apply_transform(
    transform_fn: &Value,
    row_value: Value,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    match transform_fn {
        Value::Closure { params, body, env } => {
            let mut closure_env = (**env).clone();

            if let Some(first_param) = params.first() {
                closure_env.bind(first_param.name, row_value);
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

        Value::BuiltinFunction(_, func) => func.call(vec![row_value], ctx),

        _ => Err(CompileError::new(
            CompileErrorKind::Runtime("map expects a function as second argument".to_string()),
            Loc::generated(),
        )),
    }
}

/// Create a LazyStreamingEntityBatch for TRUE streaming serialization
///
/// Uses the sample DataFrame to detect pattern, but stores LazyFrame for streaming.
/// Memory: O(chunk_size) during serialization instead of O(n)
fn try_create_lazy_streaming_batch(
    lf: polars::prelude::LazyFrame,
    sample_df: &DataFrame,
    transform_fn: &Value,
    ctx: &RuntimeContext,
) -> Result<Option<Value>, RuntimeError> {
    use crate::entity::{
        ENTITY_TYPE_ID, EntityMetadata, LAZY_ENTITY_STREAM_TYPE_ID, LazyStreamingEntityBatch,
    };
    use fossil_lang::error::{CompileError, CompileErrorKind};
    use std::sync::Arc;

    if sample_df.height() == 0 {
        return Ok(None);
    }

    // Create sample row value and evaluate transform
    let sample_df_arc = Arc::new(sample_df.clone());
    let first_row = Value::Record(sample_df_arc.clone(), 0);
    let first_result = apply_transform(transform_fn, first_row, ctx)?;

    // Check if result is Entity and extract metadata
    let (subject_uri, rdf_metadata, inner_value) = match &first_result {
        Value::Extension {
            type_id,
            metadata,
            value,
        } if *type_id == ENTITY_TYPE_ID => {
            let meta = metadata
                .as_any()
                .downcast_ref::<EntityMetadata>()
                .ok_or_else(|| {
                    CompileError::new(
                        CompileErrorKind::Runtime("Invalid Entity metadata".to_string()),
                        Loc::generated(),
                    )
                })?;
            let rdf = meta.rdf_metadata.clone().ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime("Entity has no RDF metadata".to_string()),
                    Loc::generated(),
                )
            })?;
            (meta.id.to_string(), rdf, value.as_ref())
        }
        _ => return Ok(None), // Not an Entity, use standard approach
    };

    // Check if the Entity's inner value uses the same columns as the source DataFrame
    // If the inner value is a new record (not from the source DataFrame), we can't use
    // the streaming optimization and need to fall back to per-item processing
    let inner_columns: Option<std::collections::HashSet<String>> = match inner_value {
        Value::Records(inner_lf) => {
            if let Ok(inner_df) = inner_lf.clone().slice(0, 1).collect() {
                Some(
                    inner_df
                        .get_column_names()
                        .into_iter()
                        .map(|s| s.to_string())
                        .collect(),
                )
            } else {
                None
            }
        }
        Value::Record(inner_df, _) => Some(
            inner_df
                .get_column_names()
                .into_iter()
                .map(|s| s.to_string())
                .collect(),
        ),
        _ => None,
    };

    if let Some(inner_cols) = inner_columns {
        let source_cols: std::collections::HashSet<String> = sample_df
            .get_column_names()
            .into_iter()
            .map(|s| s.to_string())
            .collect();

        // If columns don't match, we're transforming to a new record type
        // Fall back to non-streaming path
        if inner_cols != source_cols {
            return Ok(None);
        }
    }

    // Deduce subject pattern from sample
    let (subject_prefix, id_column) = deduce_subject_pattern(&sample_df_arc, &subject_uri)?;

    // Create LazyStreamingEntityBatch - LazyFrame stays lazy!
    let batch = LazyStreamingEntityBatch {
        lf,
        subject_prefix,
        id_column,
        rdf_metadata,
    };

    Ok(Some(Value::Extension {
        type_id: LAZY_ENTITY_STREAM_TYPE_ID,
        value: Box::new(Value::Unit),
        metadata: Arc::new(batch),
    }))
}

/// Deduce subject URI pattern from first row
///
/// Given subject URI "http://example.org/person/123" and DataFrame with id=123,
/// returns ("http://example.org/person/", "id")
fn deduce_subject_pattern(
    df: &DataFrame,
    subject_uri: &str,
) -> Result<(String, String), RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    // Try each column to find which one's value appears at the end of the URI
    for col in df.get_columns() {
        let col_name = col.name().to_string();

        // Get first row value as string
        let first_val = match col.get(0) {
            Ok(polars::prelude::AnyValue::Int64(n)) => n.to_string(),
            Ok(polars::prelude::AnyValue::Int32(n)) => n.to_string(),
            Ok(polars::prelude::AnyValue::UInt64(n)) => n.to_string(),
            Ok(polars::prelude::AnyValue::UInt32(n)) => n.to_string(),
            Ok(polars::prelude::AnyValue::String(s)) => s.to_string(),
            Ok(polars::prelude::AnyValue::StringOwned(s)) => s.to_string(),
            Ok(polars::prelude::AnyValue::Null) => continue,
            _ => continue,
        };

        // Check if subject_uri ends with this value
        if subject_uri.ends_with(&first_val) {
            let prefix_len = subject_uri.len() - first_val.len();
            let prefix = &subject_uri[..prefix_len];
            return Ok((prefix.to_string(), col_name));
        }
    }

    // Fallback: couldn't deduce pattern
    Err(CompileError::new(
        CompileErrorKind::Runtime(format!(
            "Could not deduce subject pattern from URI '{}'. Ensure the subject URI ends with a column value.",
            subject_uri
        )),
        Loc::generated(),
    ))
}

/// Join two LazyFrames on a common column
///
/// Signature: forall T, U, V, F1, F2. (List<T>, List<U>, FieldSelector<T, F1>, FieldSelector<U, F2>) -> List<V>
///
/// Performs an inner join between two LazyFrames on the specified column names.
///
/// # Arguments
/// - left: First LazyFrame
/// - right: Second LazyFrame
/// - left_on: Field selector for column in left LazyFrame (_.column_name)
/// - right_on: Field selector for column in right LazyFrame (_.column_name)
///
/// # Example
/// ```fossil
/// let orders = csv::load("orders.csv")
/// let customers = csv::load("customers.csv")
/// let result = List::join(orders, customers, _.customer_id, _.id)
/// ```
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
        // Field type variables for the selectors
        let f1_var = next_type_var();
        let f2_var = next_type_var();

        // T - element type of left list (record type)
        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // U - element type of right list (record type)
        let u_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        // F1, F2 - field types
        let f1_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(f1_var),
        });

        let f2_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(f2_var),
        });

        // First parameter: List<T>
        let list_ctor = gcx
            .list_type_ctor
            .expect("List type constructor not registered");

        let left_list_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![t_ty],
            },
        });

        // Second parameter: List<U>
        let right_list_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(list_ctor),
                args: vec![u_ty],
            },
        });

        // Third parameter: FieldSelector<T, F1> - selects a field from T
        // We use the wildcard symbol since the actual field will be determined at call site
        let left_selector_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::FieldSelector {
                record_ty: t_ty,
                field_ty: f1_ty,
                field: gcx.wildcard_symbol(),
            },
        });

        // Fourth parameter: FieldSelector<U, F2> - selects a field from U
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
                vec![
                    left_list_ty,
                    right_list_ty,
                    left_selector_ty,
                    right_selector_ty,
                ],
                return_ty,
            ),
        });

        Polytype::poly(vec![t_var, u_var, v_var, f1_var, f2_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let left_lf = match &args[0] {
            Value::Records(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a list as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        let right_lf = match &args[1] {
            Value::Records(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a list as second argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Column names from FieldSelector
        let left_on = match &args[2] {
            Value::FieldSelector(s) => s.as_ref().to_string(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "List::join expects a field selector (_.column) as third argument (left column)".to_string(),
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
                        "List::join expects a field selector (_.column) as fourth argument (right column)".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Validate that columns exist in their respective LazyFrames
        let left_schema = left_lf.clone().collect_schema().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to get schema for left list: {}", e)),
                Loc::generated(),
            )
        })?;

        let right_schema = right_lf.clone().collect_schema().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to get schema for right list: {}", e)),
                Loc::generated(),
            )
        })?;

        // Check that left_on column exists in left LazyFrame
        if left_schema.get(&left_on).is_none() {
            let available_cols: Vec<_> = left_schema.iter_names().map(|s| s.as_str()).collect();
            return Err(CompileError::new(
                CompileErrorKind::Runtime(format!(
                    "Column '{}' not found in left list. Available columns: {:?}",
                    left_on, available_cols
                )),
                Loc::generated(),
            ));
        }

        // Check that right_on column exists in right LazyFrame
        if right_schema.get(&right_on).is_none() {
            let available_cols: Vec<_> = right_schema.iter_names().map(|s| s.as_str()).collect();
            return Err(CompileError::new(
                CompileErrorKind::Runtime(format!(
                    "Column '{}' not found in right list. Available columns: {:?}",
                    right_on, available_cols
                )),
                Loc::generated(),
            ));
        }

        // Perform inner join
        let result = left_lf
            .clone()
            .inner_join(right_lf.clone(), col(&left_on), col(&right_on));

        Ok(Value::Records(result))
    }
}
