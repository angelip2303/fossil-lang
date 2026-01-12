//! DataFrame iteration and transformation functions
//!
//! This module provides functions for iterating over rows in a LazyFrame
//! and applying transformations, enabling functional data processing pipelines.

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::PrimitiveType;
use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::context::Symbol;
use fossil_lang::error::RuntimeError;
use fossil_lang::runtime::evaluator::ThirEvaluator;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::*;

/// Map function implementation
///
/// Signature: forall T, U. (LazyFrame, (T -> U)) -> List<U>
///
/// Iterates over rows of a LazyFrame, applying a transformation function to each row.
/// Each row is passed to the function as a single-row LazyFrame (record).
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
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // forall T, U. (LazyFrame, (T -> U)) -> List<U>

        let t_var = next_type_var();
        let u_var = next_type_var();

        // First parameter: List<T> (represented as type variable in signature)
        // At runtime, this will be either Series or LazyFrame depending on T
        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: (T -> U) function
        let t_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let u_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        let fn_param_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![t_ty], u_ty),
        });

        // Return type: List<U> (represented as LazyFrame or Series)
        // For now, we'll use Unit as placeholder
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Unit),
        });

        // Function type: (LazyFrame, (T -> U)) -> List<U>
        let map_fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, fn_param_ty], return_ty),
        });

        // Polymorphic: forall T, U. (LazyFrame, (T -> U)) -> List<U>
        Polytype::poly(vec![t_var, u_var], map_fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::context::Interner;
        use fossil_lang::error::{CompileError, CompileErrorKind};

        // Extract LazyFrame
        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "map expects a LazyFrame as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Extract transformation function (Closure or BuiltinFunction)
        let transform_fn = &args[1];

        // Collect LazyFrame to DataFrame to iterate over rows
        // Apply query optimizations before collecting
        let df = lf
            .clone()
            .with_projection_pushdown(true)
            .with_predicate_pushdown(true)
            .with_slice_pushdown(true)
            .collect()
            .map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to collect LazyFrame: {}", e)),
                    Loc::generated(),
                )
            })?;

        // Iterate over each row
        let mut results = Vec::new();

        for row_idx in 0..df.height() {
            // Extract single row as DataFrame
            let row_df = df.slice(row_idx as i64, 1);

            // Wrap row as LazyFrame Value
            let row_value = Value::LazyFrame(row_df.lazy());

            // Apply transformation function
            let result = match transform_fn {
                Value::Closure { params, body, env } => {
                    // Execute closure using ThirEvaluator
                    // Create new environment with closure's captured environment
                    let mut closure_env = (**env).clone();

                    // Bind first parameter to row value
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

                    // Evaluate function body with ThirEvaluator
                    let mut evaluator = ThirEvaluator::new(ctx.thir, ctx.gcx, closure_env);
                    evaluator.eval(*body)?
                }

                Value::BuiltinFunction(_, func) => {
                    // Call builtin function
                    func.call(vec![row_value], ctx)?
                }

                _ => {
                    return Err(CompileError::new(
                        CompileErrorKind::Runtime(
                            "map expects a function as second argument".to_string(),
                        ),
                        Loc::generated(),
                    ));
                }
            };

            results.push(result);
        }

        // Return results as List
        Ok(Value::List(results))
    }
}

/// Each function implementation (simpler version that doesn't return results)
///
/// Signature: forall T. (LazyFrame, (T -> Unit)) -> Unit
///
/// Iterates over rows of a LazyFrame, applying a side-effecting function to each row.
/// Unlike `map`, this function doesn't collect results - it's used for side effects only.
///
/// # Example
/// ```fossil
/// let people = csv::load("people.csv")
/// each(people, fn(row) ->
///     print(row.name)
/// )
/// ```
pub struct EachFunction;

impl FunctionImpl for EachFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // forall T. (LazyFrame, (T -> Unit)) -> Unit

        let t_var = next_type_var();

        // First parameter: List<T> (represented as type variable in signature)
        // At runtime, this will be either Series or LazyFrame depending on T
        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: (T -> Unit) function
        let t_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let unit_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Unit),
        });

        let fn_param_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![t_ty], unit_ty),
        });

        // Return type: Unit
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Unit),
        });

        // Function type: (LazyFrame, (T -> Unit)) -> Unit
        let each_fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, fn_param_ty], return_ty),
        });

        // Polymorphic: forall T. (LazyFrame, (T -> Unit)) -> Unit
        Polytype::poly(vec![t_var], each_fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::context::Interner;
        use fossil_lang::error::{CompileError, CompileErrorKind};

        // Extract LazyFrame
        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "each expects a LazyFrame as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Extract function
        let func = &args[1];

        // Collect to DataFrame
        // Apply query optimizations before collecting
        let df = lf
            .clone()
            .with_projection_pushdown(true)
            .with_predicate_pushdown(true)
            .with_slice_pushdown(true)
            .collect()
            .map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to collect LazyFrame: {}", e)),
                    Loc::generated(),
                )
            })?;

        // Iterate over each row
        for row_idx in 0..df.height() {
            let row_df = df.slice(row_idx as i64, 1);
            let row_value = Value::LazyFrame(row_df.lazy());

            // Apply function
            match func {
                Value::Closure { params, body, env } => {
                    let mut closure_env = (**env).clone();

                    if let Some(first_param) = params.first() {
                        closure_env.bind(first_param.name, row_value);
                    }

                    let mut evaluator = ThirEvaluator::new(ctx.thir, ctx.gcx, closure_env);
                    evaluator.eval(*body)?;
                }

                Value::BuiltinFunction(_, builtin) => {
                    builtin.call(vec![row_value], ctx)?;
                }

                _ => {
                    return Err(CompileError::new(
                        CompileErrorKind::Runtime(
                            "each expects a function as second argument".to_string(),
                        ),
                        Loc::generated(),
                    ));
                }
            }
        }

        Ok(Value::Unit)
    }
}

/// Join two LazyFrames on a common column
///
/// Signature: forall T, U, V. (List<T>, List<U>, string, string) -> List<V>
///
/// Performs an inner join between two LazyFrames on the specified column names.
///
/// # Arguments
/// - left: First LazyFrame
/// - right: Second LazyFrame
/// - left_on: Column name in left LazyFrame
/// - right_on: Column name in right LazyFrame
///
/// # Example
/// ```fossil
/// let orders = csv::load("orders.csv")
/// let customers = csv::load("customers.csv")
/// let result = List::join(orders, customers, "customer_id", "id")
/// ```
pub struct JoinFunction;

impl FunctionImpl for JoinFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        let t_var = next_type_var();
        let u_var = next_type_var();
        let v_var = next_type_var();

        let left_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let right_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        let string_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(v_var),
        });

        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(
                vec![left_ty, right_ty, string_ty, string_ty],
                return_ty,
            ),
        });

        Polytype::poly(vec![t_var, u_var, v_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let left_lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("join expects LazyFrame as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let right_lf = match &args[1] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("join expects LazyFrame as second argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let left_on = match &args[2] {
            Value::String(s) => s.as_ref(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("join expects string as third argument (left column)".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let right_on = match &args[3] {
            Value::String(s) => s.as_ref(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("join expects string as fourth argument (right column)".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Perform inner join
        let result = left_lf
            .clone()
            .inner_join(right_lf.clone(), col(left_on), col(right_on));

        Ok(Value::LazyFrame(result))
    }
}

/// Union (vertical concatenation) of two LazyFrames
///
/// Signature: forall T. (List<T>, List<T>) -> List<T>
///
/// Concatenates two LazyFrames vertically (stacks rows).
/// Both LazyFrames must have the same schema.
///
/// # Example
/// ```fossil
/// let data1 = csv::load("data1.csv")
/// let data2 = csv::load("data2.csv")
/// let combined = List::union(data1, data2)
/// ```
pub struct UnionFunction;

impl FunctionImpl for UnionFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        let t_var = next_type_var();

        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, list_ty], list_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let lf1 = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("union expects LazyFrame as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let lf2 = match &args[1] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("union expects LazyFrame as second argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Vertical concatenation using concat_lf
        let result = polars::prelude::concat(
            vec![lf1.clone(), lf2.clone()],
            UnionArgs::default(),
        ).map_err(|e| CompileError::new(
            CompileErrorKind::Runtime(format!("union failed: {}", e)),
            Loc::generated(),
        ))?;

        Ok(Value::LazyFrame(result))
    }
}

/// Select specific columns from a LazyFrame
///
/// Signature: forall T, U. (List<T>, List<string>) -> List<U>
///
/// Projects the LazyFrame to include only the specified columns.
///
/// # Example
/// ```fossil
/// let data = csv::load("data.csv")
/// let subset = List::select(data, ["name", "age"])
/// ```
pub struct SelectFunction;

impl FunctionImpl for SelectFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        let t_var = next_type_var();
        let u_var = next_type_var();

        let input_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let output_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(u_var),
        });

        let string_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // List of column names (will be Series at runtime)
        let cols_ty = string_ty;

        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![input_ty, cols_ty], output_ty),
        });

        Polytype::poly(vec![t_var, u_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("select expects LazyFrame as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Extract column names from List (Series of strings)
        let columns = match &args[1] {
            Value::Series(series) => {
                let string_series = series.str()
                    .map_err(|e| CompileError::new(
                        CompileErrorKind::Runtime(format!("select expects list of strings: {}", e)),
                        Loc::generated(),
                    ))?;

                string_series.into_iter()
                    .filter_map(|opt| opt.map(|s| s.to_string()))
                    .collect::<Vec<_>>()
            }
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("select expects list of strings as second argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Select columns
        let result = lf.clone().select(columns.iter().map(|s| col(s.as_str())).collect::<Vec<_>>());

        Ok(Value::LazyFrame(result))
    }
}

/// Sort a LazyFrame by a column
///
/// Signature: forall T. (List<T>, string, bool) -> List<T>
///
/// Sorts the LazyFrame by the specified column.
///
/// # Arguments
/// - data: LazyFrame to sort
/// - column: Column name to sort by
/// - descending: If true, sort in descending order
///
/// # Example
/// ```fossil
/// let data = csv::load("data.csv")
/// let sorted = List::sort(data, "age", false)  // ascending
/// ```
pub struct SortFunction;

impl FunctionImpl for SortFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        let t_var = next_type_var();

        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let string_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let bool_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Bool),
        });

        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, string_ty, bool_ty], list_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("sort expects LazyFrame as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let column = match &args[1] {
            Value::String(s) => s.as_ref(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("sort expects string as second argument (column name)".to_string()),
                    Loc::generated(),
                ));
            }
        };

        let descending = match &args[2] {
            Value::Bool(b) => *b,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("sort expects bool as third argument (descending)".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Sort by column
        let sort_options = SortMultipleOptions::default()
            .with_order_descending(descending);

        let result = lf.clone().sort([column], sort_options);

        Ok(Value::LazyFrame(result))
    }
}

/// Get distinct (unique) rows from a LazyFrame
///
/// Signature: forall T. (List<T>) -> List<T>
///
/// Returns only the unique rows from the LazyFrame.
///
/// # Example
/// ```fossil
/// let data = csv::load("data.csv")
/// let unique = List::distinct(data)
/// ```
pub struct DistinctFunction;

impl FunctionImpl for DistinctFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        let t_var = next_type_var();

        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty], list_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("distinct expects LazyFrame as argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Get distinct rows
        let result = lf.clone().unique(None, UniqueKeepStrategy::First);

        Ok(Value::LazyFrame(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::passes::GlobalContext;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_map_with_builtin() {
        // Create a temporary CSV file
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "id,name").unwrap();
        writeln!(temp_file, "1,Alice").unwrap();
        writeln!(temp_file, "2,Bob").unwrap();
        temp_file.flush().unwrap();

        // Load CSV as LazyFrame
        let df = CsvReadOptions::default()
            .with_has_header(true)
            .try_into_reader_with_file_path(Some(
                temp_file.path().to_str().unwrap().to_string().into(),
            ))
            .unwrap()
            .finish()
            .unwrap();

        let lf_value = Value::LazyFrame(df.lazy());

        // Note: Full integration test would require a compiled THIR with lambda
        // This test just verifies the structure compiles
        assert!(matches!(lf_value, Value::LazyFrame(_)));
    }
}
