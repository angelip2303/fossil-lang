//! Record constructor functions
//!
//! This module implements auto-generated constructor functions for record types.
//! When a record type is defined like `type Person = { name: string, age: int }`,
//! a constructor function `Person(name, age)` is automatically generated.

use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::ast::thir::{Polytype, Type, TypeId, TypeKind, TypeVar, TypedHir};
use crate::context::{DefId, Symbol};
use crate::error::{CompileError, CompileErrorKind, RuntimeError};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::{FunctionImpl, RuntimeContext};

/// Record constructor function implementation
///
/// This is a runtime function that constructs record instances from field values.
/// Each record type gets its own constructor with the appropriate signature.
pub struct RecordConstructorFunction {
    /// DefId of the type this constructor creates
    type_def_id: DefId,
    /// Field names in order (must match call argument order)
    field_names: Vec<Symbol>,
    /// Field types in order
    field_types: Vec<TypeId>,
}

impl RecordConstructorFunction {
    /// Create a new record constructor function
    pub fn new(type_def_id: DefId, field_names: Vec<Symbol>, field_types: Vec<TypeId>) -> Self {
        Self {
            type_def_id,
            field_names,
            field_types,
        }
    }

    /// Create a boxed constructor as Arc<dyn FunctionImpl>
    pub fn boxed(
        type_def_id: DefId,
        field_names: Vec<Symbol>,
        field_types: Vec<TypeId>,
    ) -> Arc<dyn FunctionImpl> {
        Arc::new(Self::new(type_def_id, field_names, field_types))
    }
}

impl FunctionImpl for RecordConstructorFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // Constructor signature: (T1, T2, ..., Tn) -> RecordType
        // where T1, T2, ..., Tn are the field types

        // Clone field types as parameters
        let params = self.field_types.clone();

        // Return type is the record type (Named type pointing to type_def_id)
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Named(self.type_def_id),
        });

        // Function type
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(params, return_ty),
        });

        // Monomorphic (no type variables)
        Polytype::mono(fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        // Verify argument count matches field count
        if args.len() != self.field_names.len() {
            return Err(CompileError::new(
                CompileErrorKind::ArityMismatch {
                    expected: self.field_names.len(),
                    actual: args.len(),
                },
                Loc::generated(),
            ));
        }

        // Build record as a LazyFrame with one row
        // Each field becomes a column (Series) with a single value
        let mut series_vec: Vec<Series> = Vec::new();

        for (field_name, value) in self.field_names.iter().zip(args.iter()) {
            let field_name_str = ctx.gcx.interner.resolve(*field_name);
            let series = value_to_series(value, field_name_str)?;
            series_vec.push(series);
        }

        // Create DataFrame from series
        // Convert Series to Column
        let columns: Vec<Column> = series_vec.into_iter().map(Column::from).collect();

        let df = DataFrame::new(columns).map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to create record: {}", e)),
                Loc::generated(),
            )
        })?;

        // Return as LazyFrame for consistency with record literals
        Ok(Value::Records(df.lazy()))
    }
}

/// Convert a Value to a Polars Series with a single element
fn value_to_series(value: &Value, name: &str) -> Result<Series, RuntimeError> {
    match value {
        Value::Int(i) => Ok(Series::new(name.into(), &[*i])),
        Value::String(s) => Ok(Series::new(name.into(), &[s.as_ref()])),
        Value::Bool(b) => Ok(Series::new(name.into(), &[*b])),
        Value::Records(lf) => {
            // If the value is already a LazyFrame (nested record), collect it
            let df = lf.clone().collect().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to collect nested record: {}", e)),
                    Loc::generated(),
                )
            })?;

            // Convert to Series (TODO: handle multi-row DataFrames)
            if df.height() != 1 {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "Nested records with multiple rows not yet supported in constructors"
                            .to_string(),
                    ),
                    Loc::generated(),
                ));
            }

            // For now, we'll create a struct column
            // This is a placeholder - proper implementation would preserve the full record structure
            Ok(Series::new(name.into(), &[df.height() as i64]))
        }
        _ => Err(CompileError::new(
            CompileErrorKind::Runtime(
                "Unsupported value type in record constructor".to_string()
            ),
            Loc::generated(),
        )),
    }
}
