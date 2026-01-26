//! Record constructor functions
//!
//! This module implements auto-generated constructor functions for record types.
//! When a record type is defined like `type Person = { name: string, age: int }`,
//! a constructor function `Person(name, age)` is automatically generated.
//!
//! # Lazy Execution
//!
//! When arguments are Expr values (from RowContext tracing), the constructor
//! builds select expressions for lazy transformation instead of materializing data.

use std::sync::Arc;

use polars::prelude::*;

use crate::ast::Loc;
use crate::context::{DefId, Symbol};
use crate::error::{CompileError, CompileErrorKind, RuntimeError};
use crate::ir::{Ident, Ir, Polytype, Type, TypeId, TypeKind, TypeVar};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;
use crate::traits::function::{FunctionImpl, RuntimeContext};

/// Record constructor function implementation
///
/// This is a runtime function that constructs record instances from field values.
/// Each record type gets its own constructor with the appropriate signature.
///
/// When called with Expr arguments (from lazy tracing), builds a lazy transformation.
/// When called with concrete values, creates a single-row DataFrame.
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
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // Constructor signature: (T1, T2, ..., Tn) -> RecordType
        // where T1, T2, ..., Tn are the field types

        // Clone field types as parameters
        let params = self.field_types.clone();

        // Return type is the record type (Named type pointing to type_def_id)
        let return_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Named(Ident::Resolved(self.type_def_id)),
        });

        // Function type
        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(params, return_ty),
        });

        // Monomorphic (no type variables)
        Polytype::mono(fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use crate::runtime::value::RecordsPlan;

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

        // Check if any argument is an expression (lazy path)
        let has_exprs = args.iter().any(|v| matches!(v, Value::Expr(_)));

        if has_exprs {
            // LAZY PATH: Build select expressions for transformation
            let select_exprs: Vec<Expr> = self
                .field_names
                .iter()
                .zip(args.into_iter())
                .map(|(field_name, value)| {
                    let name_str = ctx.gcx.interner.resolve(*field_name);
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
            Ok(Value::Records(RecordsPlan::from_exprs(
                select_exprs,
                self.type_def_id,
            )))
        } else {
            // CONCRETE PATH: Build single-row DataFrame
            let mut series_vec: Vec<Series> = Vec::new();

            for (field_name, value) in self.field_names.iter().zip(args.iter()) {
                let field_name_str = ctx.gcx.interner.resolve(*field_name);
                let series = value_to_series(value, field_name_str)?;
                series_vec.push(series);
            }

            // Create DataFrame from series
            let columns: Vec<Column> = series_vec.into_iter().map(Column::from).collect();

            let df = DataFrame::new(columns).map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to create record: {}", e)),
                    Loc::generated(),
                )
            })?;

            // Return as RecordsPlan with type information
            Ok(Value::Records(RecordsPlan::with_type(df.lazy(), self.type_def_id)))
        }
    }
}

/// Convert a Value to a Polars Series with a single element
fn value_to_series(value: &Value, name: &str) -> Result<Series, RuntimeError> {
    match value {
        Value::Int(i) => Ok(Series::new(name.into(), &[*i])),
        Value::String(s) => Ok(Series::new(name.into(), &[s.as_ref()])),
        Value::Bool(b) => Ok(Series::new(name.into(), &[*b])),
        _ => Err(CompileError::new(
            CompileErrorKind::Runtime("Unsupported value type in record constructor".to_string()),
            Loc::generated(),
        )),
    }
}
