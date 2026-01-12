//! CSV runtime data loading
//!
//! This module provides runtime CSV loading functionality, complementing
//! the compile-time CSV type provider from fossil-providers.
//!
//! While the type provider generates type definitions from CSV schemas at
//! compile-time, this module loads actual CSV data at runtime as LazyFrames.

use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::PrimitiveType;
use fossil_lang::context::Symbol;
use fossil_lang::error::RuntimeError;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::*;

/// CSV load function implementation
///
/// Signature: (string) -> LazyFrame
///
/// Loads a CSV file at runtime and returns its data as a LazyFrame.
/// This allows processing actual CSV data in Fossil code.
///
/// # Example
/// ```fossil
/// let data = csv::load("people.csv")
/// // data is a LazyFrame with all rows from the CSV
/// ```
pub struct CsvLoadFunction;

impl FunctionImpl for CsvLoadFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // (string) -> LazyFrame
        // We represent LazyFrame as a type variable for now, since it's
        // a runtime-only type that doesn't have a compile-time representation

        // Parameter: string (file path)
        let string_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Return type: Unit (placeholder - actual return is LazyFrame)
        // TODO: Define proper LazyFrame type in type system
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Unit),
        });

        // Function type: (string) -> Unit
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![string_ty], return_ty),
        });

        // Monomorphic type (no type variables)
        Polytype::mono(fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};
        use fossil_lang::context::Interner;

        // Extract file path
        let path = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("csv::load expects a string path as first argument".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Load CSV as LazyFrame for lazy evaluation
        // The LazyFrame will defer actual data loading until collect() is called
        let df = CsvReadOptions::default()
            .with_has_header(true)
            .try_into_reader_with_file_path(Some(path.to_string().into()))
            .map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to open CSV file: {}", e)),
                    Loc::generated()
                )
            })?
            .finish()
            .map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to parse CSV file: {}", e)),
                    Loc::generated()
                )
            })?;

        // Return as LazyFrame
        Ok(Value::LazyFrame(df.lazy()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::passes::GlobalContext;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_csv_load_creates_lazyframe() {
        // Create a temporary CSV file
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "id,name,age").unwrap();
        writeln!(temp_file, "1,Alice,30").unwrap();
        writeln!(temp_file, "2,Bob,25").unwrap();
        temp_file.flush().unwrap();

        let func = CsvLoadFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let path = temp_file.path().to_str().unwrap();
        let result = func.call(vec![Value::String(path.into())], &ctx);

        assert!(result.is_ok());
        match result.unwrap() {
            Value::LazyFrame(lf) => {
                let df = lf.collect().unwrap();
                assert_eq!(df.height(), 2); // 2 rows
                assert_eq!(df.width(), 3);  // 3 columns
            }
            _ => panic!("Expected LazyFrame"),
        }
    }

    #[test]
    fn test_csv_load_missing_file() {
        let func = CsvLoadFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::String("nonexistent.csv".into())], &ctx);
        assert!(result.is_err());
    }

    #[test]
    fn test_csv_load_requires_string() {
        let func = CsvLoadFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::Int(42)], &ctx);
        assert!(result.is_err());
    }
}
