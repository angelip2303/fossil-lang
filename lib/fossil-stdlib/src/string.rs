//! String operations for URI generation and data transformation
//!
//! This module provides string manipulation functions needed for RDF pipelines,
//! particularly for generating URIs from data values.

use fossil_lang::ast::Loc;
use fossil_lang::ir::{Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

/// String concatenation function implementation
///
/// Signature: (string, string) -> string
///
/// Concatenates two strings together. Useful for building URIs dynamically
/// from base paths and identifiers.
///
/// # Example
/// ```fossil
/// let uri = String::concat("http://example.com/person/", "alice")
/// // uri = "http://example.com/person/alice"
/// ```
pub struct StringConcatFunction;

impl FunctionImpl for StringConcatFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // (string, string) -> string

        // First parameter: string
        let string_ty1 = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Second parameter: string
        let string_ty2 = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Return type: string
        let return_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Function type: (string, string) -> string
        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![string_ty1, string_ty2], return_ty),
        });

        // Monomorphic type (no type variables)
        Polytype::mono(fn_ty)
    }

    fn call(&self, args: Vec<Value>, _: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        // Extract first string
        let s1 = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "String::concat expects two string arguments".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Extract second string
        let s2 = match &args[1] {
            Value::String(s) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "String::concat expects two string arguments".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Concatenate
        let result = format!("{}{}", s1, s2);
        Ok(Value::String(result.into()))
    }
}
