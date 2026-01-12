//! String operations for URI generation and data transformation
//!
//! This module provides string manipulation functions needed for RDF pipelines,
//! particularly for generating URIs from data values.

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::PrimitiveType;
use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::error::RuntimeError;
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
        thir: &mut TypedHir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // (string, string) -> string

        // First parameter: string
        let string_ty1 = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Second parameter: string
        let string_ty2 = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Return type: string
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Function type: (string, string) -> string
        let fn_ty = thir.types.alloc(Type {
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

/// Convert value to string function implementation
///
/// Signature: (T) -> string
///
/// Converts various value types to their string representation.
/// Particularly useful for converting numeric IDs to strings for URI generation.
///
/// Supported types:
/// - Int: Converts to decimal string representation
/// - String: Returns the string as-is
/// - Bool: Converts to "true" or "false"
///
/// # Example
/// ```fossil
/// let id_str = to_string(42)  // "42"
/// let uri = string::concat("http://example.com/item/", to_string(item.id))
/// ```
pub struct ToStringFunction;

impl FunctionImpl for ToStringFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // forall T. (T) -> string
        let t_var = next_type_var();

        // Parameter: T (type variable - can be anything)
        let input_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Return type: string
        let return_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Function type: (T) -> string
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![input_ty], return_ty),
        });

        // Polymorphic type: forall T. (T) -> string
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let value = &args[0];

        let str_repr = match value {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Unit => "()".to_string(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "to_string: unsupported value type (expected int, string, bool, or unit)"
                            .to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        Ok(Value::String(str_repr.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::passes::GlobalContext;

    #[test]
    fn test_string_concat_basic() {
        let func = StringConcatFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(
            vec![
                Value::String("hello".into()),
                Value::String(" world".into()),
            ],
            &ctx,
        );

        assert!(result.is_ok());
        match result.unwrap() {
            Value::String(s) => assert_eq!(s.as_ref(), "hello world"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_string_concat_uri() {
        let func = StringConcatFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(
            vec![
                Value::String("http://example.com/person/".into()),
                Value::String("alice".into()),
            ],
            &ctx,
        );

        assert!(result.is_ok());
        match result.unwrap() {
            Value::String(s) => assert_eq!(s.as_ref(), "http://example.com/person/alice"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_string_concat_requires_strings() {
        let func = StringConcatFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::Int(42), Value::String("test".into())], &ctx);
        assert!(result.is_err());
    }

    #[test]
    fn test_to_string_int() {
        let func = ToStringFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::Int(42)], &ctx);

        assert!(result.is_ok());
        match result.unwrap() {
            Value::String(s) => assert_eq!(s.as_ref(), "42"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_to_string_bool() {
        let func = ToStringFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::Bool(true)], &ctx);

        assert!(result.is_ok());
        match result.unwrap() {
            Value::String(s) => assert_eq!(s.as_ref(), "true"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_to_string_already_string() {
        let func = ToStringFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func.call(vec![Value::String("hello".into())], &ctx);

        assert!(result.is_ok());
        match result.unwrap() {
            Value::String(s) => assert_eq!(s.as_ref(), "hello"),
            _ => panic!("Expected String"),
        }
    }
}
