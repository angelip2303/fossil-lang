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
        use polars::prelude::{concat_str, DataType};

        // All values are Expr - build concat_str expression
        match (&args[0], &args[1]) {
            (Value::Expr(e1), Value::Expr(e2)) => {
                let concat_expr = concat_str(
                    vec![
                        e1.clone().cast(DataType::String),
                        e2.clone().cast(DataType::String),
                    ],
                    "",
                    true,
                );
                Ok(Value::Expr(concat_expr))
            }
            _ => Err(CompileError::new(
                CompileErrorKind::Runtime("String::concat expects two string arguments".to_string()),
                Loc::generated(),
            )),
        }
    }
}

/// String format function implementation
///
/// Signature: (String, String) -> String
///
/// Replaces `{}` placeholder in template with the value.
/// Used by Entity::with_id for clean URI template handling.
///
/// # Example
/// ```fossil
/// let uri = String::format("http://example.com/person/{}", id)
/// ```
pub struct StringFormatFunction;

impl FunctionImpl for StringFormatFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // (String, String) -> String
        let string_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![string_ty, string_ty], string_ty),
        });

        Polytype::mono(fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};
        use polars::prelude::{concat_str, lit, DataType};

        // Extract template string (must be a literal for parsing)
        let template = args[0].as_literal_string().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "String::format template must be a string literal".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        // Get the value expression
        let value_expr = match &args[1] {
            Value::Expr(e) => e.clone(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("String::format expects an expression value".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Split template on {} and build concat_str
        let parts: Vec<&str> = template.split("{}").collect();
        let mut concat_parts = Vec::new();

        for (i, part) in parts.iter().enumerate() {
            if !part.is_empty() {
                concat_parts.push(lit(*part));
            }
            // Insert value expression after each part except the last
            if i < parts.len() - 1 {
                concat_parts.push(value_expr.clone().cast(DataType::String));
            }
        }

        let concat_expr = concat_str(concat_parts, "", true);
        Ok(Value::Expr(concat_expr))
    }
}
