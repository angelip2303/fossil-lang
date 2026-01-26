//! Option type for optional values
//!
//! This module provides the Option<T> type constructor, which represents
//! values that may or may not be present. Option is used in conjunction with
//! ShEx type providers to handle optional fields (cardinality ?).
//!
//! # Example
//!
//! ```fossil
//! type Person = shex!("schema.shex", shape: "PersonShape")
//! // If PersonShape has: foaf:age xsd:integer ?
//! // Then Person will have: age: Option<int>
//!
//! let person = { name = "Alice", age = Option::some(30) }
//! let person2 = { name = "Bob", age = Option::none() }
//! ```

use std::any::Any;
use std::sync::{Arc, Mutex};

use fossil_lang::ast::Loc;
use fossil_lang::ir::{Ident, Ir, Polytype, Type, TypeKind, TypeVar};
use fossil_lang::context::DefId;
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{ExtensionMetadata, ExtensionTypeId, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use once_cell::sync::Lazy;

/// Unique identifier for the Option extension type
pub const OPTION_TYPE_ID: ExtensionTypeId = ExtensionTypeId(3);

/// Global storage for Option type constructor DefId
///
/// This is set during stdlib initialization by calling `register_option_type()`.
/// The DefId is used when constructing `TypeKind::App { ctor: option_def_id, ... }`
/// in the type system.
static OPTION_CTOR_DEF_ID: Lazy<Mutex<Option<DefId>>> = Lazy::new(|| Mutex::new(None));

/// Register Option as a type constructor in the GlobalContext
///
/// This function should be called during stdlib initialization to register
/// Option as a generic type constructor with arity 1.
///
/// # Arguments
///
/// * `gcx` - Mutable reference to GlobalContext where Option will be registered
///
/// # Returns
///
/// The DefId assigned to the Option type constructor
pub fn register_option_type(gcx: &mut GlobalContext) -> DefId {
    let option_def_id = gcx.register_type_constructor("Option", 1);
    *OPTION_CTOR_DEF_ID.lock().unwrap() = Some(option_def_id);
    option_def_id
}

/// Get the DefId of the Option type constructor
///
/// Returns the DefId that was registered via `register_option_type()`.
/// Panics if `register_option_type()` was not called first.
pub fn get_option_ctor_def_id() -> DefId {
    OPTION_CTOR_DEF_ID
        .lock()
        .unwrap()
        .expect("Option type constructor not registered. Call register_option_type() first.")
}

/// Metadata for Option values
///
/// Stores whether the Option contains a value (Some) or not (None).
#[derive(Debug, Clone)]
pub struct OptionMetadata {
    /// Whether this Option contains a value
    pub is_some: bool,
}

impl ExtensionMetadata for OptionMetadata {
    fn type_name(&self) -> &str {
        "Option"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Create an Option::none() value
///
/// Returns an Option extension with no inner value.
pub fn option_none() -> Value {
    Value::Extension {
        type_id: OPTION_TYPE_ID,
        value: Box::new(Value::Unit),
        metadata: Arc::new(OptionMetadata { is_some: false }),
    }
}

/// Create an Option::some(value) value
///
/// Wraps a value in an Option extension.
pub fn option_some(value: Value) -> Value {
    Value::Extension {
        type_id: OPTION_TYPE_ID,
        value: Box::new(value),
        metadata: Arc::new(OptionMetadata { is_some: true }),
    }
}

/// Unwrap an Option value
///
/// Returns Some(&inner_value) if the Option is Some, None otherwise.
pub fn option_unwrap(opt: &Value) -> Option<&Value> {
    match opt {
        Value::Extension {
            type_id,
            value,
            metadata,
        } if *type_id == OPTION_TYPE_ID => {
            let meta = metadata.as_any().downcast_ref::<OptionMetadata>()?;
            if meta.is_some {
                Some(value.as_ref())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Check if a Value is an Option::some
pub fn is_option_some(opt: &Value) -> bool {
    match opt {
        Value::Extension {
            type_id, metadata, ..
        } if *type_id == OPTION_TYPE_ID => {
            if let Some(meta) = metadata.as_any().downcast_ref::<OptionMetadata>() {
                meta.is_some
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a Value is an Option::none
pub fn is_option_none(opt: &Value) -> bool {
    match opt {
        Value::Extension {
            type_id, metadata, ..
        } if *type_id == OPTION_TYPE_ID => {
            if let Some(meta) = metadata.as_any().downcast_ref::<OptionMetadata>() {
                !meta.is_some
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Option::some function implementation
///
/// Signature: forall T. (T) -> Option<T>
///
/// Wraps a value in an Option.
pub struct OptionSomeFunction;

impl FunctionImpl for OptionSomeFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T) -> Option<T>
        let t_var = next_type_var();

        // Input type: T
        let input_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Create T type for Option<T> type argument
        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Output type: Option<T>
        let option_ctor = get_option_ctor_def_id();
        let output_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(option_ctor),
                args: vec![t_ty],
            },
        });

        // Function type: (T) -> Option<T>
        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![input_ty], output_ty),
        });

        // Polymorphic type: forall T. (T) -> Option<T>
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let value = args.into_iter().next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("Option::some requires a value".to_string()),
                Loc::generated(),
            )
        })?;

        Ok(option_some(value))
    }
}

/// Option::none function implementation
///
/// Signature: forall T. () -> Option<T>
///
/// Creates an empty Option.
pub struct OptionNoneFunction;

impl FunctionImpl for OptionNoneFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. () -> Option<T>
        let t_var = next_type_var();

        // Create T type for Option<T> type argument
        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Output type: Option<T>
        let option_ctor = get_option_ctor_def_id();
        let output_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(option_ctor),
                args: vec![t_ty],
            },
        });

        // Function type: () -> Option<T>
        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![], output_ty),
        });

        // Polymorphic type: forall T. () -> Option<T>
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, _args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        Ok(option_none())
    }
}
