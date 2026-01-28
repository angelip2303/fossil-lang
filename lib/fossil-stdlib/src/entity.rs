use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

/// Annotation keys for identity patterns (used by sinks like Rdf::serialize)
pub const IDENTITY_EXPR_KEY: &str = "identity:expr";

/// Entity::with_id function implementation
///
/// Signature: forall T. (T, String) -> T
///
/// Adds an identity pattern to a record plan for serialization.
/// The identity can be a simple string or a string interpolation that
/// produces a Polars expression for lazy evaluation.
///
/// # Example
/// ```fossil
/// // With interpolation (produces lazy Expr):
/// person |> Entity::with_id("http://example.com/person/${row.id}")
///
/// // With simple string:
/// person |> Entity::with_id("http://example.com/person/fixed-id")
/// ```
pub struct EntityWithIdFunction;

impl FunctionImpl for EntityWithIdFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, String) -> T
        let t_var = next_type_var();

        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let string_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![t_ty, string_ty], t_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        if args.len() != 2 {
            return Err(CompileError::new(
                CompileErrorKind::Runtime(
                    "Entity::with_id requires (records, id_pattern)".to_string(),
                ),
                Loc::generated(),
            ));
        }

        let mut args_iter = args.into_iter();
        let inner = args_iter.next().unwrap();
        let id_value = args_iter.next().unwrap();

        // Get the RecordsPlan and add identity expression
        match (inner, id_value) {
            (Value::Records(mut plan), Value::Expr(expr)) => {
                // All strings are now Expr - store directly
                plan.identity_expr = Some(expr);
                Ok(Value::Records(plan))
            }
            _ => Err(CompileError::new(
                CompileErrorKind::Runtime(
                    "Entity::with_id expects (records, string_expr)".to_string(),
                ),
                Loc::generated(),
            )),
        }
    }
}
