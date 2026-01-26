//! Entity wrapper for RDF knowledge graphs
//!
//! This module provides `Entity::with_id` which adds RDF subject URI patterns
//! to record plans, enabling RDF serialization.
//!
//! # Streaming Example (recommended)
//!
//! ```fossil
//! // Put RDF attributes directly on the CSV type for true streaming
//! type Person = csv!("people.csv") with {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!     age: int
//! }
//!
//! Person::load()
//!     |> Entity::with_id("http://example.com/person/${name}")
//!     |> Rdf::serialize("output.nt")
//! ```
//!
//! # With List::map (breaks streaming)
//!
//! ```fossil
//! type PersonData = csv!("people.csv")
//! type Person = { #[rdf(...)] name: string, ... }
//!
//! PersonData::load()
//!     |> List::map(fn(row) -> Person(...) |> Entity::with_id(...))
//!     |> Rdf::serialize(...)
//! ```

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{SubjectPattern, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

/// Entity::with_id function implementation
///
/// Signature: forall T. (T, string) -> T
///
/// Adds a subject URI pattern to a record plan for RDF serialization.
/// The record keeps its type but gains RDF subject information.
///
/// # Example
/// ```fossil
/// let entity = person |> Entity::with_id("http://example.com/person/${person.id}")
/// ```
pub struct EntityWithIdFunction;

impl FunctionImpl for EntityWithIdFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, string) -> T
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

        let mut args_iter = args.into_iter();

        let inner = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("Entity::with_id requires a value and an ID".to_string()),
                Loc::generated(),
            )
        })?;

        let id_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("Entity::with_id requires a value and an ID".to_string()),
                Loc::generated(),
            )
        })?;

        let subject_uri = match id_value {
            Value::String(s) => s.to_string(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Entity::with_id ID must be a string".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Get the RecordsPlan and add subject pattern
        match inner {
            Value::Records(mut plan) => {
                // Parse subject URI to extract prefix and id_column
                let subject_pattern = parse_subject_pattern(&subject_uri)?;
                plan.subject_pattern = Some(subject_pattern);
                Ok(Value::Records(plan))
            }
            _ => Err(CompileError::new(
                CompileErrorKind::Runtime(
                    "Entity::with_id expects a records value".to_string(),
                ),
                Loc::generated(),
            )),
        }
    }
}

/// Parse subject URI template to extract prefix and id_column
///
/// Supports template syntax: "http://example.com/person/${column_name}"
/// This is lazy-friendly - no data collection required.
fn parse_subject_pattern(subject_uri: &str) -> Result<SubjectPattern, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    // Look for ${...} pattern in the URI
    if let Some(start) = subject_uri.find("${") {
        if let Some(end) = subject_uri[start..].find('}') {
            let prefix = &subject_uri[..start];
            let id_column = &subject_uri[start + 2..start + end];

            // Check for nested field access like ${row.name} -> extract just "name"
            let id_column = if let Some(dot_pos) = id_column.rfind('.') {
                &id_column[dot_pos + 1..]
            } else {
                id_column
            };

            return Ok(SubjectPattern {
                prefix: prefix.to_string(),
                id_column: id_column.to_string(),
            });
        }
    }

    Err(CompileError::new(
        CompileErrorKind::Runtime(format!(
            "Subject URI must contain a template like ${{column_name}}. Got: '{}'",
            subject_uri
        )),
        Loc::generated(),
    ))
}
