//! RDF (Resource Description Framework) serialization for knowledge graphs
//!
//! This module provides functionality to serialize Entity-wrapped records
//! to RDF formats (Turtle and N-Triples). It uses attributes on record fields
//! to extract RDF predicates.
//!
//! # Example
//!
//! ```fossil
//! type Person = {
//!     #[uri("http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!
//!     #[uri("http://xmlns.com/foaf/0.1/age")]
//!     age: int
//! }
//!
//! let alice = { name = "Alice", age = 30 }
//! let entity = Entity::wrap(alice)
//! let ttl = rdf::serialize_turtle(entity, "http://example.com/alice")
//! ```

pub mod batch_serializer;
pub mod metadata;

pub use batch_serializer::BatchRdfSerializer;
pub use metadata::RdfMetadata;

use fossil_lang::ast::Loc;
use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::error::RuntimeError;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use crate::entity::{ENTITY_TYPE_ID, EntityMetadata};

/// rdf::serialize function implementation
///
/// Signature: (List<Entity<T>>, string) -> Unit
///
/// Serializes a list of Entity-wrapped values to an RDF file.
/// The format is determined by the file extension (.ttl for Turtle, .nt for N-Triples).
///
/// Prefixes are written once (Turtle format) and the file is written once
/// instead of appending, making this efficient for batch operations.
///
/// # Example
/// ```fossil
/// let people = csv::load("people.csv")
/// let entities = map(people, fn(row) ->
///     row |> Entity::with_id(String::concat("http://example.com/person/", to_string(row.id)))
/// )
/// rdf::serialize(entities, "people.ttl")
/// ```
pub struct RdfSerializeFunction;

impl FunctionImpl for RdfSerializeFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // forall T. (List<T>, string) -> Unit
        let t_var = next_type_var();

        // First parameter: List<Entity<T>> (represented as List in type system)
        // For now, just use a type variable
        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: string (filename)
        let filename_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(fossil_lang::ast::ast::PrimitiveType::String),
        });

        // Output type: Unit
        let output_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(fossil_lang::ast::ast::PrimitiveType::Unit),
        });

        // Function type: (List<T>, string) -> Unit
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, filename_ty], output_ty),
        });

        // Polymorphic type: forall T. (List<T>, string) -> Unit
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};
        use polars::prelude::*;

        let mut args_iter = args.into_iter();

        // Get the list of entities
        let entities_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "rdf::serialize requires a list of entities and a filename".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        // Get the filename
        let filename_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "rdf::serialize requires a list of entities and a filename".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        let filename = match filename_value {
            Value::String(s) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "rdf::serialize filename must be a string".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        // Extract list of entities
        let entities = match entities_value {
            Value::List(list) => list,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "rdf::serialize expects a list as first argument".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

        if entities.is_empty() {
            return Ok(Value::Unit);
        }

        // Convert entities to batch format (LazyFrame + subjects)
        let mut dataframes = Vec::new();
        let mut subjects = Vec::new();
        let mut rdf_metadata: Option<RdfMetadata> = None;

        for entity_value in entities {
            // Extract EntityMetadata from Extension value
            let (entity_meta, inner_value) = match &entity_value {
                Value::Extension {
                    type_id,
                    metadata,
                    value,
                } if *type_id == ENTITY_TYPE_ID => {
                    let meta = metadata
                        .as_any()
                        .downcast_ref::<EntityMetadata>()
                        .ok_or_else(|| {
                            CompileError::new(
                                CompileErrorKind::Runtime("Invalid Entity metadata".to_string()),
                                Loc::generated(),
                            )
                        })?;
                    (meta, value)
                }
                _ => {
                    return Err(CompileError::new(
                        CompileErrorKind::Runtime(
                            "rdf::serialize requires Entity values in list".to_string(),
                        ),
                        Loc::generated(),
                    ));
                }
            };

            // Capture RDF metadata from first entity (assume all have same schema)
            if rdf_metadata.is_none() {
                rdf_metadata = entity_meta.rdf_metadata.clone();
            }

            // Extract LazyFrame from entity
            let lf = match inner_value.as_ref() {
                Value::LazyFrame(lf) => lf.clone(),
                _ => {
                    return Err(CompileError::new(
                        CompileErrorKind::Runtime(
                            "Entity value must be a record (LazyFrame)".to_string(),
                        ),
                        Loc::generated(),
                    ));
                }
            };

            // Collect this entity's dataframe
            let df = lf.collect().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to collect entity data: {}", e)),
                    Loc::generated(),
                )
            })?;

            dataframes.push(df);
            subjects.push(entity_meta.id.clone());
        }

        // Concatenate all dataframes vertically (vstack)
        let combined_df = if dataframes.len() == 1 {
            dataframes.into_iter().next().unwrap()
        } else {
            let mut iter = dataframes.into_iter();
            let mut result = iter.next().unwrap();
            for df in iter {
                result = result.vstack(&df).map_err(|e| {
                    CompileError::new(
                        CompileErrorKind::Runtime(format!("Failed to combine entities: {}", e)),
                        Loc::generated(),
                    )
                })?;
            }
            result
        };

        // Convert back to LazyFrame for efficient processing
        let combined_lf = combined_df.lazy();

        // Get RDF metadata - fail if not present
        let metadata = rdf_metadata.ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "No RDF metadata found. Fields must have #[uri(\"...\")] attributes for RDF serialization.".to_string()
                ),
                Loc::generated(),
            )
        })?;

        // Create Oxigraph store and serialize
        let interner = ctx.gcx.interner.clone();
        let mut serializer = batch_serializer::create_serializer(interner, None).map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to create serializer: {}", e)),
                Loc::generated(),
            )
        })?;

        serializer
            .serialize_to_file(combined_lf, &subjects, &metadata, filename.as_ref())
            .map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Serialization failed: {}", e)),
                    Loc::generated(),
                )
            })?;

        Ok(Value::Unit)
    }
}
