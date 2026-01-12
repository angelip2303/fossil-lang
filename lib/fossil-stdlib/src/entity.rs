//! Entity wrapper for RDF knowledge graphs
//!
//! This module provides the Entity<T> extension type, which wraps a value to mark it
//! as an entity in an RDF knowledge graph. Entity is used in conjunction with
//! attributes on record fields to enable RDF serialization.
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
//! let entity = alice |> Entity::with_id("http://example.com/alice")
//! ```

use std::any::Any;
use std::sync::{Arc, Mutex};

use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::context::Symbol;
use fossil_lang::ast::Loc;
use fossil_lang::context::{DefId, Interner};
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{ExtensionMetadata, ExtensionTypeId, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use once_cell::sync::Lazy;

use crate::rdf::RdfMetadata;

/// Identificador único para el tipo Extension Entity
pub const ENTITY_TYPE_ID: ExtensionTypeId = ExtensionTypeId(1);

/// Identificador para listas de entidades (batch processing)
pub const ENTITY_LIST_TYPE_ID: ExtensionTypeId = ExtensionTypeId(2);

/// Global storage for Entity type constructor DefId
///
/// This is set during stdlib initialization by calling `register_entity_type()`.
/// The DefId is used when constructing `TypeKind::App { ctor: entity_def_id, ... }`
/// in the type system.
static ENTITY_CTOR_DEF_ID: Lazy<Mutex<Option<DefId>>> = Lazy::new(|| Mutex::new(None));

/// Register Entity as a type constructor in the GlobalContext
///
/// This function should be called during stdlib initialization to register
/// Entity as a generic type constructor with arity 1.
///
/// # Arguments
///
/// * `gcx` - Mutable reference to GlobalContext where Entity will be registered
///
/// # Returns
///
/// The DefId assigned to the Entity type constructor
pub fn register_entity_type(gcx: &mut GlobalContext) -> DefId {
    let entity_def_id = gcx.register_type_constructor("Entity", 1);
    *ENTITY_CTOR_DEF_ID.lock().unwrap() = Some(entity_def_id);
    entity_def_id
}

/// Get the DefId of the Entity type constructor
///
/// Returns the DefId that was registered via `register_entity_type()`.
/// Panics if `register_entity_type()` was not called first.
pub fn get_entity_ctor_def_id() -> DefId {
    ENTITY_CTOR_DEF_ID
        .lock()
        .unwrap()
        .expect("Entity type constructor not registered. Call register_entity_type() first.")
}

/// Metadata específica de Entity
///
/// Almacena información de compile-time capturada durante la construcción
/// del Entity, incluyendo el ID/URI del entity y metadata RDF extraída
/// de los atributos del tipo.
#[derive(Debug, Clone)]
pub struct EntityMetadata {
    /// RDF subject URI
    pub id: Arc<str>,

    /// DefId del tipo del valor wrapped
    pub type_def_id: Option<DefId>,

    /// Metadata RDF extraída de atributos #[uri(...)]
    pub rdf_metadata: Option<RdfMetadata>,
}

impl ExtensionMetadata for EntityMetadata {
    fn type_name(&self) -> &str {
        "Entity"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Extension for batch processing of entities
///
/// Represents a collection of entities as a LazyFrame with parallel subject URIs.
/// This is more efficient than Vec<Value> for large batches of entities.
///
/// # Design
///
/// - `data`: LazyFrame containing all entity data (rows are entities, columns are properties)
/// - `subjects`: Vec of subject URIs, parallel to rows in LazyFrame
/// - `rdf_metadata`: Shared RDF metadata for all entities (field URIs from #[uri(...)] attributes)
///
/// This design is extensible beyond RDF:
/// - For RDF: subjects are URIs (http://example.com/person/1)
/// - For SQL: subjects could be foreign keys (person_id = 1)
/// - For Neo4j: subjects could be node IDs (node:123)
#[derive(Clone)]
pub struct EntityExtension {
    /// LazyFrame containing entity data (lazy until serialization)
    pub data: polars::prelude::LazyFrame,

    /// Subject identifiers parallel to LazyFrame rows
    /// For RDF: URIs, For SQL: Foreign keys, For Neo4j: Node IDs
    pub subjects: Vec<Arc<str>>,

    /// Optional type DefId (if entities have same type)
    pub type_def_id: Option<DefId>,

    /// Shared RDF metadata for all entities
    pub rdf_metadata: Option<RdfMetadata>,
}

impl ExtensionMetadata for EntityExtension {
    fn type_name(&self) -> &str {
        "EntityList"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Entity::with_id function implementation
///
/// Signature: forall T. (T, string) -> Entity<T>
///
/// Wraps any value in an Entity extension with an associated RDF subject URI.
/// This is the primary constructor for Entity types. It captures type metadata
/// from compile-time (via RuntimeContext) and stores it with the value.
///
/// # Example
/// ```fossil
/// let alice = { name = "Alice", age = 30 }
/// let entity = alice |> Entity::with_id("http://example.com/alice")
/// ```
pub struct EntityWithIdFunction;

impl FunctionImpl for EntityWithIdFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // forall T. (T, string) -> Entity<T>
        let t_var = next_type_var();

        // First parameter: T (type variable)
        let input_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: string (URI)
        let string_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(fossil_lang::ast::ast::PrimitiveType::String),
        });

        // Create T type for the Entity<T> type argument
        let t_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Output type: Entity<T>
        // Using TypeKind::App to properly represent the generic type
        let entity_ctor = get_entity_ctor_def_id();
        let output_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: entity_ctor,
                args: vec![t_ty],
            },
        });

        // Function type: (T, string) -> Entity<T>
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![input_ty, string_ty], output_ty),
        });

        // Polymorphic type: forall T. (T, string) -> Entity<T>
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        // Extract arguments
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

        // Extract ID string
        let id = match id_value {
            Value::String(s) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Entity::with_id ID must be a string".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Infer DefId from value type context
        let type_def_id = infer_type_def_id(&inner, ctx);

        // Extract RDF metadata if available
        let rdf_metadata = type_def_id.and_then(|def_id| {
            ctx.gcx
                .type_metadata
                .get(&def_id)
                .and_then(|tm| {
                    let mut interner = ctx.gcx.interner.clone();
                    RdfMetadata::from_type_metadata(tm, &mut interner)
                })
        });

        // Create Entity extension
        Ok(Value::Extension {
            type_id: ENTITY_TYPE_ID,
            value: Box::new(inner),
            metadata: Arc::new(EntityMetadata {
                id,
                type_def_id,
                rdf_metadata,
            }),
        })
    }
}

/// Infer DefId from a Value and RuntimeContext
///
/// Attempts to determine the DefId of the type that created this value.
/// For records (LazyFrame), checks ctx.current_type first, then tries to
/// find a matching type definition in type_metadata.
fn infer_type_def_id(value: &Value, ctx: &RuntimeContext) -> Option<DefId> {
    // First try current_type from context
    if ctx.current_type.is_some() {
        return ctx.current_type;
    }

    // If that's not set, try to find a type definition that has metadata
    // For records (LazyFrames), we can try to match structure
    if let Value::LazyFrame(lf) = value {
        // If there's only one type with metadata, use that as a heuristic
        // This works for simple cases like our example
        if ctx.gcx.type_metadata.len() == 1 {
            return ctx.gcx.type_metadata.keys().next().copied();
        }

        // TODO: More sophisticated matching based on field names/types
        // For now, this heuristic works for the common case
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::passes::GlobalContext;

    #[test]
    fn test_entity_metadata_type_name() {
        let metadata = EntityMetadata {
            id: Arc::from("http://example.com/test"),
            type_def_id: None,
            rdf_metadata: None,
        };

        assert_eq!(metadata.type_name(), "Entity");
    }

    #[test]
    fn test_entity_metadata_downcast() {
        let metadata: Arc<dyn ExtensionMetadata> = Arc::new(EntityMetadata {
            id: Arc::from("http://example.com/test"),
            type_def_id: None,
            rdf_metadata: None,
        });

        let downcasted = metadata.as_any().downcast_ref::<EntityMetadata>();
        assert!(downcasted.is_some());
        assert_eq!(downcasted.unwrap().id.as_ref(), "http://example.com/test");
    }

    #[test]
    fn test_entity_with_id_requires_two_args() {
        let func = EntityWithIdFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        // Should fail with only one argument
        let result = func.call(vec![Value::Int(42)], &ctx);
        assert!(result.is_err());
    }

    #[test]
    fn test_entity_with_id_requires_string_id() {
        let func = EntityWithIdFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        // Should fail if second argument is not a string
        let result = func.call(vec![Value::Int(42), Value::Int(99)], &ctx);
        assert!(result.is_err());
    }

    #[test]
    fn test_entity_with_id_creates_extension() {
        let func = EntityWithIdFunction;
        let gcx = GlobalContext::new();
        let thir = TypedHir::default();
        let ctx = RuntimeContext::new(&gcx, &thir);

        let result = func
            .call(
                vec![Value::Int(42), Value::String("http://example.com/42".into())],
                &ctx,
            )
            .unwrap();

        match result {
            Value::Extension {
                type_id,
                value,
                metadata,
            } => {
                assert_eq!(type_id, ENTITY_TYPE_ID);
                assert!(matches!(*value, Value::Int(42)));

                let entity_meta = metadata.as_any().downcast_ref::<EntityMetadata>().unwrap();
                assert_eq!(entity_meta.id.as_ref(), "http://example.com/42");
            }
            _ => panic!("Expected Extension value"),
        }
    }
}
