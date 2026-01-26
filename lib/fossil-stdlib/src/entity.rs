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
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/age")]
//!     age: int
//! }
//!
//! let alice = { name = "Alice", age = 30 }
//! let entity = alice |> Entity::with_id("http://example.com/alice")
//! ```

use std::any::Any;
use std::sync::{Arc, Mutex};

use fossil_lang::ast::Loc;
use fossil_lang::ir::{Ident, Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};
use fossil_lang::context::DefId;
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{ExtensionMetadata, ExtensionTypeId, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use once_cell::sync::Lazy;

use crate::rdf::RdfMetadata;

/// Identificador único para el tipo Extension Entity
pub const ENTITY_TYPE_ID: ExtensionTypeId = ExtensionTypeId(1);

/// Identificador para streams perezosos de entidades
pub const LAZY_ENTITY_STREAM_TYPE_ID: ExtensionTypeId = ExtensionTypeId(2);

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

    /// Metadata RDF extraída de atributos #[rdf(uri = "...")]
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

/// Lazy Streaming Entity Batch for streaming serialization
///
/// Stores a LazyFrame that will be processed in chunks during serialization.
/// This enables O(chunk_size) memory usage instead of O(n).
///
/// Memory: O(chunk_size) per chunk during serialization
#[derive(Clone)]
pub struct LazyStreamingEntityBatch {
    /// Source LazyFrame (not collected until serialize)
    pub lf: polars::prelude::LazyFrame,
    /// Subject URI prefix (e.g., "http://example.org/person/")
    pub subject_prefix: String,
    /// Column name for subject ID (e.g., "id")
    pub id_column: String,
    /// RDF metadata (predicate URIs for each column)
    pub rdf_metadata: RdfMetadata,
}

impl std::fmt::Debug for LazyStreamingEntityBatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LazyStreamingEntityBatch")
            .field("subject_prefix", &self.subject_prefix)
            .field("id_column", &self.id_column)
            .finish()
    }
}

impl ExtensionMetadata for LazyStreamingEntityBatch {
    fn type_name(&self) -> &str {
        "LazyStreamingEntityBatch"
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
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, string) -> Entity<T>
        let t_var = next_type_var();

        // First parameter: T (type variable)
        let input_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: string (URI)
        let string_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        // Create T type for the Entity<T> type argument
        let t_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Output type: Entity<T>
        // Using TypeKind::App to properly represent the generic type
        let entity_ctor = get_entity_ctor_def_id();
        let output_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::App {
                ctor: Ident::Resolved(entity_ctor),
                args: vec![t_ty],
            },
        });

        // Function type: (T, string) -> Entity<T>
        let fn_ty = ir.types.alloc(Type {
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

        // Extract RDF metadata if available (using reference, no clone needed)
        let rdf_metadata = type_def_id.and_then(|def_id| {
            ctx.gcx
                .type_metadata
                .get(&def_id)
                .and_then(|tm| RdfMetadata::from_type_metadata(tm, &ctx.gcx.interner))
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
/// find a matching type definition in type_metadata via structural matching.
fn infer_type_def_id(value: &Value, ctx: &RuntimeContext) -> Option<DefId> {
    // First try current_type from context
    if ctx.current_type.is_some() {
        return ctx.current_type;
    }

    // If that's not set, try to find a type definition that has metadata
    // For records (LazyFrames), use structural matching based on field names
    if let Value::Records(lf) = value {
        // If there's only one type with metadata, use that as a heuristic
        if ctx.gcx.type_metadata.len() == 1 {
            return ctx.gcx.type_metadata.keys().next().copied();
        }

        // Structural matching: collect the first row to get column names
        if let Ok(df) = lf.clone().slice(0, 1).collect() {
            let columns: std::collections::HashSet<String> = df
                .get_column_names()
                .into_iter()
                .map(|s| s.to_string())
                .collect();

            // Find a type that matches all column names
            for (def_id, type_meta) in &ctx.gcx.type_metadata {
                let type_fields: std::collections::HashSet<String> = type_meta
                    .field_metadata
                    .keys()
                    .map(|sym| ctx.gcx.interner.resolve(*sym).to_string())
                    .collect();

                // Check if field names match exactly
                if columns == type_fields {
                    return Some(*def_id);
                }
            }
        }
    }

    None
}
