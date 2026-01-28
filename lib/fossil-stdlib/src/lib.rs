pub mod entity;
pub mod list;
pub mod rdf;
pub mod string;

// DataFrame iteration and transformation
pub use list::{JoinFunction, MapFunction};

// Entity functions
pub use entity::EntityWithIdFunction;

// RDF serialization
pub use rdf::{RdfMetadata, RdfSerializeFunction};

// String operations
pub use string::{StringConcatFunction, StringFormatFunction};

use fossil_lang::passes::GlobalContext;

/// Initialize the fossil-stdlib by registering all types and functions
///
/// This function should be called before using any stdlib functionality
/// to ensure that generic types like Option are properly registered in
/// the type system.
///
/// # Arguments
///
/// * `gcx` - Mutable reference to the GlobalContext
///
/// # Example
///
/// ```rust
/// use fossil_lang::passes::GlobalContext;
/// use fossil_stdlib;
///
/// let mut gcx = GlobalContext::new();
/// fossil_stdlib::init(&mut gcx);
/// ```
pub fn init(gcx: &mut GlobalContext) {
    // Register Entity functions (Entity::with_id adds subject pattern to plans)
    gcx.register_function("Entity", "with_id", EntityWithIdFunction);

    // Register string operations
    gcx.register_function("String", "concat", StringConcatFunction);
    gcx.register_function("String", "format", StringFormatFunction);

    // Register List functions
    gcx.register_function("List", "map", MapFunction);
    gcx.register_function("List", "join", JoinFunction);

    // Register RDF serialization functions
    gcx.register_function("Rdf", "serialize", RdfSerializeFunction);
}
