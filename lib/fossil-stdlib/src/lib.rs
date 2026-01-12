// TODO: Fix builtin.rs - has incorrect imports
// pub mod builtin;
pub mod csv;
pub mod entity;
pub mod list;
pub mod rdf;
pub mod string;

// CSV runtime loading
pub use csv::CsvLoadFunction;

// DataFrame iteration and transformation
pub use list::{
    EachFunction,
    MapFunction,
    JoinFunction,
    UnionFunction,
    SelectFunction,
    SortFunction,
    DistinctFunction,
};

// Entity extension system
pub use entity::{ENTITY_TYPE_ID, EntityMetadata, EntityWithIdFunction, register_entity_type};

// RDF serialization
pub use rdf::{RdfMetadata, RdfSerializeFunction};

// String operations
pub use string::{StringConcatFunction, ToStringFunction};

use fossil_lang::passes::GlobalContext;

/// Initialize the fossil-stdlib by registering all types and functions
///
/// This function should be called before using any stdlib functionality
/// to ensure that generic types like Entity are properly registered in
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
    // Register Entity as a type constructor
    register_entity_type(gcx);

    // Register Entity functions
    gcx.register_function("Entity", "with_id", EntityWithIdFunction);

    // NOTE: csv::load is NOT registered here because the CSV type provider
    // generates type-specific load functions (e.g., Person::load()) with
    // the file path captured at compile-time. Registering a generic csv::load
    // here would conflict with those provider-generated functions.
    // gcx.register_function("csv", "load", CsvLoadFunction);

    // Register string operations
    gcx.register_function("String", "concat", StringConcatFunction);
    gcx.register_function("String", "to_string", ToStringFunction);

    // Register List iteration functions
    gcx.register_function("List", "map", MapFunction);
    gcx.register_function("List", "each", EachFunction);

    // Register List DataFrame operations
    gcx.register_function("List", "join", JoinFunction);
    gcx.register_function("List", "union", UnionFunction);
    gcx.register_function("List", "select", SelectFunction);
    gcx.register_function("List", "sort", SortFunction);
    gcx.register_function("List", "distinct", DistinctFunction);

    // Register RDF serialization functions
    gcx.register_function("rdf", "serialize", RdfSerializeFunction);
}
