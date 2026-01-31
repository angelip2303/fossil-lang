pub mod rdf;

// RDF serialization
pub use rdf::{MapFunction, RdfMetadata, RdfSerializeFunction};

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
/// let mut gcx = GlobalContext::default();
/// fossil_stdlib::init(&mut gcx);
/// ```
pub fn init(gcx: &mut GlobalContext) {
    // Register RDF serialization functions
    gcx.register_function("Rdf", "serialize", RdfSerializeFunction);

    // Register map() as a variadic function for multi-output streaming
    // Accepts: map(source, fn1, fn2, fn3, ...)
    gcx.register_variadic_toplevel_function("map", MapFunction);
}
