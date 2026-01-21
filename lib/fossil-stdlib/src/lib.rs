pub mod entity;
pub mod list;
pub mod rdf;
pub mod string;

// DataFrame iteration and transformation
pub use list::{JoinFunction, MapFunction};

// Entity extension system
pub use entity::{ENTITY_TYPE_ID, EntityMetadata, EntityWithIdFunction, register_entity_type};

// RDF serialization
pub use rdf::{RdfMetadata, RdfSerializeFunction};

// String operations
pub use string::{StringConcatFunction, ToStringFunction};

use fossil_lang::context::{ArgSpec, ArgType, AttributeSchema, AttributeTarget};
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

    // Register string operations
    gcx.register_function("String", "concat", StringConcatFunction);
    gcx.register_function("String", "to_string", ToStringFunction);

    // Register List functions
    gcx.register_function("List", "map", MapFunction);
    gcx.register_function("List", "join", JoinFunction);

    // Register RDF serialization functions
    gcx.register_function("Rdf", "serialize", RdfSerializeFunction);

    // Register attribute schemas
    register_attribute_schemas(gcx);
}

/// Register all attribute schemas for compile-time validation
///
/// This registers the schemas for:
/// - `#[rdf(uri = "...")]` - RDF predicate mapping for fields
/// - `#[optional]` - Mark a field as optional
/// - `#[sql(...)]` - SQL column mapping for fields
fn register_attribute_schemas(gcx: &mut GlobalContext) {
    // #[rdf(uri = "...", prefix = "...", datatype = "...")]
    // Used to map record fields to RDF predicates
    gcx.register_attribute(
        AttributeSchema::new("rdf", AttributeTarget::Field)
            .arg("uri", ArgSpec::required(ArgType::String).describe("The RDF predicate URI"))
            .arg("prefix", ArgSpec::optional(ArgType::String).describe("Optional namespace prefix"))
            .arg("datatype", ArgSpec::optional(ArgType::String).describe("RDF datatype URI"))
            .description("Maps a field to an RDF predicate for serialization"),
    );

    // #[optional]
    // Used to mark fields as optional (can be missing/null)
    gcx.register_attribute(
        AttributeSchema::new("optional", AttributeTarget::Field)
            .description("Marks a field as optional"),
    );

    // #[sql(column = "...", primary_key = true/false)]
    // Used to map record fields to SQL columns
    gcx.register_attribute(
        AttributeSchema::new("sql", AttributeTarget::Field)
            .arg("column", ArgSpec::optional(ArgType::String).describe("The SQL column name"))
            .arg("primary_key", ArgSpec::optional(ArgType::Bool).describe("Whether this is the primary key"))
            .arg("nullable", ArgSpec::optional(ArgType::Bool).describe("Whether the column can be NULL"))
            .description("Maps a field to a SQL column"),
    );

    // #[deprecated(message = "...")]
    // Used to mark types or fields as deprecated
    gcx.register_attribute(
        AttributeSchema::new("deprecated", AttributeTarget::Any)
            .arg("message", ArgSpec::optional(ArgType::String).describe("Deprecation message"))
            .description("Marks an item as deprecated"),
    );
}
