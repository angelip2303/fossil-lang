//! Type metadata infrastructure
//!
//! This module provides infrastructure for capturing and storing compile-time
//! metadata about types, particularly attributes on record fields. This metadata
//! can then be accessed at runtime by builtin functions.
//!
//! # Architecture
//!
//! ```text
//! AST (RecordField.attrs)
//!   → Resolution (extract_type_metadata)
//!   → GlobalContext.type_metadata
//!   → Runtime (FunctionImpl accesses via RuntimeContext)
//! ```

use std::collections::HashMap;

use crate::ast::ast::Literal;
use crate::context::{DefId, Symbol};

/// Type metadata extracted from AST during name resolution
///
/// Contains metadata about a type definition, particularly attributes
/// on record fields. This information is captured at compile-time and
/// made available to runtime functions via RuntimeContext.
///
/// # Example
/// ```fossil
/// type Person = {
///     #[uri("http://xmlns.com/foaf/0.1/name")]
///     name: string,
/// }
/// ```
///
/// Results in TypeMetadata with:
/// - def_id: DefId of Person
/// - field_metadata: { "name" -> [Attribute("uri", "http://...")] }
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    /// DefId of the type this metadata belongs to
    pub def_id: DefId,

    /// Metadata for each field in the type (if it's a record)
    pub field_metadata: HashMap<Symbol, FieldMetadata>,
}

impl TypeMetadata {
    /// Create new TypeMetadata for a type
    pub fn new(def_id: DefId) -> Self {
        Self {
            def_id,
            field_metadata: HashMap::new(),
        }
    }

    /// Get metadata for a specific field
    pub fn get_field(&self, field: Symbol) -> Option<&FieldMetadata> {
        self.field_metadata.get(&field)
    }

    /// Check if this type has any field metadata
    pub fn is_empty(&self) -> bool {
        self.field_metadata.is_empty()
    }
}

/// Metadata for a single field in a record type
#[derive(Debug, Clone)]
pub struct FieldMetadata {
    /// Attributes attached to this field
    pub attributes: Vec<AttributeData>,
}

impl FieldMetadata {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    /// Find an attribute by name
    pub fn get_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.attributes.iter().find(|attr| attr.name == name)
    }
}

impl Default for FieldMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// Data for a single attribute
///
/// Represents an attribute like `#[uri("http://example.com")]`
/// with the attribute name and its arguments.
#[derive(Debug, Clone)]
pub struct AttributeData {
    /// Name of the attribute (e.g., "uri", "rename")
    pub name: Symbol,

    /// Arguments to the attribute as literals
    pub args: Vec<Literal>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Interner;

    #[test]
    fn test_type_metadata_creation() {
        let def_id = DefId::new(0);
        let metadata = TypeMetadata::new(def_id);

        assert_eq!(metadata.def_id, def_id);
        assert!(metadata.is_empty());
    }

    #[test]
    fn test_field_metadata() {
        let mut interner = Interner::default();
        let uri_sym = interner.intern("uri");

        let mut metadata = FieldMetadata::new();
        metadata.attributes.push(AttributeData {
            name: uri_sym,
            args: vec![],
        });

        let found = metadata.get_attribute(uri_sym);
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, uri_sym);
    }

    #[test]
    fn test_type_metadata_with_fields() {
        let mut interner = Interner::default();
        let def_id = DefId::new(0);
        let name_field = interner.intern("name");

        let mut type_meta = TypeMetadata::new(def_id);
        type_meta
            .field_metadata
            .insert(name_field, FieldMetadata::new());

        assert!(!type_meta.is_empty());
        assert!(type_meta.get_field(name_field).is_some());
    }
}
