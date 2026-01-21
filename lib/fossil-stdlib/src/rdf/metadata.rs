//! RDF metadata extraction from type attributes
//!
//! This module provides functionality to extract RDF predicate URIs
//! from record field attributes captured during compilation.

use std::collections::HashMap;

use fossil_lang::ast::ast::Literal;
use fossil_lang::context::{Interner, Symbol, TypeMetadata};

/// RDF metadata extracted from a record type's attributes
///
/// Contains a mapping from field names to their RDF predicate URIs,
/// extracted from #[uri("...")] attributes.
#[derive(Debug, Clone)]
pub struct RdfMetadata {
    /// Mapping from field name to predicate URI
    pub predicates: HashMap<Symbol, String>,
}

impl RdfMetadata {
    /// Create empty RDF metadata
    pub fn new() -> Self {
        Self {
            predicates: HashMap::new(),
        }
    }

    /// Extract RDF metadata from TypeMetadata captured during resolution
    ///
    /// This is the new path that uses compile-time captured metadata instead of
    /// parsing the AST at runtime. Looks for #[uri("...")] attributes in the
    /// TypeMetadata and builds a mapping from field names to predicate URIs.
    ///
    /// # Arguments
    ///
    /// * `type_metadata` - The compile-time captured type metadata
    /// * `interner` - String interner for resolving symbols (immutable reference)
    ///
    /// # Returns
    ///
    /// `Some(RdfMetadata)` if the type has any URI attributes, `None` otherwise
    pub fn from_type_metadata(
        type_metadata: &TypeMetadata,
        interner: &Interner,
    ) -> Option<Self> {
        if type_metadata.is_empty() {
            return None;
        }

        // Use lookup to find "uri" symbol without requiring mutable access
        // If "uri" hasn't been interned, there can't be any URI attributes
        let uri_symbol = interner.lookup("uri")?;

        let mut metadata = RdfMetadata::new();

        for (field_name, field_metadata) in &type_metadata.field_metadata {
            // Look for #[uri("...")] attribute
            if let Some(attr_data) = field_metadata.get_attribute(uri_symbol) {
                // Extract the URI string from the first argument
                if let Some(Literal::String(uri_sym)) = attr_data.args.first() {
                    let uri = interner.resolve(*uri_sym).to_string();
                    metadata.predicates.insert(*field_name, uri);
                }
            }
        }

        if metadata.is_empty() {
            None
        } else {
            Some(metadata)
        }
    }

    /// Get the predicate URI for a field, if it exists
    pub fn get_predicate(&self, field: Symbol) -> Option<&str> {
        self.predicates.get(&field).map(|s| s.as_str())
    }

    /// Check if metadata contains any predicates
    pub fn is_empty(&self) -> bool {
        self.predicates.is_empty()
    }
}

impl Default for RdfMetadata {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::context::{AttributeData, DefId, FieldMetadata};

    #[test]
    fn test_from_type_metadata() {
        let mut interner = Interner::default();

        // Create TypeMetadata with URI attributes
        let def_id = DefId::new(0);
        let mut type_metadata = TypeMetadata::new(def_id);

        let uri_str = interner.intern("http://xmlns.com/foaf/0.1/name");
        let uri_attr = interner.intern("uri");
        let name_field = interner.intern("name");
        let age_field = interner.intern("age");

        // Add field with URI attribute
        let mut name_meta = FieldMetadata::new();
        name_meta.attributes.push(AttributeData {
            name: uri_attr,
            args: vec![Literal::String(uri_str)],
        });
        type_metadata.field_metadata.insert(name_field, name_meta);

        // Add field without URI attribute
        type_metadata
            .field_metadata
            .insert(age_field, FieldMetadata::new());

        let metadata = RdfMetadata::from_type_metadata(&type_metadata, &interner);
        assert!(metadata.is_some());

        let metadata = metadata.unwrap();
        assert_eq!(metadata.predicates.len(), 1);
        assert_eq!(
            metadata.get_predicate(name_field),
            Some("http://xmlns.com/foaf/0.1/name")
        );
        assert_eq!(metadata.get_predicate(age_field), None);
    }

    #[test]
    fn test_empty_type_metadata() {
        let interner = Interner::default();
        let def_id = DefId::new(0);
        let type_metadata = TypeMetadata::new(def_id);

        let metadata = RdfMetadata::from_type_metadata(&type_metadata, &interner);
        assert!(metadata.is_none());
    }
}
