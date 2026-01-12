//! RDF metadata extraction from AST attributes
//!
//! This module provides functionality to extract RDF predicate URIs
//! from record field attributes in the AST.

use std::collections::HashMap;

use fossil_lang::ast::ast::{Ast, Literal, RecordField, TypeId, TypeKind};
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

    /// Extract RDF metadata from a record type in the AST
    ///
    /// Looks for #[uri("...")] attributes on record fields and builds
    /// a mapping from field names to predicate URIs.
    ///
    /// # Arguments
    ///
    /// * `ast` - The AST containing type definitions
    /// * `type_id` - The ID of the record type to extract metadata from
    /// * `interner` - String interner for resolving symbols
    ///
    /// # Returns
    ///
    /// `Some(RdfMetadata)` if the type is a record, `None` otherwise
    pub fn from_record_type(ast: &Ast, type_id: TypeId, interner: &Interner) -> Option<Self> {
        let ty = ast.types.get(type_id);

        match &ty.kind {
            TypeKind::Record(fields) => {
                let mut metadata = RdfMetadata::new();

                for field in fields {
                    // Look for #[uri("...")] attribute
                    if let Some(uri) = extract_uri_attribute(&field, interner) {
                        metadata.predicates.insert(field.name, uri);
                    }
                }

                Some(metadata)
            }
            _ => None,
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
    /// * `interner` - String interner for resolving symbols
    ///
    /// # Returns
    ///
    /// `Some(RdfMetadata)` if the type has any URI attributes, `None` otherwise
    pub fn from_type_metadata(
        type_metadata: &TypeMetadata,
        interner: &mut Interner,
    ) -> Option<Self> {
        if type_metadata.is_empty() {
            return None;
        }

        let mut metadata = RdfMetadata::new();
        let uri_symbol = interner.intern("uri");

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

/// Extract URI from #[uri("...")] attribute
fn extract_uri_attribute(field: &RecordField, interner: &Interner) -> Option<String> {
    for attr in &field.attrs {
        let attr_name = interner.resolve(attr.name);

        if attr_name == "uri" {
            // Look for string literal argument
            if let Some(Literal::String(uri_sym)) = attr.args.first() {
                let uri = interner.resolve(*uri_sym).to_string();
                return Some(uri);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use fossil_lang::ast::Loc;
    use fossil_lang::ast::ast::{Attribute, PrimitiveType, Type};

    #[test]
    fn test_extract_uri_attribute() {
        let mut interner = Interner::default();
        let mut ast = Ast::default();

        // Create a record field with #[uri("http://example.com/name")] attribute
        let uri_str = interner.intern("http://example.com/name");
        let attr_name = interner.intern("uri");
        let field_name = interner.intern("name");

        let string_ty = ast.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let field = RecordField {
            name: field_name,
            ty: string_ty,
            attrs: vec![Attribute {
                name: attr_name,
                args: vec![Literal::String(uri_str)],
            }],
        };

        let uri = extract_uri_attribute(&field, &interner);
        assert_eq!(uri, Some("http://example.com/name".to_string()));
    }

    #[test]
    fn test_no_uri_attribute() {
        let mut interner = Interner::default();
        let mut ast = Ast::default();

        let field_name = interner.intern("test_field");
        let string_ty = ast.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let field = RecordField {
            name: field_name,
            ty: string_ty,
            attrs: vec![],
        };

        let uri = extract_uri_attribute(&field, &interner);
        assert_eq!(uri, None);
    }

    #[test]
    fn test_from_record_type() {
        let mut interner = Interner::default();
        let mut ast = Ast::default();

        // Create record type with two fields, one with URI attribute
        let uri_str = interner.intern("http://xmlns.com/foaf/0.1/name");
        let attr_name = interner.intern("uri");
        let name_field = interner.intern("name");
        let age_field = interner.intern("age");

        let string_ty = ast.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let int_ty = ast.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Int),
        });

        let fields = vec![
            RecordField {
                name: name_field,
                ty: string_ty,
                attrs: vec![Attribute {
                    name: attr_name,
                    args: vec![Literal::String(uri_str)],
                }],
            },
            RecordField {
                name: age_field,
                ty: int_ty,
                attrs: vec![],
            },
        ];

        let record_ty = ast.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Record(fields),
        });

        let metadata = RdfMetadata::from_record_type(&ast, record_ty, &interner);
        assert!(metadata.is_some());

        let metadata = metadata.unwrap();
        assert_eq!(metadata.predicates.len(), 1);
        assert_eq!(
            metadata.get_predicate(name_field),
            Some("http://xmlns.com/foaf/0.1/name")
        );
        assert_eq!(metadata.get_predicate(age_field), None);
    }
}
