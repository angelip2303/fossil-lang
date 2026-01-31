//! RDF metadata extraction from type attributes
//!
//! # Example
//!
//! ```fossil
//! #[rdf(type = "http://schema.org/Person")]
//! type Person = {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/age")]
//!     age: int,
//! }
//! ```

use std::collections::HashMap;

use fossil_lang::context::{Interner, Symbol, TypeMetadata};
use fossil_macros::FromAttrs;

// ============================================================================
// RDF TERM TYPES
// ============================================================================

/// RDF term type for proper XSD datatype serialization
#[derive(Debug, Clone, Default)]
pub enum RdfTermType {
    /// URI reference: `<http://example.org/thing>`
    Uri,
    /// Plain literal (xsd:string): `"hello"`
    #[default]
    String,
    /// Integer: `"42"^^<xsd:integer>`
    Integer,
    /// Boolean: `"true"^^<xsd:boolean>`
    Boolean,
    /// Decimal: `"3.14"^^<xsd:decimal>`
    Decimal,
    /// Double: `"3.14E0"^^<xsd:double>`
    Double,
    /// Date: `"2024-01-15"^^<xsd:date>`
    Date,
    /// DateTime: `"2024-01-15T10:30:00"^^<xsd:dateTime>`
    DateTime,
}

impl RdfTermType {
    /// XSD datatype suffix for N-Triples (None for plain strings)
    pub fn xsd_suffix(&self) -> Option<&'static str> {
        match self {
            RdfTermType::Uri => None,
            RdfTermType::String => None,
            RdfTermType::Integer => Some("^^<http://www.w3.org/2001/XMLSchema#integer>"),
            RdfTermType::Boolean => Some("^^<http://www.w3.org/2001/XMLSchema#boolean>"),
            RdfTermType::Decimal => Some("^^<http://www.w3.org/2001/XMLSchema#decimal>"),
            RdfTermType::Double => Some("^^<http://www.w3.org/2001/XMLSchema#double>"),
            RdfTermType::Date => Some("^^<http://www.w3.org/2001/XMLSchema#date>"),
            RdfTermType::DateTime => Some("^^<http://www.w3.org/2001/XMLSchema#dateTime>"),
        }
    }

    /// Map Fossil primitive type name to RDF term type
    pub fn from_fossil_type(type_name: &str) -> Self {
        match type_name {
            "int" | "i64" | "i32" => RdfTermType::Integer,
            "bool" => RdfTermType::Boolean,
            "float" | "f64" | "f32" => RdfTermType::Double,
            "string" => RdfTermType::String,
            _ => RdfTermType::String,
        }
    }
}

// ============================================================================
// FIELD INFO
// ============================================================================

/// Complete RDF info for a field: predicate URI + XSD type
#[derive(Debug, Clone)]
pub struct RdfFieldInfo {
    /// Predicate URI (from #[rdf(uri = "...")])
    pub uri: String,
    /// XSD datatype for serialization
    pub term_type: RdfTermType,
}

// ============================================================================
// ATTRIBUTE EXTRACTION (declarative via FromAttrs)
// ============================================================================

/// Type-level RDF attributes
#[derive(Debug, Clone, FromAttrs)]
pub struct RdfTypeAttrs {
    #[attr("rdf.type")]
    pub rdf_type: Option<String>,
}

/// Field-level RDF attributes
#[derive(Debug, Clone, FromAttrs)]
pub struct RdfFieldAttrs {
    #[attr("rdf.uri", field)]
    pub uri: Option<String>,
}

// ============================================================================
// RDF METADATA
// ============================================================================

/// RDF metadata for a type
///
/// Contains rdf:type and per-field info (predicate URI + XSD type).
#[derive(Debug, Clone, Default)]
pub struct RdfMetadata {
    /// rdf:type URI (from #[rdf(type = "...")])
    pub rdf_type: Option<String>,
    /// Per-field: predicate URI + term type
    pub fields: HashMap<Symbol, RdfFieldInfo>,
}

impl RdfMetadata {
    pub fn new() -> Self {
        Self::default()
    }

    /// Extract from TypeMetadata
    ///
    /// Note: Field types default to String. Use `set_field_type` to override
    /// based on schema info, or let the serializer infer from Polars dtypes.
    pub fn from_type_metadata(type_metadata: &TypeMetadata, interner: &Interner) -> Option<Self> {
        let type_attrs = RdfTypeAttrs::from_type_metadata(type_metadata, interner);

        let mut metadata = RdfMetadata {
            rdf_type: type_attrs.rdf_type,
            fields: HashMap::new(),
        };

        for (field_name, field_metadata) in &type_metadata.field_metadata {
            let field_attrs = RdfFieldAttrs::from_field_metadata(field_metadata, interner);

            if let Some(uri) = field_attrs.uri {
                // Default to String, can be overridden or inferred later
                metadata.fields.insert(
                    *field_name,
                    RdfFieldInfo {
                        uri,
                        term_type: RdfTermType::String,
                    },
                );
            }
        }

        if metadata.has_metadata() {
            Some(metadata)
        } else {
            None
        }
    }

    /// Set field type (for when schema info becomes available)
    pub fn set_field_type(&mut self, field: Symbol, term_type: RdfTermType) {
        if let Some(info) = self.fields.get_mut(&field) {
            info.term_type = term_type;
        }
    }

    /// Get field info by name
    pub fn get_field(&self, field: Symbol) -> Option<&RdfFieldInfo> {
        self.fields.get(&field)
    }

    /// Check if has any metadata
    pub fn has_metadata(&self) -> bool {
        self.rdf_type.is_some() || !self.fields.is_empty()
    }
}
