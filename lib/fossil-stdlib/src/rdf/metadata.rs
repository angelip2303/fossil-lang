//! RDF metadata extraction from type attributes
//!
//! This module provides functionality to extract RDF metadata from both
//! type-level and field-level attributes captured during compilation.
//!
//! # Type-Level Attributes
//!
//! ```fossil
//! #[rdf(
//!     type = "http://schema.org/Person",
//!     id = "http://example.org/${id}"
//! )]
//! type Person = shex!("person.shex", shape: "PersonShape")
//! ```
//!
//! - `type`: The rdf:type URI for instances of this type
//! - `id`: Template for generating subject URIs (supports `${column}` interpolation)
//!
//! Columns referenced in `${...}` are automatically extracted from the source data.
//! For example, `${id}` will preserve the `id` column from the source even if
//! it's not part of the target type's fields.
//!
//! # Field-Level Attributes
//!
//! ```fossil
//! type Person = {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//! }
//! ```

use std::collections::HashMap;

use fossil_lang::context::{Interner, Symbol, TypeMetadata};
use fossil_macros::FromAttrs;

/// Type-level RDF attributes extracted declaratively
///
/// Uses the `FromAttrs` derive macro for clean, declarative extraction.
#[derive(Debug, Clone, FromAttrs)]
pub struct RdfTypeAttrs {
    /// The rdf:type URI (from #[rdf(type = "...")])
    #[attr("rdf.type")]
    pub rdf_type: Option<String>,

    /// Template for subject URIs (from #[rdf(id = "...")])
    #[attr("rdf.id")]
    pub id_template: Option<String>,
}

/// Field-level RDF attributes extracted declaratively
#[derive(Debug, Clone, FromAttrs)]
pub struct RdfFieldAttrs {
    /// Predicate URI (from #[rdf(uri = "...")])
    #[attr("rdf.uri", field)]
    pub uri: Option<String>,
}

/// RDF metadata extracted from a record type's attributes
///
/// Contains both type-level metadata (rdf:type, subject ID pattern) and
/// field-level metadata (predicate URIs).
#[derive(Debug, Clone)]
pub struct RdfMetadata {
    /// The rdf:type URI for this type (from #[rdf(type = "...")])
    pub rdf_type: Option<String>,

    /// Template for generating subject URIs (from #[rdf(id = "...")])
    /// Supports interpolation like "http://example.org/${id}"
    pub id_template: Option<String>,

    /// Mapping from field name to predicate URI
    pub predicates: HashMap<Symbol, String>,

    /// Columns referenced in id_template that need to be preserved from source
    /// These are automatically inferred from `${column}` patterns in the template
    pub subject_columns: Vec<String>,
}

impl RdfMetadata {
    /// Create empty RDF metadata
    pub fn new() -> Self {
        Self {
            rdf_type: None,
            id_template: None,
            predicates: HashMap::new(),
            subject_columns: Vec::new(),
        }
    }

    /// Extract RDF metadata from TypeMetadata captured during resolution
    ///
    /// Uses the declarative `FromAttrs` derive macro for clean extraction.
    /// Columns referenced in the id_template via `${column}` are automatically
    /// extracted and stored in `subject_columns`.
    ///
    /// # Arguments
    ///
    /// * `type_metadata` - The compile-time captured type metadata
    /// * `interner` - String interner for resolving symbols
    ///
    /// # Returns
    ///
    /// `Some(RdfMetadata)` if the type has any RDF attributes, `None` otherwise
    pub fn from_type_metadata(type_metadata: &TypeMetadata, interner: &Interner) -> Option<Self> {
        // Use declarative extraction for type-level attributes
        let type_attrs = RdfTypeAttrs::from_type_metadata(type_metadata, interner);

        // Auto-extract columns from id_template
        let subject_columns = type_attrs
            .id_template
            .as_ref()
            .map(|t| extract_template_columns(t))
            .unwrap_or_default();

        let mut metadata = RdfMetadata {
            rdf_type: type_attrs.rdf_type,
            id_template: type_attrs.id_template,
            predicates: HashMap::new(),
            subject_columns,
        };

        // Extract field-level attributes using declarative extraction
        for (field_name, field_metadata) in &type_metadata.field_metadata {
            let field_attrs = RdfFieldAttrs::from_field_metadata(field_metadata, interner);
            if let Some(uri) = field_attrs.uri {
                metadata.predicates.insert(*field_name, uri);
            }
        }

        // Return metadata if it has any content
        if metadata.has_any_metadata() {
            Some(metadata)
        } else {
            None
        }
    }

    /// Get the predicate URI for a field, if it exists
    pub fn get_predicate(&self, field: Symbol) -> Option<&str> {
        self.predicates.get(&field).map(|s| s.as_str())
    }

    /// Check if this type should generate blank nodes (no id template)
    pub fn uses_blank_nodes(&self) -> bool {
        self.id_template.is_none()
    }

    /// Check if metadata contains any predicates
    pub fn is_empty(&self) -> bool {
        self.predicates.is_empty()
    }

    /// Check if metadata has any content (type-level or field-level)
    pub fn has_any_metadata(&self) -> bool {
        self.rdf_type.is_some() || self.id_template.is_some() || !self.predicates.is_empty()
    }
}

impl Default for RdfMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract column names from a template string
///
/// Parses `${column}` patterns and returns a list of column names.
/// For example: `"http://example.org/${id}/${name}"` returns `["id", "name"]`
fn extract_template_columns(template: &str) -> Vec<String> {
    let mut columns = Vec::new();
    let mut current_pos = 0;

    while current_pos < template.len() {
        if let Some(start) = template[current_pos..].find("${") {
            let start_abs = current_pos + start;
            let expr_start = start_abs + 2;

            // Find matching }
            if let Some(end_rel) = template[expr_start..].find('}') {
                let column_name = &template[expr_start..expr_start + end_rel];
                columns.push(column_name.to_string());
                current_pos = expr_start + end_rel + 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    columns
}

