use std::collections::HashMap;

use fossil_lang::ast::Loc;
use fossil_lang::context::{Interner, Symbol, TypeMetadata};
use fossil_lang::error::{FossilWarning, FossilWarnings};
use fossil_macros::FromAttrs;

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

#[derive(Debug, Clone, Default)]
pub enum RdfTermType {
    Uri,
    #[default]
    String,
    Integer,
    Boolean,
    Decimal,
    Double,
    Date,
    DateTime,
}

impl RdfTermType {
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
}

#[derive(Debug, Clone)]
pub struct RdfFieldInfo {
    pub uri: String,
    pub term_type: RdfTermType,
}

#[derive(Debug, Clone, FromAttrs)]
pub struct RdfTypeAttrs {
    #[attr("rdf.type")]
    pub rdf_type: Option<String>,
    #[attr("rdf.base")]
    pub base: Option<String>,
}

#[derive(Debug, Clone, FromAttrs)]
pub struct RdfFieldAttrs {
    #[attr("rdf.uri", field)]
    pub uri: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct RdfMetadataResult {
    pub metadata: RdfMetadata,
    pub warnings: FossilWarnings,
}

impl RdfMetadataResult {
    pub fn new(metadata: RdfMetadata) -> Self {
        Self {
            metadata,
            warnings: FossilWarnings::new(),
        }
    }

    pub fn with_warnings(mut self, warnings: FossilWarnings) -> Self {
        self.warnings = warnings;
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct RdfMetadata {
    pub rdf_type: Option<String>,
    pub base: Option<String>,
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
        let result = Self::from_type_metadata_with_warnings(type_metadata, interner, None);
        if result.metadata.has_metadata() {
            Some(result.metadata)
        } else {
            None
        }
    }

    /// Extract from TypeMetadata with conflict detection
    ///
    /// This version also checks for conflicts between #[rdf(type = "...")] attribute
    /// and fields that have rdf:type as their predicate URI.
    ///
    /// # Arguments
    /// * `type_metadata` - The type metadata to extract from
    /// * `interner` - Symbol interner for string resolution
    /// * `type_name` - Optional type name for warning messages
    pub fn from_type_metadata_with_warnings(
        type_metadata: &TypeMetadata,
        interner: &Interner,
        type_name: Option<&str>,
    ) -> RdfMetadataResult {
        let type_attrs = RdfTypeAttrs::from_type_metadata(type_metadata, interner);
        let mut warnings = FossilWarnings::new();

        let mut metadata = RdfMetadata {
            rdf_type: type_attrs.rdf_type.clone(),
            base: type_attrs.base.clone(),
            fields: HashMap::new(),
        };

        for (field_name, field_metadata) in &type_metadata.field_metadata {
            let field_attrs = RdfFieldAttrs::from_field_metadata(field_metadata, interner);

            if let Some(uri) = field_attrs.uri {
                // Check for rdf:type conflict
                if uri == RDF_TYPE {
                    if let Some(ref attr_type) = type_attrs.rdf_type {
                        let field_name_str = interner.resolve(*field_name);
                        let type_name_str = type_name.unwrap_or("<anonymous>");

                        warnings.push(FossilWarning::generic(
                            format!("type '{}': @rdf(type) attribute '{}' conflicts with rdf:type field '{}'", type_name_str, attr_type, field_name_str),
                            Loc::generated(),
                        ));
                    }
                }

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

        RdfMetadataResult { metadata, warnings }
    }

    pub fn set_field_type(&mut self, field: Symbol, term_type: RdfTermType) {
        if let Some(info) = self.fields.get_mut(&field) {
            info.term_type = term_type;
        }
    }

    pub fn get_field(&self, field: Symbol) -> Option<&RdfFieldInfo> {
        self.fields.get(&field)
    }

    pub fn has_metadata(&self) -> bool {
        self.rdf_type.is_some() || !self.fields.is_empty()
    }
}
