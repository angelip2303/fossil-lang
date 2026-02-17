use std::collections::HashMap;

use fossil_lang::ast::Loc;
use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{Interner, Symbol, TypeMetadata};
use fossil_lang::error::{FossilWarning, FossilWarnings};
use fossil_macros::FromAttrs;

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// Map a Fossil PrimitiveType to its canonical XSD datatype IRI.
///
/// Returns `None` for `String` since RDF simple literals are implicitly `xsd:string`.
pub fn primitive_to_xsd(prim: PrimitiveType) -> Option<&'static str> {
    match prim {
        PrimitiveType::Int => Some("http://www.w3.org/2001/XMLSchema#integer"),
        PrimitiveType::Float => Some("http://www.w3.org/2001/XMLSchema#double"),
        PrimitiveType::Bool => Some("http://www.w3.org/2001/XMLSchema#boolean"),
        PrimitiveType::String => None,
    }
}

#[derive(Debug, Clone)]
pub struct RdfFieldInfo {
    pub uri: String,
    /// XSD datatype IRI for typed literals (e.g. `xsd:integer`).
    /// `None` means the value is serialized as a simple literal.
    pub xsd_datatype: Option<String>,
    /// The Fossil primitive type for this field, used for Polars cast coercion.
    pub primitive_type: Option<PrimitiveType>,
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
    /// Extract from TypeMetadata
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

                metadata.fields.insert(
                    *field_name,
                    RdfFieldInfo { uri, xsd_datatype: None, primitive_type: None },
                );
            }
        }

        RdfMetadataResult { metadata, warnings }
    }

    pub fn has_metadata(&self) -> bool {
        self.rdf_type.is_some() || !self.fields.is_empty()
    }

    /// Resolve XSD datatype annotations for all fields using Fossil's type information.
    ///
    /// Each field's `xsd_datatype` is set to the canonical XSD IRI for its `PrimitiveType`.
    /// Fields whose symbol isn't in `field_types` (or whose primitive maps to `None`,
    /// like `String`) are left as simple literals.
    pub fn resolve_xsd_types(&mut self, field_types: &HashMap<Symbol, PrimitiveType>) {
        for (sym, field_info) in &mut self.fields {
            if let Some(prim) = field_types.get(sym) {
                field_info.xsd_datatype = primitive_to_xsd(*prim).map(|s| s.to_string());
                field_info.primitive_type = Some(*prim);
            }
        }
    }

    /// Build a map from predicate URI → XSD datatype IRI for all fields
    /// that have a typed literal annotation.
    ///
    /// Intended to be called once after `resolve_xsd_types` and passed
    /// to the batch serializer.
    pub fn xsd_type_map(&self) -> HashMap<String, String> {
        self.fields
            .values()
            .filter_map(|f| {
                f.xsd_datatype
                    .as_ref()
                    .map(|xsd| (f.uri.clone(), xsd.clone()))
            })
            .collect()
    }
}
