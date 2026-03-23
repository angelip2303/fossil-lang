use fossil_lang::common::PrimitiveType;
use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::context::{DefId, Symbol};
use fossil_lang::passes::GlobalContext;
use fossil_macros::FromAttrs;

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

#[derive(Debug, Clone, FromAttrs)]
pub struct RdfTypeAttrs {
    #[attr("rdf.type")]
    pub rdf_type: Option<String>,
    #[attr("rdf.subject")]
    pub subject: Option<String>,
}

#[derive(Debug, Clone, FromAttrs)]
pub struct RdfFieldAttrs {
    #[attr("rdf.uri", field)]
    pub uri: Option<String>,
}

impl RdfTypeAttrs {
    pub fn from_def_id(def_id: DefId, gcx: &GlobalContext) -> Option<Self> {
        let metadata = gcx.type_metadata.get(&def_id)?;
        Some(Self::from_type_metadata(metadata, &gcx.interner))
    }
}

/// Get the primitive type for a field from the registered types.
pub fn field_primitive_type(
    def_id: DefId,
    field: Symbol,
    gcx: &GlobalContext,
) -> Option<PrimitiveType> {
    gcx.registered_types
        .get(&def_id)?
        .iter()
        .find(|(sym, _)| *sym == field)
        .map(|(_, ft)| match ft {
            BuiltInFieldType::Required(p) | BuiltInFieldType::Optional(p) => *p,
        })
}
