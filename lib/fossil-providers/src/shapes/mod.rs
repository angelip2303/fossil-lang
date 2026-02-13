use fossil_lang::ast::PrimitiveType;
use fossil_lang::traits::provider::FieldType;

pub mod shex;

#[derive(Debug)]
pub struct ShapeField {
    pub name: String,
    pub predicate_uri: String,
    pub ty: FieldType,
    pub validate_args: Vec<(String, ValidateValue)>,
}

#[derive(Debug)]
pub enum ValidateValue {
    Int(i64),
    Str(String),
}

pub fn extract_local_name(iri: &str) -> String {
    let iri = iri.trim_start_matches('<').trim_end_matches('>');
    if let Some(pos) = iri.rfind('#') {
        return iri[pos + 1..].to_string();
    }
    if let Some(pos) = iri.rfind('/') {
        return iri[pos + 1..].to_string();
    }
    iri.to_string()
}

pub fn xsd_to_fossil_type(iri: &str) -> PrimitiveType {
    let local = iri
        .rsplit_once('#')
        .map(|(_, name)| name)
        .or_else(|| iri.rsplit_once('/').map(|(_, name)| name))
        .unwrap_or(iri)
        .trim_end_matches('>');

    match local {
        "string" | "normalizedString" | "token" | "language" | "Name" | "NCName" | "NMTOKEN"
        | "anyURI" | "QName" | "NOTATION" | "ID" | "IDREF" | "ENTITY" => PrimitiveType::String,

        "integer" | "int" | "long" | "short" | "byte" | "nonNegativeInteger"
        | "nonPositiveInteger" | "negativeInteger" | "positiveInteger" | "unsignedLong"
        | "unsignedInt" | "unsignedShort" | "unsignedByte" => PrimitiveType::Int,

        "float" | "double" | "decimal" => PrimitiveType::Float,

        "boolean" => PrimitiveType::Bool,

        "date" | "time" | "dateTime" | "dateTimeStamp" | "gYear" | "gMonth" | "gDay"
        | "gYearMonth" | "gMonthDay" | "duration" | "yearMonthDuration" | "dayTimeDuration" => {
            PrimitiveType::String
        }

        "hexBinary" | "base64Binary" => PrimitiveType::String,

        _ => PrimitiveType::String,
    }
}
