//! Shared utilities for shape-based providers (ShEx, SHACL)
//!
//! This module contains common functionality used by both ShEx and SHACL providers
//! for extracting type information from RDF shape languages.

use fossil_lang::ast::ast::PrimitiveType;

/// Field extracted from a shape definition
#[derive(Debug)]
pub struct ShapeField {
    /// Field name (derived from predicate local name)
    pub name: String,
    /// Full predicate IRI
    pub predicate_uri: String,
    /// Fossil primitive type
    pub fossil_type: PrimitiveType,
    /// Whether the field is optional
    pub optional: bool,
    /// Whether the field is a list
    pub is_list: bool,
}

/// Extract local name from IRI (last segment after # or /)
///
/// # Examples
/// - `<http://xmlns.com/foaf/0.1/name>` -> `"name"`
/// - `<http://example.org/Person>` -> `"Person"`
pub fn extract_local_name(iri: &str) -> String {
    // Remove angle brackets if present
    let iri = iri.trim_start_matches('<').trim_end_matches('>');

    // Try fragment first
    if let Some(pos) = iri.rfind('#') {
        return iri[pos + 1..].to_string();
    }

    // Then try last path segment
    if let Some(pos) = iri.rfind('/') {
        return iri[pos + 1..].to_string();
    }

    iri.to_string()
}

/// Map XSD datatype IRI to Fossil primitive type
///
/// Handles common XSD datatypes and maps them to appropriate Fossil types.
pub fn xsd_to_fossil_type(iri: &str) -> PrimitiveType {
    // Extract local name from IRI for matching
    let local = iri
        .rsplit_once('#')
        .map(|(_, name)| name)
        .or_else(|| iri.rsplit_once('/').map(|(_, name)| name))
        .unwrap_or(iri)
        .trim_end_matches('>');

    match local {
        // String types
        "string" | "normalizedString" | "token" | "language" | "Name" | "NCName" | "NMTOKEN"
        | "anyURI" | "QName" | "NOTATION" | "ID" | "IDREF" | "ENTITY" => PrimitiveType::String,

        // Integer types
        "integer" | "int" | "long" | "short" | "byte" | "nonNegativeInteger"
        | "nonPositiveInteger" | "negativeInteger" | "positiveInteger" | "unsignedLong"
        | "unsignedInt" | "unsignedShort" | "unsignedByte" => PrimitiveType::Int,

        // Float types
        "float" | "double" | "decimal" => PrimitiveType::Float,

        // Boolean type
        "boolean" => PrimitiveType::Bool,

        // Date/time types -> String for now (future: native date types)
        "date" | "time" | "dateTime" | "dateTimeStamp" | "gYear" | "gMonth" | "gDay"
        | "gYearMonth" | "gMonthDay" | "duration" | "yearMonthDuration" | "dayTimeDuration" => {
            PrimitiveType::String
        }

        // Binary types -> String (base64 encoded)
        "hexBinary" | "base64Binary" => PrimitiveType::String,

        // Default to string for unknown types
        _ => PrimitiveType::String,
    }
}
