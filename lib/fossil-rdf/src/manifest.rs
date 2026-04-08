use serde::{Deserialize, Serialize};

/// Per-column statistics for a vertex property.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct ColumnStat {
    /// Short label used as Parquet column name.
    pub name: String,
    /// Full predicate IRI (empty if not RDF).
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub iri: String,
    /// Polars-level data type name (e.g. "string", "int64", "double").
    pub datatype: String,
    pub count: u64,
    pub n_unique: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub min: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub samples: Vec<String>,
}

/// Manifest for a vertex type (one Parquet file per type).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct TypeManifest {
    /// Short type name (e.g. "person").
    pub name: String,
    /// Full RDF type IRI.
    pub iri: String,
    /// Relative path to vertex Parquet file.
    pub vertex_file: String,
    pub entity_count: u64,
    pub columns: Vec<ColumnStat>,
}

/// Manifest for an edge type (GraphAr v1 ordered adjacency lists).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct EdgeManifest {
    /// Short edge label (e.g. "knows").
    pub name: String,
    /// Full predicate IRI.
    pub iri: String,
    /// Source vertex type name.
    pub source_type: String,
    /// Target vertex type name.
    pub target_type: String,
    /// CSR-ordered edge parquet (ordered by source vertex).
    pub by_source: String,
    /// CSC-ordered edge parquet (ordered by target vertex).
    pub by_target: String,
    pub count: u64,
}

/// GraphAr-compatible manifest describing a property graph stored as Parquet files.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct DataManifest {
    pub types: Vec<TypeManifest>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub edges: Vec<EdgeManifest>,
}
