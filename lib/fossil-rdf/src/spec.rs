use std::collections::HashMap;

use polars::prelude::*;

/// Vertex type specification — lazy frame ready for parquet sink.
///
/// Generic input for `materialize_frames()`. The `frame` must produce
/// columns: `_id` (u64), `subject` (string), plus zero or more property columns.
pub struct VertexSpec {
    /// Directory/type name (e.g. `"person"`, `"Dataset"`).
    pub name: String,
    /// Full RDF type IRI. May be empty for non-RDF graphs.
    pub iri: String,
    /// Lazy frame producing `_id | subject | properties…`
    pub frame: LazyFrame,
    /// Optional column → IRI mapping for `ColumnStat.iri`.
    pub column_iris: HashMap<String, String>,
}

/// Edge type specification — lazy frame with source/target vertex IDs.
///
/// Generic input for `materialize_frames()`.
pub struct EdgeSpec {
    /// Short edge label (e.g. `"knows"`, `"dataset"`).
    pub label: String,
    /// Full predicate IRI.
    pub iri: String,
    /// Source vertex type name.
    pub source_type: String,
    /// Target vertex type name.
    pub target_type: String,
    /// Lazy frame producing `source | target` (u64 IDs).
    pub frame: LazyFrame,
}
