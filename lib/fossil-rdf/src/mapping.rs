use std::collections::HashMap;

use polars::prelude::*;

/// Describes how a type maps to a graph vertex.
pub struct VertexMapping {
    /// Polars selection expressions producing `_subject` and scalar property columns.
    pub selection: Vec<Expr>,
    /// The expression that produces the subject IRI.
    pub subject_expr: Expr,
    /// Short column label → full predicate IRI.
    pub label_to_iri: HashMap<String, String>,
    /// Edge mappings from this vertex type to other vertex types.
    pub edges: Vec<EdgeMapping>,
    /// Directory name for this type (e.g., `"wall"`).
    pub type_dir: String,
    /// Full RDF type IRI (e.g., `"http://example.com/bim#Wall"`).
    pub type_iri: String,
}

/// Describes how a reference between Fossil types maps to a graph edge.
pub struct EdgeMapping {
    /// Full predicate IRI (e.g. `"http://schema.org/knows"`).
    pub predicate_uri: String,
    /// Short label for the edge (local name of predicate IRI).
    pub label: String,
    /// Target vertex type directory name (e.g. `"person"`).
    pub target_type_dir: String,
    /// Polars expression that produces the target subject IRI.
    pub expr: Expr,
}
