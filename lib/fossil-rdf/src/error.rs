use thiserror::Error;

#[derive(Debug, Error)]
pub enum RdfGraphError {
    #[error("Dataset not found: {0}")]
    NotFound(String),

    #[error("Missing index: {0}")]
    MissingIndex(String),

    #[error("Polars error: {0}")]
    Polars(#[from] polars::prelude::PolarsError),

    #[error("Unsupported SPARQL feature: {0}")]
    UnsupportedSparql(String),

    #[error("SPARQL parse error: {0}")]
    SparqlParse(String),

    #[error("{0}")]
    Other(String),
}
