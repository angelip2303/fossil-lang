use thiserror::Error;

#[derive(Debug, Error)]
pub enum RdfError {
    #[error("Rdf.materialize requires (emission, path) arguments")]
    MissingArguments,

    #[error("path argument must be a string literal")]
    InvalidPath,

    #[error("first argument must be an Emission with type specs")]
    ExpectedEmission,

    #[error("failed to write RDF: {0}")]
    Write(String),

    #[error("vertex dedup failed for type '{type_dir}': {reason}")]
    VertexDedupFailed { type_dir: String, reason: String },

    #[error("edge join failed for '{edge}': {reason}")]
    EdgeJoinFailed { edge: String, reason: String },

    #[error("duckdb: {0}")]
    DuckDb(String),
}

impl From<duckdb::Error> for RdfError {
    fn from(e: duckdb::Error) -> Self {
        RdfError::DuckDb(e.to_string())
    }
}
