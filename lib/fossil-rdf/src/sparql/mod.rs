mod expr;
mod translator;

use polars::prelude::*;
use spargebra::{Query, SparqlParser};

use crate::error::RdfGraphError;
use crate::graph::RdfGraph;

/// SPARQL-to-Polars query engine.
///
/// Translates a subset of SPARQL SELECT queries into Polars lazy operations,
/// executed against an `RdfGraph`'s Parquet indices.
pub struct SparqlEngine<'a> {
    graph: &'a RdfGraph,
}

impl<'a> SparqlEngine<'a> {
    pub fn new(graph: &'a RdfGraph) -> Self {
        Self { graph }
    }

    /// Execute a SPARQL SELECT query, returning a collected DataFrame.
    pub fn select(&self, sparql: &str) -> Result<DataFrame, RdfGraphError> {
        let query = SparqlParser::new()
            .parse_query(sparql)
            .map_err(|e| RdfGraphError::SparqlParse(e.to_string()))?;

        match query {
            Query::Select { pattern, .. } => {
                let lf = translator::translate_pattern(self.graph, &pattern)?;
                lf.collect().map_err(Into::into)
            }
            _ => Err(RdfGraphError::UnsupportedSparql(
                "Only SELECT queries are supported".into(),
            )),
        }
    }
}
