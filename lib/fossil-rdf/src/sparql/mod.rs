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

    /// Translate a SPARQL SELECT query into a lazy Polars plan.
    pub fn select(&self, sparql: &str) -> Result<LazyFrame, RdfGraphError> {
        let query = SparqlParser::new()
            .parse_query(sparql)
            .map_err(|e| RdfGraphError::SparqlParse(e.to_string()))?;

        match query {
            Query::Select { pattern, .. } => {
                translator::translate_pattern(self.graph, &pattern)
            }
            _ => Err(RdfGraphError::UnsupportedSparql(
                "Only SELECT queries are supported".into(),
            )),
        }
    }
}
