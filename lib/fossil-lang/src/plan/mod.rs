//! FossilPlan — the serializable output of the Fossil compiler.
//!
//! The plan is the boundary between compiler (pure) and host (executes).
//! It contains 3 phases: sources → SQL → outputs.
//!
//! Reference: dbt compiled models, DataFusion LogicalPlan.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::dialect::{ScanStrategy, SqlDialect};
use crate::rq::RelationalQuery;

/// The complete execution plan produced by the compiler.
///
/// Serializable as JSON. The host (keasy) deserializes and executes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FossilPlan {
    /// Phase 1: load external data into DuckDB (before SQL).
    pub sources: Vec<SourceDef>,
    /// Phase 2: single SQL query with CTEs.
    pub sql: String,
    /// Phase 3: write results to external formats (after SQL).
    pub outputs: Vec<OutputDef>,
    /// The relational query for introspection (DCAT, debugging).
    pub rq: RelationalQuery,
}

/// Source data to load before the main SQL query.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceDef {
    /// Handler name (e.g. "pdf_extract", "docx_extract").
    pub handler: String,
    /// Path to the source file.
    pub path: String,
    /// Temp table name in DuckDB.
    pub output_table: String,
    /// Expected schema: (column_name, type_name).
    pub schema: Vec<(String, String)>,
    /// Additional parameters.
    pub params: HashMap<String, String>,
}

/// Output to materialize after the main SQL query.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputDef {
    /// Output format handler (e.g. "graphar", "turtle").
    pub format: String,
    /// Output path.
    pub path: String,
    /// Tables to read from DuckDB.
    pub input_tables: Vec<String>,
    /// Entity projections with field mappings.
    pub projections: Vec<EntityProjection>,
}

/// Maps a SQL table to an RDF entity type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EntityProjection {
    /// RDF type name (e.g. "Person").
    pub type_name: String,
    /// SQL expression for the subject IRI.
    pub subject_template: String,
    /// Field mappings: (rdf_field_name, sql_expression).
    pub fields: Vec<FieldMapping>,
    /// Constructor key columns (identity).
    pub identity_columns: Vec<String>,
}

/// Maps an RDF field to a SQL expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldMapping {
    pub field_name: String,
    pub sql_expr: String,
    pub data_type: String,
}

/// Result of executing an output step.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputResult {
    pub format: String,
    pub path: String,
}

impl FossilPlan {
    /// Create a plan from a RelationalQuery and a SqlDialect.
    /// The dialect decides which sources need preprocessing.
    pub fn from_rq(rq: RelationalQuery, dialect: &dyn SqlDialect) -> Self {
        use crate::rq::emit_sql::rq_to_sql;
        let sql = rq_to_sql(&rq, dialect);

        let sources = rq
            .transforms
            .iter()
            .filter_map(|t| {
                if let crate::rq::Transform::Scan { source, .. } = t {
                    match dialect.scan_strategy(source) {
                        ScanStrategy::Preprocess { handler, output_table } => Some(SourceDef {
                            handler,
                            path: source.path.clone(),
                            output_table,
                            schema: vec![],
                            params: source.params.clone(),
                        }),
                        ScanStrategy::Sql(_) => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        let outputs = rq
            .outputs
            .iter()
            .map(|o| {
                let projections = o
                    .emissions
                    .iter()
                    .filter_map(|&idx| rq.emissions.get(idx))
                    .map(|e| EntityProjection {
                        type_name: e.type_name.clone(),
                        subject_template: crate::rq::emit_sql::expr_to_sql(
                            &e.subject_template,
                            &rq,
                        ),
                        fields: e
                            .fields
                            .iter()
                            .map(|(name, col)| FieldMapping {
                                field_name: name.clone(),
                                sql_expr: rq.col_name(*col).to_string(),
                                data_type: "unknown".into(), // type not yet propagated
                            })
                            .collect(),
                        identity_columns: e
                            .identity_columns
                            .iter()
                            .map(|c| rq.col_name(*c).to_string())
                            .collect(),
                    })
                    .collect();

                OutputDef {
                    format: o.format.clone(),
                    path: o.path.clone(),
                    input_tables: Vec::new(), // filled by SQL CTE names
                    projections,
                }
            })
            .collect();

        Self {
            sources,
            sql,
            outputs,
            rq,
        }
    }
}
