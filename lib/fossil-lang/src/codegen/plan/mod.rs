//! FossilPlan — the serializable output of the Fossil compiler.
//!
//! The plan is the boundary between compiler (pure) and host (executes).
//! It contains three phases:
//!
//! 1. **`sources`** — name-based source manifest. The host must register
//!    each source alias in its catalog (DuckDB view, preprocessed temp
//!    table, registered DataFrame, …) before running the SQL. fossil-lang
//!    has no opinion on how: the decision lives entirely with the host,
//!    which knows its execution environment.
//! 2. **`sql`** — a single SQL query with CTEs that references sources
//!    purely by alias. Never contains format-specific reads like
//!    `read_csv(...)`.
//! 3. **`outputs`** — RDF/GraphAr materialization instructions, applied
//!    to the SQL result by the host after execution.
//!
//! Reference: dbt compiled models; DataFusion `LogicalPlan`; PRQL compiled
//! output. They all separate "what to read" (catalog/manifest) from "what
//! to compute" (SQL) from "how to write" (outputs).

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::rq::RelationalQuery;

/// The complete execution plan produced by the compiler.
///
/// Serializable as JSON (the `rq` field is skipped because sqlparser AST
/// nodes are not `Serialize`). Hosts that need the raw relational query
/// for introspection should hold the `RelationalQuery` alongside the
/// plan rather than inside it.
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct FossilPlan {
    /// Phase 1: named source manifest. Host registers each alias in its
    /// catalog before executing `sql`.
    pub sources: Vec<SourceDef>,
    /// Phase 2: single SQL query with CTEs.
    pub sql: String,
    /// Phase 3: write results to external formats after SQL execution.
    pub outputs: Vec<OutputDef>,
    /// The relational query for introspection (DCAT, debugging).
    /// Skipped during (de)serialization — see struct doc.
    #[serde(skip)]
    pub rq: RelationalQuery,
}

/// One entry in the source manifest.
///
/// Tells the host: "before running `sql`, make sure a table named `alias`
/// exists in the catalog, populated from the source described by `format`
/// and `path` (plus `params`)." How is entirely the host's call.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceDef {
    /// Alias used by the SQL query (e.g. `src_csv_1`).
    pub alias: String,
    /// Source format as declared in fossil (e.g. `csv`, `parquet`, `pdf`).
    pub format: String,
    /// Source path or URI.
    pub path: String,
    /// Parameters as passed in the fossil source call (e.g. `delim=","`).
    pub params: HashMap<String, String>,
}

/// Output to materialize after the main SQL query.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputDef {
    pub format: String,
    pub path: String,
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

/// Maps an RDF field to a SQL expression. Types come from the DuckDB result
/// set at runtime — fossil-lang does not duplicate them in the plan.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldMapping {
    pub field_name: String,
    pub sql_expr: String,
}

/// Result of executing an output step.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputResult {
    pub format: String,
    pub path: String,
}

impl FossilPlan {
    /// Create a plan from a [`RelationalQuery`]. Pure projection — no host
    /// knowledge, no dialect. The source manifest and outputs flow straight
    /// from the lowered RQ; the SQL is emitted by stringifying the
    /// sqlparser AST.
    pub fn from_rq(rq: RelationalQuery) -> Self {
        let sql = rq.to_query().to_string();

        let sources = rq
            .sources
            .iter()
            .map(|s| SourceDef {
                alias: s.alias.value.clone(),
                format: s.format.clone(),
                path: s.path.clone(),
                params: s.params.clone(),
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
                        subject_template: e.subject_template.to_string(),
                        fields: e
                            .fields
                            .iter()
                            .map(|(name, col)| FieldMapping {
                                field_name: name.clone(),
                                sql_expr: col.value.clone(),
                            })
                            .collect(),
                        identity_columns: e
                            .identity_columns
                            .iter()
                            .map(|c| c.value.clone())
                            .collect(),
                    })
                    .collect();

                OutputDef {
                    format: o.format.clone(),
                    path: o.path.clone(),
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
