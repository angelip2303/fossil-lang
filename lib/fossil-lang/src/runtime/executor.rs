use std::sync::{Arc, Mutex};

use serde::{Deserialize, Serialize};

use crate::error::FossilError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::value::{Environment, Value};

/// Per-column statistics for a vertex property.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct ColumnStat {
    /// Short label used as Parquet column name.
    pub name: String,
    /// Full predicate IRI.
    pub iri: String,
    /// Data type: "string", "int64", "double", "boolean", "date".
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

/// Describes a single output produced during script execution.
#[derive(Debug, Clone)]
pub struct OutputRecord {
    pub kind: OutputKind,
    pub path: String,
    pub manifest: Option<DataManifest>,
}

/// The type of output produced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputKind {
    RdfParquet,
}

/// Result of executing a fossil script.
pub struct ExecutionResult {
    pub values: Vec<Value>,
    pub outputs: Vec<OutputRecord>,
}

pub struct IrExecutor;

impl IrExecutor {
    pub fn execute(program: IrProgram) -> Result<ExecutionResult, FossilError> {
        let outputs: Arc<Mutex<Vec<OutputRecord>>> = Arc::new(Mutex::new(Vec::new()));

        let IrProgram { ir, gcx, type_index, resolutions, typeck_results } = program;
        let mut evaluator = IrEvaluator::new(
            &ir,
            &gcx,
            &type_index,
            &resolutions,
            &typeck_results,
            Environment::default(),
            outputs.clone(),
        );
        let mut values = Vec::new();

        for &stmt_id in &ir.root {
            match &ir.stmts.get(stmt_id).kind {
                StmtKind::Let { name, value, .. } => {
                    let val = evaluator.eval(*value)?;
                    evaluator.bind(*name, val.clone());
                    values.push(val);
                }

                StmtKind::Expr(expr_id) => {
                    let val = evaluator.eval(*expr_id)?;
                    values.push(val);
                }

                StmtKind::Type { .. } => {}
            }
        }

        let outputs = match Arc::try_unwrap(outputs) {
            Ok(mutex) => mutex.into_inner().unwrap_or_default(),
            Err(arc) => arc.lock().expect("outputs lock poisoned").clone(),
        };

        Ok(ExecutionResult { values, outputs })
    }
}
