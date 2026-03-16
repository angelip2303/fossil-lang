use std::sync::{Arc, Mutex};

use serde::{Deserialize, Serialize};

use crate::error::FossilError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::value::{Environment, Value};

/// Per-predicate statistics produced by RDF materialization.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PredicateStat {
    pub predicate: String,
    pub count: u64,
    pub n_unique: u64,
    pub min: Option<String>,
    pub max: Option<String>,
    /// XSD IRI (e.g. `http://www.w3.org/2001/XMLSchema#integer`) or `"uri"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub datatype: Option<String>,
    /// 3-5 representative values for this predicate.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub samples: Vec<String>,
}

/// Manifest describing all files and stats produced by RDF materialization.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfManifest {
    pub subjects: String,
    pub objects: String,
    pub types: String,
    pub predicates: Vec<String>,
    pub meta: Vec<PredicateStat>,
    /// Total unique subjects (entities) across all triples.
    #[serde(default)]
    pub entity_count: u64,
}

/// Describes a single output produced during script execution.
#[derive(Debug, Clone)]
pub struct OutputRecord {
    pub kind: OutputKind,
    pub path: String,
    pub manifest: Option<RdfManifest>,
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
