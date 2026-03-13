use std::sync::{Arc, Mutex};

use crate::error::FossilError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::value::{Environment, Value};

/// Describes a single output produced during script execution.
#[derive(Debug, Clone)]
pub struct OutputRecord {
    pub kind: OutputKind,
    pub path: String,
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
