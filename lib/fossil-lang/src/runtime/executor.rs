use std::sync::Arc;

use crate::error::FossilError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::output::{LocalOutputResolver, OutputResolver};
use crate::runtime::value::{Environment, Value};

pub struct ExecutionConfig {
    pub output_resolver: Arc<dyn OutputResolver>,
}

impl Default for ExecutionConfig {
    fn default() -> Self {
        Self {
            output_resolver: Arc::new(LocalOutputResolver),
        }
    }
}

pub struct IrExecutor;

impl IrExecutor {
    pub fn execute(program: IrProgram) -> Result<Vec<Value>, FossilError> {
        Self::execute_with_config(program, ExecutionConfig::default())
    }

    pub fn execute_with_config(
        program: IrProgram,
        config: ExecutionConfig,
    ) -> Result<Vec<Value>, FossilError> {
        let IrProgram { ir, gcx, type_index, resolutions, typeck_results } = program;
        let mut evaluator = IrEvaluator::new(
            &ir,
            &gcx,
            &type_index,
            &resolutions,
            &typeck_results,
            Environment::default(),
            config.output_resolver,
        );
        let mut results = Vec::new();

        for &stmt_id in &ir.root {
            match &ir.stmts.get(stmt_id).kind {
                StmtKind::Let { name, value, .. } => {
                    let val = evaluator.eval(*value)?;
                    evaluator.bind(*name, val.clone());
                    results.push(val);
                }

                StmtKind::Expr(expr_id) => {
                    let val = evaluator.eval(*expr_id)?;
                    results.push(val);
                }

                StmtKind::Type { .. } => {}
            }
        }

        Ok(results)
    }
}
