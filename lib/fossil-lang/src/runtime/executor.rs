use crate::error::FossilError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::value::{Environment, Value};

pub struct IrExecutor;

impl IrExecutor {
    pub fn execute(program: IrProgram) -> Result<Vec<Value>, FossilError> {
        let IrProgram { ir, gcx, .. } = program;
        let mut evaluator = IrEvaluator::new(&ir, &gcx, Environment::default());
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
