use std::collections::HashMap;

use crate::{ast::*, error::Result, runtime::Value, solver::Context};

pub struct Interpreter {
    context: Context,
    env: HashMap<NodeId, Value>,
}

impl Interpreter {
    pub fn new(context: Context) -> Self {
        Self {
            context,
            env: HashMap::new(),
        }
    }

    pub fn eval_stmt(&mut self, stmt_id: NodeId) -> Result<Value> {
        let ast = self.context.ast.clone();
        let stmt = ast.get_stmt(stmt_id); // TODO: we should remove the clone here

        match &stmt.kind {
            StmtKind::Let { name, value } => {
                let val = self.eval_expr(*value)?;
                self.env.insert(*name, val.clone());
                Ok(val)
            }

            StmtKind::Expr(expr) => self.eval_expr(*expr),

            _ => Ok(Value::Unit),
        }
    }

    pub fn eval_expr(&mut self, expr_id: NodeId) -> Result<Value> {
        let ast = self.context.ast.clone();
        let expr = ast.get_expr(expr_id);

        match &expr.kind {
            ExprKind::Unit => Ok(Value::Unit),
            ExprKind::Integer(n) => Ok(Value::Int(*n)),
            ExprKind::String(s) => Ok(Value::String(s.clone())),
            ExprKind::Boolean(b) => Ok(Value::Bool(*b)),

            ExprKind::Identifier(name) => self
                .env
                .get(name)
                .cloned()
                .ok_or_else(|| todo!("unbound variable")),

            ExprKind::Path(segments) => {
                let segments: Vec<&str> = segments
                    .iter()
                    .map(|id| self.context.ast.get_name(*id))
                    .collect();

                match self.context.globals.modules.resolve(&segments) {
                    Some(crate::module::Lookup::Function(func)) => {
                        Ok(Value::Function(func.clone()))
                    }
                    _ => {
                        let path = segments.join(".");
                        todo!("Unresolved path: {}", path)
                    }
                }
            }

            ExprKind::Call { callee, arg } => {
                let arg_val = self.eval_expr(*arg)?;
                let callee_val = self.eval_expr(*callee)?;

                match callee_val {
                    Value::Function(func) => func.call(vec![arg_val]),
                    _ => todo!("not callable"),
                }
            }

            _ => todo!(),
        }
    }
}
