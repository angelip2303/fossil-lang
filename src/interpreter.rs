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
                // TODO: this should not be necessary, we should favour everything as a function
                // let path_str = segments
                //     .iter()
                //     .map(|id| self.context.ast.get_name(*id))
                //     .collect::<Vec<_>>()
                //     .join(".");

                // if let Some(load_info) = self.context.globals.generated_loads.get(&path_str) {
                //     // Retornar un closure que ejecutará el load
                //     return Ok(Value::GeneratedLoad(load_info.clone()));
                // }

                // Buscar función normal
                let segments_str: Vec<&str> = segments
                    .iter()
                    .map(|id| self.context.ast.get_name(*id))
                    .collect();

                match self.context.globals.modules.resolve(&segments_str) {
                    Some(crate::module::Lookup::Function(func)) => {
                        Ok(Value::Function(func.clone()))
                    }
                    _ => todo!("unresolved path"),
                }
            }

            ExprKind::Call { callee, arg } => {
                let arg_val = self.eval_expr(*arg)?;
                let callee_val = self.eval_expr(*callee)?;

                match callee_val {
                    Value::Function(func) => func.call(vec![arg_val]),

                    // TODO: generated load should not be necessary, and should be a function itself
                    // Value::GeneratedLoad(load_info) => {
                    //     let path = match arg_val {
                    //         Value::String(s) => s,
                    //         _ => todo!("expected string"),
                    //     };

                    //     // Obtener el provider
                    //     let provider_segments: Vec<&str> =
                    //         load_info.provider_name.split('.').collect();

                    //     let provider =
                    //         match self.context.globals.modules.resolve(&provider_segments) {
                    //             Some(crate::module::Lookup::Provider(p)) => p,
                    //             Some(crate::module::Lookup::Module(m)) => m
                    //                 .providers
                    //                 .get(m.name.as_str())
                    //                 .ok_or_else(|| todo!("provider not found"))?
                    //                 .clone(),
                    //             _ => todo!("provider not found"),
                    //         };

                    //     // Ejecutar load
                    //     let df = provider.load(&path)?;
                    //     Ok(Value::DataFrame(df))
                    // }
                    _ => todo!("not callable"),
                }
            }

            _ => todo!(),
        }
    }
}
