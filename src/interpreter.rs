use std::sync::Arc;

use indexmap::IndexMap;

use crate::runtime::value::Function;
use crate::runtime::value::RecordRepr;
use crate::runtime::value::UserFunction;
use crate::{
    ast::*,
    error::{CompileError, Result},
    runtime::Value,
    solver::Context,
};

pub struct Interpreter {
    context: Context,
    env: IndexMap<NodeId, Value>,
}

impl Interpreter {
    pub fn new(context: Context) -> Self {
        Self {
            context,
            env: IndexMap::new(),
        }
    }

    pub fn eval_stmt(&mut self, stmt_id: NodeId) -> Result<Value> {
        let ast = self.context.ast.clone();
        let stmt = ast.get_stmt(stmt_id);

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
            ExprKind::String(s) => Ok(Value::String(s.as_str().into())),
            ExprKind::Boolean(b) => Ok(Value::Bool(*b)),

            ExprKind::Identifier(name) => self.env.get(name).cloned().ok_or_else(|| {
                let name_str = self.context.ast.get_name(*name);
                CompileError::UnboundVariable(name_str.to_string()) // TODO: remove to_string here
            }),

            ExprKind::Path(segments) => {
                let segments: Vec<&str> = segments
                    .iter()
                    .map(|id| self.context.ast.get_name(*id))
                    .collect();

                match self.context.globals.modules.resolve(&segments) {
                    Some(crate::module::Lookup::Function(func)) => {
                        Ok(Value::Function(Function::Native(Arc::from(func))))
                    }
                    _ => Err(CompileError::UnboundVariable(segments.join("."))),
                }
            }

            ExprKind::Function { param, body } => {
                // TODO: think about this on free variables
                let free_vars = self.find_free_vars(*body, param.name);
                let mut captured = IndexMap::new();

                for var in free_vars {
                    if let Some(value) = self.env.get(&var) {
                        captured.insert(var, value.clone());
                    }
                }

                Ok(Value::Function(Function::User(UserFunction {
                    param: param.name,
                    body: *body,
                    env: captured,
                })))
            }

            ExprKind::Call { callee, arg } => {
                let func = self.eval_expr(*callee)?;
                let arg_val = self.eval_expr(*arg)?;
                self.apply_function(func, arg_val)
            }

            ExprKind::Record(fields) => {
                let mut map = IndexMap::new();
                for (name_id, value_id) in fields {
                    let ast = self.context.ast.clone();
                    let name = ast.get_name(*name_id);
                    let value = self.eval_expr(*value_id)?;
                    map.insert(Arc::from(name), value);
                }
                Ok(Value::Record(RecordRepr::Owned(map)))
            }

            ExprKind::List(items) => {
                let values: Result<Vec<_>> = items.iter().map(|id| self.eval_expr(*id)).collect();
                Value::from_vec(values?)
            }

            ExprKind::MemberAccess { object, field } => {
                let obj = self.eval_expr(*object)?;
                let field_name = self.context.ast.get_name(*field);
                obj.get_field(field_name)
            }

            ExprKind::Pipe { left, right } => {
                let left_val = self.eval_expr(*left)?;
                let func = self.eval_expr(*right)?;
                self.apply_function(func, left_val)
            }

            _ => Ok(Value::Unit),
        }
    }

    fn apply_function(&mut self, func: Value, arg: Value) -> Result<Value> {
        match func {
            Value::Function(f) => match f {
                Function::Native(native) => {
                    return native.call(vec![arg]);
                }
                Function::User(uf) => {
                    let saved_env = std::mem::replace(&mut self.env, uf.env);
                    self.env.insert(uf.param, arg);
                    let result = self.eval_expr(uf.body)?;
                    self.env = saved_env;

                    return Ok(result);
                }
            },
            _ => Err(CompileError::ExpectedFunction(format!("{:?}", func))),
        }
    }

    // TODO: I don't really get this
    fn find_free_vars(&self, expr_id: NodeId, bound: NodeId) -> Vec<NodeId> {
        let expr = self.context.ast.get_expr(expr_id);
        let mut free = Vec::new();

        match &expr.kind {
            ExprKind::Identifier(id) => {
                if *id != bound && self.env.contains_key(id) {
                    free.push(*id);
                }
            }

            ExprKind::BinaryOp { left, right, .. } => {
                free.extend(self.find_free_vars(*left, bound));
                free.extend(self.find_free_vars(*right, bound));
            }

            ExprKind::Pipe { left, right } => {
                free.extend(self.find_free_vars(*left, bound));
                free.extend(self.find_free_vars(*right, bound));
            }

            ExprKind::Call { callee, arg } => {
                free.extend(self.find_free_vars(*callee, bound));
                free.extend(self.find_free_vars(*arg, bound));
            }

            ExprKind::MemberAccess { object, .. } => {
                free.extend(self.find_free_vars(*object, bound));
            }

            ExprKind::List(items) => {
                for item in items {
                    free.extend(self.find_free_vars(*item, bound));
                }
            }

            ExprKind::Record(fields) => {
                for (_, value) in fields {
                    free.extend(self.find_free_vars(*value, bound));
                }
            }

            _ => {}
        }

        free.sort();
        free.dedup();
        free
    }
}
