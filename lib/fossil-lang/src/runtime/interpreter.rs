use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::{IntoLazy, UnionArgs, concat};
use polars::series::Series;

use crate::ast::*;
use crate::context::{Interner, Symbol};
use crate::error::RuntimeError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingRef, ResolutionTable, TypedProgram};
use crate::runtime::value::{Environment, Value};

pub struct Interpreter<'a> {
    registry: &'a ModuleRegistry,
    env: Environment,
}

impl<'a> Interpreter<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            env: Default::default(),
        }
    }

    pub fn execute(&mut self, program: TypedProgram) -> Result<Vec<Value>, RuntimeError> {
        let TypedProgram {
            ast,
            symbols,
            resolution,
            ..
        } = program;

        let mut results = Vec::new();

        for (_, decl) in ast.decls.iter() {
            match decl {
                Decl::Let { name, value } => {
                    let val = self.eval(*value, &ast, &symbols, &resolution)?;
                    self.env.bind(*name, val.clone());
                    results.push(val);
                }

                Decl::Expr(expr_id) => {
                    let val = self.eval(*expr_id, &ast, &symbols, &resolution)?;
                    results.push(val);
                }

                Decl::Type { .. } => {}
            }
        }

        Ok(results)
    }

    fn eval(
        &mut self,
        expr_id: ExprId,
        ast: &Ast,
        symbols: &Interner,
        resolution: &ResolutionTable,
    ) -> Result<Value, RuntimeError> {
        let value = match ast.exprs.get(expr_id) {
            Expr::Literal(lit) => match lit {
                Literal::Integer(i) => Value::Int(*i),
                Literal::String(s) => Value::String(Arc::from(symbols.resolve(*s))),
                Literal::Boolean(b) => Value::Bool(*b),
            },

            Expr::Identifier(_) => match resolution.exprs.get(&expr_id).unwrap() {
                BindingRef::Local(decl_id) => match ast.decls.get(*decl_id) {
                    Decl::Let { name, .. } => self.env.lookup(*name).cloned().unwrap(),
                    _ => unreachable!(),
                },

                BindingRef::Module(binding_id) => match self.registry.get(*binding_id) {
                    Binding::Function(_) => Value::Function(*binding_id),
                    _ => unreachable!(),
                },
            },

            Expr::List(items) => self.eval_list(items, ast, symbols, resolution)?,

            Expr::Record(fields) => self.eval_record(fields, ast, symbols, resolution)?,

            Expr::Function { params, body } => Value::Closure {
                params: params.clone(),
                body: *body,
                env: Rc::new(self.env.clone()),
            },

            Expr::Application { callee, args } => {
                let callee = self.eval(*callee, ast, symbols, resolution)?;

                let arg_values = args
                    .iter()
                    .map(|arg| self.eval(*arg, ast, symbols, resolution))
                    .collect()?;

                match callee {
                    Value::Closure { params, body, env } => {
                        let saved_env = self.env.clone();

                        for (param, arg) in params.iter().zip(args) {
                            self.env.bind(*param, arg);
                        }

                        let result = self.eval(body, ast, symbols, resolution)?;

                        self.env = saved_env;

                        result
                    }

                    Value::Function(binding_id) => match self.registry.get(binding_id) {
                        Binding::Function(func) => func.call(arg_values)?,
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                }
            }
        };
        Ok(value)
    }

    fn eval_list(
        &mut self,
        items: &[ExprId],
        ast: &Ast,
        symbols: &Interner,
        resolution: &ResolutionTable,
    ) -> Result<Value, RuntimeError> {
        if items.is_empty() {
            return Ok(Value::Series(Default::default()));
        }

        let values = items
            .iter()
            .map(|item| self.eval(*item, ast, symbols, resolution))
            .collect::<Result<Vec<_>, _>>()?;

        match values[0] {
            // List of scalars → LazyFrame with single column
            Value::Int(_) | Value::String(_) | Value::Bool(_) => {
                Ok(Value::Series(Series::from_iter(values)))
            }

            // List of LazyFrames → concatenate them vertically
            Value::LazyFrame(_) => {
                let dfs = values
                    .into_iter()
                    .map(|v| match v {
                        Value::LazyFrame(df) => df.lazy(),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                Ok(Value::LazyFrame(concat(dfs, UnionArgs::default())?))
            }

            // List of Series → concatenate them horizontally
            Value::Series(_) => {
                let series = values.into_iter().map(|v| match v {
                    Value::Series(s) => s,
                    _ => unreachable!(),
                });
            }

            // we cannot build lists from function values
            _ => unreachable!(),
        }
    }

    fn eval_record(
        &mut self,
        fields: &[(Symbol, ExprId)],
        ast: &Ast,
        symbols: &Interner,
        resolution: &ResolutionTable,
    ) -> Result<Value, RuntimeError> {
        let evaluated = fields
            .iter()
            .map(|(name, expr)| {
                self.eval(*expr, ast, symbols, resolution)
                    .map(|val| (*name, val))
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (name, value) in evaluated {
            match value {
                Value::Bool(bool) => {}
                Value::Int(int) => {}
                Value::String(s) => {}
                Value::Series(series) => {}
                Value::LazyFrame(df) => {}
                _ => unreachable!(),
            }
        }
    }
}
