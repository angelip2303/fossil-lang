use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::{DataFrame, IntoColumn, IntoLazy, NamedFrom, PlSmallStr, UnionArgs, concat};
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
            Expr::Unit => Value::Unit,

            Expr::Literal(lit) => match lit {
                Literal::Integer(i) => Value::Int(*i),
                Literal::String(s) => Value::String(Arc::from(symbols.resolve(*s))),
                Literal::Boolean(b) => Value::Bool(*b),
            },

            Expr::Identifier(path) => match resolution.exprs.get(&expr_id).unwrap() {
                BindingRef::Local(decl) => match ast.decls.get(*decl) {
                    Decl::Let { name, .. } => self.env.lookup(*name).cloned().unwrap(),
                    _ => unreachable!(),
                },

                BindingRef::Module(binding) => match self.registry.get(*binding) {
                    Binding::Function(_) => Value::Function(*binding),
                    _ => unreachable!(),
                },

                BindingRef::Parameter { function: _ } => {
                    let name = match path {
                        Path::Simple(n) => n,
                        Path::Qualified(_) => unreachable!(),
                    };

                    self.env.lookup(*name).cloned().unwrap()
                }
            },

            Expr::List(items) => self.eval_list(items, ast, symbols, resolution)?,

            Expr::Record(fields) => self.eval_record(fields, ast, symbols, resolution)?,

            Expr::Function { params, body } => Value::Closure {
                params: params.clone(),
                body: *body,
                env: Rc::new(self.env.clone()),
            },

            Expr::Application { callee, args } => {
                let callee_val = self.eval(*callee, ast, symbols, resolution)?;

                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|arg| self.eval(*arg, ast, symbols, resolution))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee_val {
                    Value::Closure { params, body, env } => {
                        let saved_env = self.env.clone();
                        self.env = (*env).clone();

                        for (param, arg) in params.iter().zip(arg_values.iter()) {
                            self.env.bind(*param, arg.clone());
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
            return Ok(Value::Series(Series::default()));
        }

        let values = items
            .iter()
            .map(|item| self.eval(*item, ast, symbols, resolution))
            .collect::<Result<Vec<_>, _>>()?;

        match &values[0] {
            Value::Int(_) => {
                let ints = values.into_iter().map(|v| match v {
                    Value::Int(i) => i,
                    _ => unreachable!(),
                });
                Ok(Value::Series(Series::from_iter(ints)))
            }

            Value::String(_) => {
                let strings = values.iter().map(|v| match v {
                    Value::String(s) => s.as_ref(),
                    _ => unreachable!(),
                });
                Ok(Value::Series(Series::from_iter(strings)))
            }

            Value::Bool(_) => {
                let bools = values.into_iter().map(|v| match v {
                    Value::Bool(b) => b,
                    _ => unreachable!(),
                });
                Ok(Value::Series(Series::from_iter(bools)))
            }

            Value::LazyFrame(_) => {
                let dfs: Vec<_> = values
                    .into_iter()
                    .map(|v| match v {
                        Value::LazyFrame(df) => df.lazy(),
                        _ => unreachable!(),
                    })
                    .collect();
                Ok(Value::LazyFrame(concat(dfs, UnionArgs::default())?))
            }

            Value::Unit | Value::Closure { .. } | Value::Function(_) | Value::Series(_) => {
                unreachable!("Type checker prevents Unit, functions and lists as elements in lists")
            }
        }
    }

    fn eval_record(
        &mut self,
        fields: &[(Symbol, ExprId)],
        ast: &Ast,
        symbols: &Interner,
        resolution: &ResolutionTable,
    ) -> Result<Value, RuntimeError> {
        if fields.is_empty() {
            return Ok(Value::LazyFrame(Default::default()));
        }

        let evaluated: Vec<(Symbol, Value)> = fields
            .iter()
            .map(|(name, expr)| {
                self.eval(*expr, ast, symbols, resolution)
                    .map(|val| (*name, val))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut series_vec = Vec::new();

        for (name, value) in evaluated {
            let name = PlSmallStr::from_str(symbols.resolve(name));

            let series = match value {
                Value::Int(i) => Series::new(name, &[i]).into_column(),
                Value::String(s) => Series::new(name, &[s.as_ref()]).into_column(),
                Value::Bool(b) => Series::new(name, &[b]).into_column(),

                Value::Series(mut s) => {
                    s.rename(name);
                    s.into_column()
                }

                Value::Unit | Value::Closure { .. } | Value::Function(_) | Value::LazyFrame(_) => {
                    unreachable!(
                        "Type checker prevents Unit, functions and records as fields in a record"
                    )
                }
            };

            series_vec.push(series);
        }

        let df = DataFrame::new(series_vec)?;
        Ok(Value::LazyFrame(df.lazy()))
    }
}
