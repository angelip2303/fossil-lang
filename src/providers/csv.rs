use std::collections::HashMap;

use polars::prelude::*;

use crate::const_eval::ConstEvaluator;
use crate::error::Result;
use crate::module::Module;
use crate::providers::{Arg, Ast, TypeProvider};
use crate::runtime::{RuntimeFunction, Value};
use crate::solver::Type;

pub struct CsvProvider;

impl TypeProvider for CsvProvider {
    fn name(&self) -> &str {
        "Csv"
    }

    fn param_types(&self) -> Vec<Type> {
        vec![Type::String]
    }

    fn provide_type(&self, ast: &Ast, args: &[Arg]) -> Result<Type> {
        let eval = ConstEvaluator::new(ast);
        let path = eval.eval_to_string(args[0].value)?;

        let file = std::fs::read_to_string(&path).map_err(|_| todo!("failed to read csv"))?;

        let header = file.lines().next().ok_or_else(|| todo!("empty csv file"))?;

        let fields = header
            .split(',')
            .map(|col_name| {
                let name = col_name.trim().to_string();
                (name, Box::new(Type::String))
            })
            .collect();

        Ok(Type::Record(fields))
    }

    fn generate_module(&self, type_name: &str, ast: &Ast, args: &[Arg]) -> Result<Module> {
        let record_type = self.provide_type(ast, args)?;

        let mut module = Module::new(type_name);

        module.add_function(CsvLoadFunction {
            namespace: type_name.to_string(),
            target_type: record_type.clone(),
        });

        Ok(module)
    }
}

struct CsvLoadFunction {
    namespace: String,
    target_type: Type,
}

impl RuntimeFunction for CsvLoadFunction {
    fn name(&self) -> &str {
        "load"
    }

    fn ty(&self) -> Type {
        Type::Func(
            Box::new(Type::String),
            Box::new(Type::List(Box::new(self.target_type.clone()))),
        )
    }

    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let path = match &args[0] {
            Value::String(s) => s,
            _ => unreachable!(),
        };

        let lf = LazyCsvReader::new(PlPath::from_str(path))
            .finish()
            .map_err(|_| todo!("failed to read csv"))?;

        lazyframe_to_records(lf, &self.target_type)
    }
}

fn lazyframe_to_records(lf: LazyFrame, target_type: &Type) -> Result<Value> {
    let df = lf.collect().map_err(|_| todo!("failed to collect"))?;

    let fields = match target_type {
        Type::Record(fields) => fields,
        _ => todo!("expected record type"),
    };

    let mut rows = Vec::new();
    for idx in 0..df.height() {
        let mut record = HashMap::new();

        for (field_name, field_type) in fields {
            let series = df
                .column(field_name)
                .map_err(|_| todo!("column not found"))?;

            let value = match **field_type {
                Type::String => {
                    let s = series.str().map_err(|_| todo!("not a string column"))?;
                    Value::String(s.get(idx).unwrap_or("").to_string())
                }
                Type::Int => {
                    let i = series.i64().map_err(|_| todo!("not an int column"))?;
                    Value::Int(i.get(idx).unwrap_or(0))
                }
                Type::Bool => {
                    let b = series.bool().map_err(|_| todo!("not a bool column"))?;
                    Value::Bool(b.get(idx).unwrap_or(false))
                }
                _ => todo!("unsupported field type"),
            };

            record.insert(field_name.clone(), value);
        }

        rows.push(Value::Record(record));
    }

    Ok(Value::List(rows))
}

pub fn records_to_dataframe(records: &[Value]) -> Result<DataFrame> {
    if records.is_empty() {
        return Ok(DataFrame::empty());
    }

    let first_record = match &records[0] {
        Value::Record(r) => r,
        _ => todo!("expected records"),
    };

    let mut series_map: HashMap<String, Vec<_>> = HashMap::new();

    for key in first_record.keys() {
        series_map.insert(key.clone(), Vec::new());
    }

    for record_val in records {
        let record = match record_val {
            Value::Record(r) => r,
            _ => continue,
        };

        for (key, value) in record {
            if let Some(vec) = series_map.get_mut(key) {
                vec.push(value.clone());
            }
        }
    }

    let mut series_vec = Vec::new();
    for (name, values) in series_map {
        let series = match values.first() {
            Some(Value::String(_)) => {
                let strings: Vec<String> = values
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.clone(),
                        _ => String::new(),
                    })
                    .collect();
                Series::new(name.into(), strings)
            }
            Some(Value::Int(_)) => {
                let ints: Vec<i64> = values
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i,
                        _ => 0,
                    })
                    .collect();
                Series::new(name.into(), ints)
            }
            Some(Value::Bool(_)) => {
                let bools: Vec<bool> = values
                    .iter()
                    .map(|v| match v {
                        Value::Bool(b) => *b,
                        _ => false,
                    })
                    .collect();
                Series::new(name.into(), bools)
            }
            _ => continue,
        };
        series_vec.push(series);
    }

    Ok(DataFrame::from_iter(series_vec))
}
