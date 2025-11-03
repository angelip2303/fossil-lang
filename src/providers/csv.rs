use polars::prelude::*;

use crate::ast::*;
use crate::const_eval::ConstEvaluator;
use crate::error::Error;
use crate::functions::Value;
use crate::providers::Read;
use crate::providers::TypeProvider;
use crate::providers::Write;
use crate::solver::Type;

pub struct CsvProvider;

impl TypeProvider for CsvProvider {
    fn provide(&self, ast: &Ast, args: &[Arg]) -> Result<Type, Error> {
        // TODO: maybe we could define a signature here?
        if args.is_empty() {
            todo!("Csv provider requires a file path argument")
        }

        let eval = ConstEvaluator::new(ast);
        let path = eval.eval_to_string(args[0].value)?;
        let file = std::fs::read_to_string(&path).map_err(|e| todo!())?;

        let header = file.lines().next().ok_or_else(|| todo!())?;

        let fields = header
            .split(',')
            .map(|col_name| {
                // TODO: we could try to infer the type of the column
                let name = col_name.trim().to_string();
                (name, Box::new(Type::String))
            })
            .collect();

        Ok(Type::Record(fields))
    }
}

impl Read for CsvProvider {
    type Output = LazyFrame;
    fn read(&self, args: &[Value]) -> Result<Self::Output, Error> {
        // TODO: maybe we should check this in compile time?
        let path = match args[0].clone() {
            Value::String(path) => PlPath::from_str(&path),
            _ => todo!("Expected a string argument"),
        };

        Ok(LazyCsvReader::new(path).finish().unwrap())
    }
}

impl Write for CsvProvider {
    type Input = LazyFrame;
    fn write(&self, input: Self::Input, args: &[Value]) -> Result<(), Error> {
        // TODO: maybe we should check this in compile time?
        let path = match args[0].clone() {
            Value::String(path) => path,
            _ => todo!("Expected a string argument"),
        };

        let mut file = std::fs::File::create(path).map_err(|e| todo!())?;
        let mut df = input.collect().unwrap();
        Ok(CsvWriter::new(&mut file).finish(&mut df).unwrap())
    }
}
