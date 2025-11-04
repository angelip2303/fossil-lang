use polars::prelude::*;

use crate::const_eval::ConstEvaluator;
use crate::error::CompileError;
use crate::providers::Arg;
use crate::providers::Ast;
use crate::providers::TypeProvider;
use crate::solver::Type;

pub struct CsvProvider;

impl TypeProvider for CsvProvider {
    fn param_types(&self) -> Vec<Type> {
        vec![Type::String]
    }

    fn provide(&self, ast: &Ast, args: &[Arg]) -> Result<Type, CompileError> {
        let eval = ConstEvaluator::new(ast);
        let path = eval.eval_to_string(args[0].value)?;
        let file = std::fs::read_to_string(&path).map_err(|e| todo!("failed to read csv"))?;

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

    fn load(&self, path: &str) -> Result<LazyFrame, CompileError> {
        use polars::prelude::*;

        let path = PlPath::from_str(path);
        LazyCsvReader::new(path)
            .finish()
            .map_err(|_| todo!("failed to read csv"))
    }
}
