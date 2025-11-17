use fossil_lang::context::Interner;
use polars::prelude::*;

use fossil_lang::ast::PrimitiveType;
use fossil_lang::ast::{Ast, Literal, Type};
use fossil_lang::error::ProviderError;
use fossil_lang::traits::provider::TypeProviderImpl;

pub struct CsvProvider;

impl TypeProviderImpl for CsvProvider {
    fn generate(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        symbols: &mut Interner,
    ) -> Result<Type, ProviderError> {
        let path = match args {
            [Literal::String(path)] => {
                let path = symbols.resolve(*path);
                PlPath::from_str(path)
            }
            _ => return Err(ProviderError::InvalidArguments),
        };

        let mut lf = LazyCsvReader::new(path).finish()?;
        let schema = lf.collect_schema()?;

        let fields = schema
            .iter()
            .map(move |(name, dtype)| {
                let ty = match dtype {
                    DataType::Boolean => Type::Primitive(PrimitiveType::Bool),

                    DataType::Int8
                    | DataType::Int16
                    | DataType::Int32
                    | DataType::Int64
                    | DataType::UInt8
                    | DataType::UInt16
                    | DataType::UInt32
                    | DataType::UInt64 => Type::Primitive(PrimitiveType::Int),

                    DataType::String => Type::Primitive(PrimitiveType::String),

                    _ => unimplemented!(),
                };

                let name = symbols.intern(name);
                let ty = ast.types.alloc(ty);

                (name, ty)
            })
            .collect();

        Ok(Type::Record(fields))
    }
}
