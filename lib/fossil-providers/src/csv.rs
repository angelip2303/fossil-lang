use fossil_lang::ast::{Ast, Literal, Type};
use fossil_lang::context::Interner;
use fossil_lang::error::ProviderError;
use fossil_lang::traits::provider::TypeProviderImpl;
use polars::prelude::*;

use crate::utils::*;

pub struct CsvProvider;

impl TypeProviderImpl for CsvProvider {
    fn generate(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        symbols: &mut Interner,
    ) -> Result<Type, ProviderError> {
        let path_str = extract_path_arg(args, symbols)?;

        validate_extension(path_str, &["csv"])?;
        validate_local_file(path_str)?;

        let df = CsvReadOptions::default()
            .with_infer_schema_length(Some(100))
            .with_has_header(true)
            .try_into_reader_with_file_path(Some(path_str.into()))?
            .finish()?;

        let schema = df.schema();
        let fields = schema_to_fields(&schema, ast, symbols);

        Ok(Type::Record(fields))
    }
}
