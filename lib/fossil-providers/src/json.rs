use std::fs::File;
use std::io::BufReader;
use std::num::NonZeroUsize;
use std::path::Path;

use fossil_lang::ast::{Ast, Literal, Type};
use fossil_lang::context::Interner;
use fossil_lang::error::ProviderError;
use fossil_lang::traits::provider::TypeProviderImpl;
use polars::prelude::*;

use crate::utils::*;

pub struct JsonProvider;

impl TypeProviderImpl for JsonProvider {
    fn generate(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        symbols: &mut Interner,
    ) -> Result<Type, ProviderError> {
        let path_str = extract_path_arg(args, symbols)?;

        validate_extension(path_str, &["json", "ndjson"])?;
        validate_local_file(path_str)?;

        let path = Path::new(path_str);
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let df = JsonReader::new(reader)
            .infer_schema_len(NonZeroUsize::new(100))
            .finish()?;

        let schema = df.schema();
        let fields = schema_to_fields(&schema, ast, symbols);

        Ok(Type::Record(fields))
    }
}
