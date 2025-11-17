use std::path::Path;

use fossil_lang::ast::{Ast, Literal, Type, TypeId};
use fossil_lang::context::{Interner, Symbol};
use fossil_lang::error::ProviderError;
use polars::prelude::*;

pub fn extract_path_arg<'a>(
    args: &'a [Literal],
    symbols: &'a Interner,
) -> Result<&'a str, ProviderError> {
    match args {
        [Literal::String(path)] => Ok(symbols.resolve(*path)),
        _ => Err(ProviderError::InvalidArguments),
    }
}

pub fn validate_extension(path_str: &str, allowed: &[&str]) -> Result<(), ProviderError> {
    let path = Path::new(path_str);
    let expected = allowed.join(", ");

    match path.extension().and_then(|s| s.to_str()) {
        Some(ext) if allowed.contains(&ext) => Ok(()),
        Some(other) => Err(ProviderError::InvalidExtension {
            expected,
            found: other.to_string(),
        }),
        None => Err(ProviderError::InvalidExtension {
            expected,
            found: "none".to_string(),
        }),
    }
}

pub fn validate_local_file(path_str: &str) -> Result<(), ProviderError> {
    if !PlPath::new(path_str).is_local() {
        return Ok(());
    }

    let path = Path::new(path_str);

    if !path.exists() {
        return Err(ProviderError::FileNotFound(path_str.to_string()));
    }

    if !path.is_file() {
        return Err(ProviderError::NotAFile(path_str.to_string()));
    }

    Ok(())
}

pub fn schema_to_fields(
    schema: &Schema,
    ast: &mut Ast,
    symbols: &mut Interner,
) -> Vec<(Symbol, TypeId)> {
    schema
        .iter()
        .map(|(name, dtype)| {
            let name = symbols.intern(name);
            let ty = ast.types.alloc(Type::Primitive(dtype.clone().into()));
            (name, ty)
        })
        .collect()
}
