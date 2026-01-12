use std::path::Path;

use fossil_lang::ast::ast::{Literal, Ast, TypeId, Type, TypeKind, PrimitiveType, RecordField};
use fossil_lang::context::{Interner, Symbol};
use fossil_lang::error::ProviderError;
use polars::prelude::*;

pub fn extract_path_arg(
    args: &[Literal],
    interner: &mut Interner,
) -> Result<String, ProviderError> {
    match args {
        [Literal::String(path)] => Ok(interner.resolve(*path).to_string()),
        _ => {
            use fossil_lang::error::{CompileError, CompileErrorKind};
            let msg = interner.intern("Provider expects a single string argument (file path)");
            Err(CompileError::new(
                CompileErrorKind::ProviderError(msg),
                fossil_lang::ast::Loc::generated(),
            ))
        }
    }
}

pub fn validate_extension(
    path_str: &str,
    allowed: &[&str],
    interner: &mut Interner,
) -> Result<(), ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    let path = Path::new(path_str);

    match path.extension().and_then(|s| s.to_str()) {
        Some(ext) if allowed.contains(&ext) => Ok(()),
        Some(other) => {
            let msg = interner.intern(&format!(
                "Invalid file extension '{}', expected: {}",
                other,
                allowed.join(", ")
            ));
            Err(CompileError::new(
                CompileErrorKind::ProviderError(msg),
                fossil_lang::ast::Loc::generated(),
            ))
        }
        None => {
            let msg = interner.intern(&format!(
                "No file extension found, expected: {}",
                allowed.join(", ")
            ));
            Err(CompileError::new(
                CompileErrorKind::ProviderError(msg),
                fossil_lang::ast::Loc::generated(),
            ))
        }
    }
}

pub fn validate_local_file(
    path_str: &str,
    interner: &mut Interner,
) -> Result<(), ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    if !PlPath::new(path_str).is_local() {
        return Ok(());
    }

    let path = Path::new(path_str);

    if !path.exists() {
        let msg = interner.intern(&format!("File not found: {}", path_str));
        return Err(CompileError::new(
            CompileErrorKind::ProviderError(msg),
            fossil_lang::ast::Loc::generated(),
        ));
    }

    if !path.is_file() {
        let msg = interner.intern(&format!("Not a file: {}", path_str));
        return Err(CompileError::new(
            CompileErrorKind::ProviderError(msg),
            fossil_lang::ast::Loc::generated(),
        ));
    }

    Ok(())
}

pub fn schema_to_ast_fields(
    schema: &Schema,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Vec<RecordField> {
    schema
        .iter()
        .map(|(name, dtype)| {
            let field_name = interner.intern(name);
            let primitive_type: PrimitiveType = dtype.clone().into();
            let ty = ast.types.alloc(Type {
                loc: fossil_lang::ast::Loc::generated(),
                kind: TypeKind::Primitive(primitive_type),
            });
            RecordField {
                name: field_name,
                ty,
                attrs: vec![],
            }
        })
        .collect()
}
