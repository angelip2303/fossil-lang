use std::path::Path;

use fossil_lang::ast::ast::{Ast, Literal, PrimitiveType, ProviderArgument, RecordField, Type, TypeKind};
use fossil_lang::context::Interner;
use fossil_lang::error::ProviderError;
use polars::prelude::Schema;

/// Extract path from provider arguments (supports both positional and named args)
pub fn extract_path_arg_from_provider(
    args: &[ProviderArgument],
    interner: &mut Interner,
) -> Result<String, ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    // First, try named arg "path"
    let path_sym = interner.intern("path");
    for arg in args {
        if let ProviderArgument::Named { name, value } = arg
            && *name == path_sym
                && let Literal::String(s) = value {
                    return Ok(interner.resolve(*s).to_string());
                }
    }

    // Then try first positional arg
    for arg in args {
        if let ProviderArgument::Positional(Literal::String(path)) = arg {
            return Ok(interner.resolve(*path).to_string());
        }
    }

    let msg = interner.intern("Provider expects a string argument (file path)");
    Err(CompileError::new(
        CompileErrorKind::ProviderError(msg),
        fossil_lang::ast::Loc::generated(),
    ))
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

/// Validates that a local file path exists and is a file.
///
/// Note: This function should only be called for local file paths.
/// Use `DataSource::is_local()` to check the source type before calling this.
pub fn validate_local_file(
    path_str: &str,
    interner: &mut Interner,
) -> Result<(), ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

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
