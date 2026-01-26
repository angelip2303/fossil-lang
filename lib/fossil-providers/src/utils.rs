use fossil_lang::ast::ast::{Ast, PrimitiveType, RecordField, Type, TypeKind};
use fossil_lang::context::Interner;
use fossil_lang::error::ProviderError;
use polars::prelude::{PlPathRef, Schema};

pub fn validate_extension(
    path: PlPathRef,
    allowed: &[&str],
    interner: &mut Interner,
) -> Result<(), ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    match path.extension() {
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

pub fn validate_path(path: PlPathRef, interner: &mut Interner) -> Result<(), ProviderError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    match path {
        PlPathRef::Local(path) => {
            if !path.exists() {
                let msg = interner.intern(&format!("File not found: {:?}", path));
                return Err(CompileError::new(
                    CompileErrorKind::ProviderError(msg),
                    fossil_lang::ast::Loc::generated(),
                ));
            }

            // in case it is a directory
            if !path.is_file() {
                let msg = interner.intern(&format!("Not a file: {:?}", path));
                return Err(CompileError::new(
                    CompileErrorKind::ProviderError(msg),
                    fossil_lang::ast::Loc::generated(),
                ));
            }
        }
        PlPathRef::Cloud(_cloud) => todo!(),
    };

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
