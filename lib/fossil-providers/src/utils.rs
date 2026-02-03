use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{Ast, Literal, PrimitiveType, RecordField, Type, TypeKind};
use fossil_lang::context::{DefId, Interner};
use fossil_lang::error::FossilError;
use fossil_lang::passes::GlobalContext;

use polars::prelude::{PlPath, PlPathRef, Schema};

pub fn extract_string_path(
    lit: &Literal,
    interner: &Interner,
    loc: Loc,
) -> Result<PlPath, FossilError> {
    match lit {
        Literal::String(sym) => Ok(PlPath::from_str(interner.resolve(*sym))),
        _ => Err(FossilError::invalid_argument_type("path", "a string", loc)),
    }
}

pub fn lookup_type_id(name: &str, gcx: &GlobalContext) -> Option<DefId> {
    gcx.interner
        .lookup(name)
        .and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()))
}

pub fn validate_extension(path: PlPathRef, allowed: &[&str], loc: Loc) -> Result<(), FossilError> {
    match path.extension() {
        Some(ext) if allowed.contains(&ext) => Ok(()),
        Some(other) => Err(FossilError::invalid_extension(other, allowed.join(", "), loc)),
        None => Err(FossilError::invalid_extension("(none)", allowed.join(", "), loc)),
    }
}

pub fn validate_path(path: PlPathRef, loc: Loc) -> Result<(), FossilError> {
    match path {
        PlPathRef::Local(p) => {
            if !p.exists() {
                return Err(FossilError::file_not_found(p.display().to_string(), loc));
            }
            if !p.is_file() {
                return Err(FossilError::not_a_file(p.display().to_string(), loc));
            }
            Ok(())
        }
        PlPathRef::Cloud(_cloud) => todo!("Cloud path validation not implemented"),
    }
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
                loc: Loc::generated(),
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
