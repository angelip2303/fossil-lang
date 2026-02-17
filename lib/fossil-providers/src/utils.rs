use fossil_lang::ast::Loc;
use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{DefId, Interner};
use fossil_lang::error::FossilError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::traits::provider::{FieldSpec, FieldType};

use polars::prelude::{CloudScheme, PlPath, PlPathRef, Schema};

pub fn lookup_type_id(name: &str, gcx: &GlobalContext) -> Option<DefId> {
    gcx.interner.lookup(name).and_then(|sym| {
        gcx.definitions
            .find_by_symbol(sym, |k| matches!(k, fossil_lang::context::DefKind::Type))
            .map(|d| d.id())
    })
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
        PlPathRef::Cloud(url) => match url.scheme() {
            CloudScheme::S3 | CloudScheme::S3a
            | CloudScheme::Gs | CloudScheme::Gcs
            | CloudScheme::Az | CloudScheme::Azure
            | CloudScheme::Abfs | CloudScheme::Abfss
            | CloudScheme::Adl => Ok(()),
            other => Err(FossilError::data_error(
                format!("Unsupported cloud storage scheme '{:?}'. Supported: s3://, gs://, az://, abfss://", other),
                loc,
            )),
        }
    }
}

pub fn polars_schema_to_field_specs(
    schema: &Schema,
    interner: &mut Interner,
) -> Vec<FieldSpec> {
    schema
        .iter()
        .map(|(name, dtype)| {
            let primitive_type: PrimitiveType = dtype.clone().into();
            FieldSpec {
                name: interner.intern(name),
                ty: FieldType::Primitive(primitive_type),
                attrs: vec![],
            }
        })
        .collect()
}

pub fn resolve_path(path_str: &str) -> PlPath {
    PlPath::from_str(path_str)
}
