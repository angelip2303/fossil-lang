use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{Ast, Literal, PrimitiveType, RecordField, Type, TypeKind};
use fossil_lang::context::{DefId, Interner};
use fossil_lang::error::{CompileErrorKind, ProviderError, ProviderErrorKind};
use fossil_lang::passes::GlobalContext;
use polars::prelude::{PlPath, PlPathRef, Schema};

// =============================================================================
// Error Construction
// =============================================================================

/// Create a ProviderError from a ProviderErrorKind
///
/// This is the main way to create provider errors with typed variants.
pub fn provider_err(kind: ProviderErrorKind) -> ProviderError {
    ProviderError::new(CompileErrorKind::Provider(kind), Loc::generated())
}

// =============================================================================
// Literal Extraction
// =============================================================================

/// Extract a string path from a Literal
///
/// Returns the path or an appropriate ProviderErrorKind.
pub fn extract_string_path(lit: &Literal, interner: &Interner) -> Result<PlPath, ProviderErrorKind> {
    match lit {
        Literal::String(sym) => Ok(PlPath::from_str(interner.resolve(*sym))),
        _ => Err(ProviderErrorKind::InvalidArgumentType {
            name: "path",
            expected: "a string",
        }),
    }
}

// =============================================================================
// Type Lookup
// =============================================================================

/// Look up a type's DefId by name from the GlobalContext
pub fn lookup_type_id(name: &str, gcx: &GlobalContext) -> Option<DefId> {
    gcx.interner
        .lookup(name)
        .and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()))
}

// =============================================================================
// File Validation
// =============================================================================

/// Validate file extension matches allowed list
pub fn validate_extension(path: PlPathRef, allowed: &[&str]) -> Result<(), ProviderErrorKind> {
    match path.extension() {
        Some(ext) if allowed.contains(&ext) => Ok(()),
        Some(other) => Err(ProviderErrorKind::InvalidExtension {
            found: other.to_string(),
            expected: allowed.join(", "),
        }),
        None => Err(ProviderErrorKind::InvalidExtension {
            found: "(none)".to_string(),
            expected: allowed.join(", "),
        }),
    }
}

/// Validate that path exists and is a file
pub fn validate_path(path: PlPathRef) -> Result<(), ProviderErrorKind> {
    match path {
        PlPathRef::Local(p) => {
            if !p.exists() {
                return Err(ProviderErrorKind::FileNotFound {
                    path: p.display().to_string(),
                });
            }
            if !p.is_file() {
                return Err(ProviderErrorKind::NotAFile {
                    path: p.display().to_string(),
                });
            }
            Ok(())
        }
        PlPathRef::Cloud(_cloud) => todo!("Cloud path validation not implemented"),
    }
}

// =============================================================================
// Schema Conversion
// =============================================================================

/// Convert a Polars Schema to AST RecordFields
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
