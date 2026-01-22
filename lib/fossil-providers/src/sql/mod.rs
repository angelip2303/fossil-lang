//! SQL Type Provider
//!
//! F#-style type provider for SQL databases with support for:
//! - Schema inference at compile-time
//! - Connection pooling
//! - Streaming/batch row iteration
//! - Query pushdown optimization
//!
//! # Usage
//!
//! ```ignore
//! // Basic usage - load from a table
//! type Customer = sql<"postgres://localhost/shop", "customers">
//! let customers = Customer::load()
//!
//! // Custom query
//! type ActiveOrder = sql<"${DATABASE_URL}", "SELECT * FROM orders WHERE status = 'active'">
//!
//! // With options
//! type LargeTable = sql<"postgres://...", "transactions", 5000, 60>  // batch_size=5000, timeout=60
//! ```

use std::sync::Arc;

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{
    Ast, Literal, PrimitiveType, ProviderArgument, RecordField, Type as AstType,
    TypeKind as AstTypeKind,
};
use fossil_lang::context::Interner;
use fossil_lang::error::ProviderError;
use fossil_lang::traits::provider::{
    FunctionDef, ModuleSpec, ProviderOutput, ProviderParamInfo, TypeProviderImpl,
};
use polars::prelude::DataType;

pub mod config;
pub mod connection;
pub mod error;
pub mod functions;
pub mod pushdown;
pub mod schema;
pub mod streaming;

use config::parse_sql_config;
use connection::get_runtime;
use functions::SqlLoadFunction;
use schema::{SqlSchema, infer_schema};

/// SQL Type Provider
///
/// Generates record types and load functions from SQL database tables/queries.
pub struct SqlProvider;

impl TypeProviderImpl for SqlProvider {
    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![
            ProviderParamInfo {
                name: "url",
                required: true,
                default: None,
            },
            ProviderParamInfo {
                name: "source",
                required: false,
                default: None,
            },
            ProviderParamInfo {
                name: "batch_size",
                required: false,
                default: Some(Literal::Integer(1000)),
            },
            ProviderParamInfo {
                name: "timeout",
                required: false,
                default: Some(Literal::Integer(30)),
            },
        ]
    }

    fn provide(
        &self,
        args: &[ProviderArgument],
        ast: &mut Ast,
        interner: &mut Interner,
        type_name: &str,
    ) -> Result<ProviderOutput, ProviderError> {
        // Parse configuration from arguments
        let config = parse_sql_config(args, interner)?;

        eprintln!("[SQL Provider] Type: {}", type_name);
        eprintln!(
            "[SQL Provider] Connection string: {}",
            config.connection_string
        );
        eprintln!("[SQL Provider] Current dir: {:?}", std::env::current_dir());

        // Infer schema from database (runs async code synchronously)
        let rt = get_runtime();
        let schema = rt
            .block_on(async { infer_schema(&config).await })
            .map_err(|e| {
                eprintln!("[SQL Provider] Error: {:?}", e);
                e.to_provider_error(interner)
            })?;

        // Convert schema to AST fields
        let fields = schema_to_ast_fields(&schema, ast, interner);

        // Create AST record type
        let record_ty = ast.types.alloc(AstType {
            loc: Loc::generated(),
            kind: AstTypeKind::Record(fields),
        });

        // Generate module with load function
        let module_spec = ModuleSpec {
            functions: vec![FunctionDef {
                name: "load".to_string(),
                implementation: Arc::new(SqlLoadFunction {
                    config: config.clone(),
                    schema: schema.clone(),
                    type_name: type_name.to_string(),
                }),
            }],
            submodules: vec![],
        };

        Ok(ProviderOutput {
            generated_type: record_ty,
            module_spec: Some(module_spec),
        })
    }
}

/// Convert SQL schema to AST record fields
fn schema_to_ast_fields(
    schema: &SqlSchema,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Vec<RecordField> {
    schema
        .columns
        .iter()
        .map(|col| {
            let field_name = interner.intern(&col.name);
            let primitive_type = polars_dtype_to_primitive(&col.polars_dtype);
            let ty = ast.types.alloc(AstType {
                loc: Loc::generated(),
                kind: AstTypeKind::Primitive(primitive_type),
            });
            RecordField {
                name: field_name,
                ty,
                attrs: vec![],
            }
        })
        .collect()
}

/// Convert polars DataType to fossil PrimitiveType
fn polars_dtype_to_primitive(dtype: &DataType) -> PrimitiveType {
    match dtype {
        DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UInt8
        | DataType::UInt16
        | DataType::UInt32
        | DataType::UInt64 => PrimitiveType::Int,
        DataType::Float32 | DataType::Float64 => PrimitiveType::Float,
        DataType::Boolean => PrimitiveType::Bool,
        DataType::String | DataType::Categorical(_, _) => PrimitiveType::String,
        _ => PrimitiveType::String, // Default to string for unknown types
    }
}
