use std::sync::Arc;

use fossil_lang::ast::ast::{
    Ast, Literal, ProviderArgument, Type as AstType, TypeKind as AstTypeKind,
};
use fossil_lang::context::Interner;
use fossil_lang::error::{ProviderError, ProviderErrorKind, RuntimeError};
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{
    FunctionDef, ModuleSpec, ProviderOutput, ProviderParamInfo, TypeProviderImpl,
};
use polars::prelude::*;

use crate::utils::{
    extract_string_path, lookup_type_id, provider_err, schema_to_ast_fields, validate_extension,
    validate_path,
};

/// Configuration options for CSV provider
#[derive(Debug, Clone)]
pub struct CsvOptions {
    pub delimiter: u8,
    pub has_header: bool,
    pub quote_char: Option<u8>,
    pub infer_schema_length: Option<usize>,
}

impl Default for CsvOptions {
    fn default() -> Self {
        Self {
            delimiter: b',',
            has_header: true,
            quote_char: Some(b'"'),
            infer_schema_length: Some(100),
        }
    }
}

/// Parse CSV provider options from provider arguments (positional or named)
fn parse_csv_options(
    args: &[ProviderArgument],
    interner: &Interner,
) -> Result<(PlPath, CsvOptions), ProviderError> {
    let mut options = CsvOptions::default();
    let mut iter = args.iter();

    let Some(arg) = iter.next() else {
        return Err(provider_err(ProviderErrorKind::MissingArgument {
            name: "path",
            provider: "csv",
        }));
    };

    let ProviderArgument::Positional(lit) = arg else {
        return Err(provider_err(ProviderErrorKind::MissingArgument {
            name: "path",
            provider: "csv",
        }));
    };

    let path = extract_string_path(lit, interner).map_err(provider_err)?;

    // Parse optional named arguments
    for arg in iter {
        if let ProviderArgument::Named { name, value } = arg {
            let name_str = interner.resolve(*name);
            match name_str {
                "delimiter" => {
                    if let Literal::String(sym) = value {
                        let delim_str = interner.resolve(*sym);
                        if !delim_str.is_empty() {
                            options.delimiter = delim_str.as_bytes()[0];
                        }
                    }
                }
                "has_header" => {
                    if let Literal::Boolean(b) = value {
                        options.has_header = *b;
                    }
                }
                _ => {}
            }
        }
    }

    Ok((path, options))
}

pub struct CsvProvider;

impl TypeProviderImpl for CsvProvider {
    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![
            ProviderParamInfo {
                name: "path",
                required: true,
                default: None,
            },
            ProviderParamInfo {
                name: "delimiter",
                required: false,
                default: Some(Literal::String(fossil_lang::context::Symbol::synthetic())), // ","
            },
            ProviderParamInfo {
                name: "has_header",
                required: false,
                default: Some(Literal::Boolean(true)),
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
        let (path, options) = parse_csv_options(args, interner)?;
        validate_extension(path.as_ref(), &["csv"]).map_err(provider_err)?;
        validate_path(path.as_ref()).map_err(provider_err)?;
        let schema = infer_csv_schema(path.clone(), &options)?;
        let fields = schema_to_ast_fields(&schema, ast, interner);

        let record_ty = ast.types.alloc(AstType {
            loc: fossil_lang::ast::Loc::generated(),
            kind: AstTypeKind::Record(fields),
        });

        // Generate module with load function
        let module_spec = ModuleSpec {
            functions: vec![FunctionDef {
                name: "load".to_string(),
                implementation: Arc::new(CsvLoadFunction {
                    uri: path,
                    options: options.clone(),
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

fn infer_csv_schema(path: PlPath, options: &CsvOptions) -> Result<Schema, ProviderError> {
    let schema = LazyCsvReader::new(path)
        .with_has_header(options.has_header)
        .with_infer_schema_length(options.infer_schema_length)
        .with_separator(options.delimiter)
        .with_quote_char(options.quote_char)
        .finish()?
        .collect_schema()?
        .as_ref()
        .clone();

    Ok(schema)
}

/// Load function that captures the CSV URI and options at compile-time
///
/// This function is generated by the CSV provider and captures the URI
/// and configuration options specified in the type provider invocation.
/// At runtime, it loads the CSV without needing any arguments.
///
/// The URI can be either a local file path or an HTTP/HTTPS URL:
/// - Local: `csv!("data/people.csv")`
/// - HTTP: `csv!("https://example.com/data.csv")`
///
/// Example:
/// ```ignore
/// type People = csv!("people.csv")
/// let data = People::load()  // No arguments needed!
///
/// type RemoteData = csv!("https://api.example.com/data.csv")
/// let data = RemoteData::load()  // Fetches from HTTP
/// ```
pub struct CsvLoadFunction {
    uri: PlPath,
    options: CsvOptions,
    type_name: String,
}

impl FunctionImpl for CsvLoadFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        // () -> List<RecordType>
        let result_ty = match (
            lookup_type_id(&self.type_name, gcx),
            lookup_type_id("List", gcx),
        ) {
            (Some(type_id), Some(list_id)) => {
                let record_ty = ir.named_type(type_id);
                ir.list_type(record_ty, list_id)
            }
            (Some(type_id), None) => ir.named_type(type_id),
            _ => ir.var_type(next_type_var()),
        };

        Polytype::mono(ir.fn_type(vec![], result_ty))
    }

    fn call(&self, _args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::runtime::value::{RecordsPlan, SourceDescriptor};

        // Extract path string from PlPath
        let path_str = match &self.uri {
            PlPath::Local(p) => p.to_string_lossy().to_string(),
            PlPath::Cloud(c) => c.to_string(),
        };

        // Create source descriptor - no data is loaded yet!
        let source = SourceDescriptor::Csv {
            path: path_str,
            delimiter: self.options.delimiter,
            has_header: self.options.has_header,
        };

        // Infer schema at load time (this reads only the header, not all data)
        let schema = LazyCsvReader::new(self.uri.clone())
            .with_has_header(self.options.has_header)
            .with_infer_schema_length(self.options.infer_schema_length)
            .with_separator(self.options.delimiter)
            .with_quote_char(self.options.quote_char)
            .finish()?
            .collect_schema()?
            .as_ref()
            .clone();

        // Look up type DefId to include in the plan
        let type_def_id = ctx
            .gcx
            .interner
            .lookup(&self.type_name)
            .and_then(|sym| ctx.gcx.definitions.get_by_symbol(sym).map(|d| d.id()));

        let plan = match type_def_id {
            Some(def_id) => RecordsPlan::with_type(source, schema, def_id),
            None => RecordsPlan::new(source, schema),
        };

        Ok(Value::Records(plan))
    }
}
