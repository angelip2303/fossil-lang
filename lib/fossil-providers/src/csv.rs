use std::io::Cursor;
use std::sync::Arc;

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{Ast, Literal, ProviderArgument, Type as AstType, TypeKind as AstTypeKind};
use fossil_lang::ast::thir::{
    Polytype, Type as ThirType, TypeKind as ThirTypeKind, TypeVar, TypedHir,
};
use fossil_lang::context::Interner;
use fossil_lang::error::{ProviderError, RuntimeError};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec, ProviderOutput, ProviderParamInfo, TypeProviderImpl};
use polars::prelude::*;

use crate::source::{DataSource, ReadableSource, format_source_error};
use crate::utils::*;

/// Configuration options for CSV provider
#[derive(Debug, Clone)]
pub struct CsvOptions {
    /// File path to the CSV
    pub path: String,
    /// Delimiter character (default: ',')
    pub delimiter: u8,
    /// Whether the CSV has a header row (default: true)
    pub has_header: bool,
    /// Quote character (default: '"')
    pub quote_char: Option<u8>,
    /// Number of rows to use for schema inference (default: 100)
    pub infer_schema_length: Option<usize>,
}

impl Default for CsvOptions {
    fn default() -> Self {
        Self {
            path: String::new(),
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
    interner: &mut Interner,
) -> Result<CsvOptions, ProviderError> {
    use fossil_lang::error::CompileErrorKind;

    if args.is_empty() {
        return Err(ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("CSV provider requires at least a file path argument"),
            ),
            Loc::generated(),
        ));
    }

    let mut options = CsvOptions::default();

    // Track which args have been processed by name
    let path_sym = interner.intern("path");
    let delimiter_sym = interner.intern("delimiter");
    let has_header_sym = interner.intern("has_header");

    // First pass: collect named arguments
    for arg in args {
        if let ProviderArgument::Named { name, value } = arg {
            if *name == path_sym {
                if let Literal::String(sym) = value {
                    options.path = interner.resolve(*sym).to_string();
                }
            } else if *name == delimiter_sym {
                if let Literal::String(sym) = value {
                    let delim_str = interner.resolve(*sym);
                    if !delim_str.is_empty() {
                        options.delimiter = delim_str.as_bytes()[0];
                    }
                }
            } else if *name == has_header_sym {
                if let Literal::Boolean(b) = value {
                    options.has_header = *b;
                }
            }
        }
    }

    // Second pass: handle positional arguments (only if path not set by named)
    if options.path.is_empty() {
        let mut positional_idx = 0;
        for arg in args {
            if let ProviderArgument::Positional(lit) = arg {
                match positional_idx {
                    0 => {
                        // First positional: path
                        if let Literal::String(sym) = lit {
                            options.path = interner.resolve(*sym).to_string();
                        } else {
                            return Err(ProviderError::new(
                                CompileErrorKind::ProviderError(
                                    interner.intern("CSV provider path must be a string literal"),
                                ),
                                Loc::generated(),
                            ));
                        }
                    }
                    1 => {
                        // Second positional: delimiter
                        if let Literal::String(sym) = lit {
                            let delim_str = interner.resolve(*sym);
                            if !delim_str.is_empty() {
                                options.delimiter = delim_str.as_bytes()[0];
                            }
                        }
                    }
                    2 => {
                        // Third positional: has_header
                        if let Literal::Boolean(b) = lit {
                            options.has_header = *b;
                        }
                    }
                    _ => {}
                }
                positional_idx += 1;
            }
        }
    }

    // Validate that path was provided
    if options.path.is_empty() {
        return Err(ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("CSV provider requires a file path"),
            ),
            Loc::generated(),
        ));
    }

    Ok(options)
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
        use fossil_lang::error::CompileErrorKind;

        let options = parse_csv_options(args, interner)?;
        let uri = &options.path;

        // Detect data source type (local file or HTTP)
        let source = DataSource::detect(uri).map_err(|e| {
            ProviderError::new(
                CompileErrorKind::ProviderError(interner.intern(&e.to_string())),
                Loc::generated(),
            )
        })?;

        // Validate file extension
        validate_extension(uri, &["csv"], interner)?;

        // For local files, validate that the file exists
        if source.is_local() {
            validate_local_file(uri, interner)?;
        }

        // Infer schema based on source type
        let schema = infer_csv_schema(&source, &options, interner)?;
        let fields = schema_to_ast_fields(&schema, ast, interner);

        // Create AST record type
        let record_ty = ast.types.alloc(AstType {
            loc: fossil_lang::ast::Loc::generated(),
            kind: AstTypeKind::Record(fields),
        });

        // Generate module with load function
        let module_spec = ModuleSpec {
            functions: vec![FunctionDef {
                name: "load".to_string(),
                implementation: Arc::new(CsvLoadFunction {
                    uri: uri.clone(),
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

/// Infers CSV schema from any data source (local or HTTP).
fn infer_csv_schema(
    source: &DataSource,
    options: &CsvOptions,
    interner: &mut Interner,
) -> Result<Schema, ProviderError> {
    use fossil_lang::error::CompileErrorKind;

    match source {
        DataSource::Local(path) => {
            // Use Polars directly for local files
            let mut csv_reader = CsvReadOptions::default()
                .with_infer_schema_length(options.infer_schema_length)
                .with_has_header(options.has_header);

            if options.delimiter != b',' {
                csv_reader = csv_reader
                    .map_parse_options(|opts| opts.with_separator(options.delimiter));
            }

            if let Some(quote) = options.quote_char {
                csv_reader = csv_reader
                    .map_parse_options(|opts| opts.with_quote_char(Some(quote)));
            }

            let df = csv_reader
                .try_into_reader_with_file_path(Some(path.clone()))
                .map_err(|e| {
                    ProviderError::new(
                        CompileErrorKind::Runtime(format!("Failed to open CSV file: {}", e)),
                        Loc::generated(),
                    )
                })?
                .finish()
                .map_err(|e| {
                    ProviderError::new(
                        CompileErrorKind::Runtime(format!("Failed to parse CSV file: {}", e)),
                        Loc::generated(),
                    )
                })?;

            Ok(df.schema().as_ref().clone())
        }
        DataSource::Http(_) => {
            // Fetch data over HTTP and infer schema
            let bytes = source.read_for_schema(None).map_err(|e| {
                let error_msg = format_source_error(source, "infer schema", &e);
                ProviderError::new(
                    CompileErrorKind::ProviderError(interner.intern(&error_msg)),
                    Loc::generated(),
                )
            })?;

            // Parse CSV from bytes using Polars
            let cursor = Cursor::new(bytes);
            let mut csv_reader = CsvReadOptions::default()
                .with_infer_schema_length(options.infer_schema_length)
                .with_has_header(options.has_header);

            if options.delimiter != b',' {
                csv_reader = csv_reader
                    .map_parse_options(|opts| opts.with_separator(options.delimiter));
            }

            if let Some(quote) = options.quote_char {
                csv_reader = csv_reader
                    .map_parse_options(|opts| opts.with_quote_char(Some(quote)));
            }

            let df = csv_reader
                .into_reader_with_file_handle(cursor)
                .finish()
                .map_err(|e| {
                    ProviderError::new(
                        CompileErrorKind::Runtime(format!(
                            "Failed to parse CSV from '{}': {}",
                            source.as_str(),
                            e
                        )),
                        Loc::generated(),
                    )
                })?;

            Ok(df.schema().as_ref().clone())
        }
    }
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
    uri: String,
    options: CsvOptions,
    /// The name of the type this function belongs to (e.g., "PersonData")
    /// Used to look up the type DefId at signature time
    type_name: String,
}

impl FunctionImpl for CsvLoadFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        // Look up the type DefId by name
        let type_sym = gcx.interner.lookup(&self.type_name);
        let type_def_id = type_sym.and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()));

        let result_ty = if let Some(def_id) = type_def_id {
            // Look up the List type constructor
            let list_sym = gcx.interner.lookup("List");
            let list_def_id = list_sym.and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()));

            // Create Named type for the record type
            let record_ty = thir.types.alloc(ThirType {
                loc: Loc::generated(),
                kind: ThirTypeKind::Named(def_id),
            });

            if let Some(list_id) = list_def_id {
                // Return List<RecordType>
                thir.types.alloc(ThirType {
                    loc: Loc::generated(),
                    kind: ThirTypeKind::App {
                        ctor: list_id,
                        args: vec![record_ty],
                    },
                })
            } else {
                // Fallback: just return the record type (shouldn't happen)
                record_ty
            }
        } else {
            // Fallback: use type variable if we can't find the type
            let t_var = _next_type_var();
            thir.types.alloc(ThirType {
                loc: Loc::generated(),
                kind: ThirTypeKind::Var(t_var),
            })
        };

        // Create function type: () -> List<RecordType>
        let fn_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Function(vec![], result_ty),
        });

        Polytype::mono(fn_ty)
    }

    fn call(&self, _args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        // Use scan_csv for TRUE lazy loading - data is only read when needed
        // This enables streaming/chunked processing for large files
        // Polars with "http" feature handles both local paths and HTTP URLs
        let mut lf = LazyCsvReader::new(PlPath::new(&self.uri))
            .with_has_header(self.options.has_header)
            .with_separator(self.options.delimiter);

        // Apply custom quote char if specified
        if let Some(quote) = self.options.quote_char {
            lf = lf.with_quote_char(Some(quote));
        }

        let lazy_frame = lf.finish().map_err(|e| {
            let compile_err: RuntimeError = e.into();
            compile_err
        })?;

        Ok(Value::Records(lazy_frame))
    }
}
