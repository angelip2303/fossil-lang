use std::sync::Arc;

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{Ast, Literal, Type as AstType, TypeKind as AstTypeKind};
use fossil_lang::ast::thir::{
    Polytype, Type as ThirType, TypeKind as ThirTypeKind, TypeVar, TypedHir,
};
use fossil_lang::context::Interner;
use fossil_lang::error::{ProviderError, RuntimeError};
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec, ProviderOutput, TypeProviderImpl};
use polars::prelude::*;

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

/// Parse CSV provider options from literal arguments
fn parse_csv_options(
    args: &[Literal],
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

    // First argument is always the path
    match &args[0] {
        Literal::String(sym) => {
            options.path = interner.resolve(*sym).to_string();
        }
        _ => {
            return Err(ProviderError::new(
                CompileErrorKind::ProviderError(
                    interner.intern("CSV provider path must be a string literal"),
                ),
                Loc::generated(),
            ));
        }
    }

    // Parse optional arguments (currently as positional, could be named in future)
    // Example: csv<"file.csv", ";", "true">
    if args.len() > 1 {
        // Second arg: delimiter
        if let Literal::String(sym) = &args[1] {
            let delim_str = interner.resolve(*sym);
            if !delim_str.is_empty() {
                options.delimiter = delim_str.as_bytes()[0];
            }
        }
    }

    if args.len() > 2 {
        // Third arg: has_header
        if let Literal::Boolean(has_header) = &args[2] {
            options.has_header = *has_header;
        }
    }

    Ok(options)
}

pub struct CsvProvider;

impl TypeProviderImpl for CsvProvider {
    fn provide(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        interner: &mut Interner,
    ) -> Result<ProviderOutput, ProviderError> {
        let options = parse_csv_options(args, interner)?;
        let path_str = &options.path;

        validate_extension(path_str, &["csv"], interner)?;
        validate_local_file(path_str, interner)?;

        // Read CSV to infer schema using configured options
        let mut csv_reader = CsvReadOptions::default()
            .with_infer_schema_length(options.infer_schema_length)
            .with_has_header(options.has_header);

        // Set delimiter if different from default
        if options.delimiter != b',' {
            csv_reader =
                csv_reader.map_parse_options(|opts| opts.with_separator(options.delimiter));
        }

        // Set quote char if specified
        if let Some(quote) = options.quote_char {
            csv_reader = csv_reader.map_parse_options(|opts| opts.with_quote_char(Some(quote)));
        }

        let df = csv_reader
            .try_into_reader_with_file_path(Some(path_str.clone().into()))
            .map_err(|e| {
                use fossil_lang::error::CompileErrorKind;
                ProviderError::new(
                    CompileErrorKind::Runtime(format!("Failed to open CSV file: {}", e)),
                    Loc::generated(),
                )
            })?
            .finish()
            .map_err(|e| {
                use fossil_lang::error::CompileErrorKind;
                ProviderError::new(
                    CompileErrorKind::Runtime(format!("Failed to parse CSV file: {}", e)),
                    Loc::generated(),
                )
            })?;

        let schema = df.schema();
        let fields = schema_to_ast_fields(schema, ast, interner);

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
                    file_path: path_str.clone(),
                    options: options.clone(),
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

/// Load function that captures the CSV file path and options at compile-time
///
/// This function is generated by the CSV provider and captures the file path
/// and configuration options specified in the type provider invocation.
/// At runtime, it loads the CSV without needing any arguments.
///
/// Example:
/// ```ignore
/// type People = csv<"people.csv">
/// let data = People::load()  // No arguments needed!
///
/// type EuropeanData = csv<"data.csv", ";", true>  // Custom delimiter
/// let data = EuropeanData::load()
/// ```
pub struct CsvLoadFunction {
    file_path: String,
    options: CsvOptions,
}

impl FunctionImpl for CsvLoadFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype {
        // Signature: () -> List<Record>
        // Since we can't resolve the record type here, we use a type variable
        // The typechecker will infer the actual record type based on usage
        let t_var = next_type_var();
        let result_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Var(t_var),
        });

        // Create function type: () -> result_ty
        let fn_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Function(vec![], result_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, _args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        // Load CSV as LazyFrame for lazy evaluation using configured options
        // The LazyFrame will defer actual data loading until collect() is called
        let mut csv_reader = CsvReadOptions::default().with_has_header(self.options.has_header);

        // Apply custom delimiter if specified
        if self.options.delimiter != b',' {
            csv_reader =
                csv_reader.map_parse_options(|opts| opts.with_separator(self.options.delimiter));
        }

        // Apply custom quote char if specified
        if let Some(quote) = self.options.quote_char {
            csv_reader = csv_reader.map_parse_options(|opts| opts.with_quote_char(Some(quote)));
        }

        let df = csv_reader
            .try_into_reader_with_file_path(Some(self.file_path.clone().into()))
            .map_err(|e| {
                // Use PolarsError conversion
                let compile_err: RuntimeError = e.into();
                compile_err
            })?
            .finish()
            .map_err(|e| {
                let compile_err: RuntimeError = e.into();
                compile_err
            })?;

        Ok(Value::LazyFrame(df.lazy()))
    }
}
