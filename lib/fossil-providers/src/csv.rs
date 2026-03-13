use fossil_lang::ast::Loc;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{
    FunctionDef, ModuleSpec, ProviderArgs, ProviderContext, ProviderInfo, ProviderKind,
    ProviderOutput, ProviderParamInfo, ProviderSchema, TypeProviderImpl,
};
use fossil_lang::traits::source::Source;

use polars::prelude::*;
use polars::prelude::cloud::CloudOptions;

use crate::utils::{lookup_type_id, polars_schema_to_field_specs, validate_extension, validate_path};

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

#[derive(Debug, Clone)]
pub struct CsvSource {
    pub path: PlPath,
    pub options: CsvOptions,
    pub cloud_options: Option<CloudOptions>,
}

impl CsvSource {
    pub fn new(path: PlPath, options: CsvOptions, cloud_options: Option<CloudOptions>) -> Self {
        Self { path, options, cloud_options }
    }
}

impl Source for CsvSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        LazyCsvReader::new(self.path.clone())
            .with_separator(self.options.delimiter)
            .with_has_header(self.options.has_header)
            .with_quote_char(self.options.quote_char)
            .with_infer_schema_length(self.options.infer_schema_length)
            .with_cloud_options(self.cloud_options.clone())
            .finish()
    }
}

pub struct CsvProvider;

impl TypeProviderImpl for CsvProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo { extensions: vec!["csv"], kind: ProviderKind::Both }
    }

    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![
            ProviderParamInfo {
                name: "path",
                required: true,
                default: None,
                expected_type: Some("string"),
            },
            ProviderParamInfo {
                name: "delimiter",
                required: false,
                default: None,
                expected_type: Some("string"),
            },
            ProviderParamInfo {
                name: "has_header",
                required: false,
                default: None,
                expected_type: Some("bool"),
            },
        ]
    }

    fn provide(
        &self,
        args: &ProviderArgs,
        ctx: &mut ProviderContext,
        type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError> {
        let (raw_path, resolved) = args.resolve_path("path", ctx.path_resolver, "csv", loc)?;
        let path = PlPath::from_str(&resolved.url);
        validate_extension(path.as_ref(), &["csv"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let mut options = CsvOptions::default();
        if let Some(delim_str) = args.get_string("delimiter") {
            if !delim_str.is_empty() {
                let bytes = delim_str.as_bytes();
                if bytes.len() != 1 || !bytes[0].is_ascii() {
                    return Err(FossilError::invalid_argument_type(
                        "delimiter", "a single ASCII character", loc,
                    ));
                }
                options.delimiter = bytes[0];
            }
        }
        if let Some(has_header) = args.get_bool("has_header") {
            options.has_header = has_header;
        }

        let csv_source = CsvSource::new(path, options.clone(), resolved.cloud_options);
        let schema = csv_source
            .infer_schema()
            .map_err(|e| FossilError::data_error(e.to_string(), loc))?;

        let fields = polars_schema_to_field_specs(&schema, ctx.interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new("load", CsvLoadFunction {
                raw_path,
                options,
                type_name: type_name.to_string(),
                loc,
            })],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields })
            .with_module(module_spec))
    }
}

pub struct CsvLoadFunction {
    raw_path: String,
    options: CsvOptions,
    type_name: String,
    loc: Loc,
}

impl FunctionImpl for CsvLoadFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        let result_ty = match lookup_type_id(&self.type_name, gcx) {
            Some(type_id) => ir.named_type(type_id),
            None => ir.var_type(next_type_var()),
        };

        Polytype::mono(ir.fn_type(vec![], result_ty))
    }

    fn call(&self, _args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let resolved = ctx.gcx.path_resolver.resolve(&self.raw_path)
            .map_err(|e| FossilError::data_error(e, self.loc))?;

        let source = CsvSource {
            path: PlPath::from_str(&resolved.url),
            options: self.options.clone(),
            cloud_options: resolved.cloud_options,
        };

        let frame = source.scan()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;

        Ok(Value::Frame(frame))
    }
}
