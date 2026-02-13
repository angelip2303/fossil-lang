use fossil_lang::ast::Loc;
use fossil_lang::context::Interner;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{
    FunctionDef, ModuleSpec, ProviderArgs, ProviderOutput, ProviderParamInfo, ProviderSchema,
    TypeProviderImpl,
};
use fossil_lang::traits::source::Source;

use polars::prelude::*;

use crate::utils::{lookup_type_id, polars_schema_to_field_specs, resolve_path, validate_extension, validate_path};

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
}

impl CsvSource {
    pub fn new(path: PlPath, options: CsvOptions) -> Self {
        Self { path, options }
    }

    pub fn infer_schema(&self) -> PolarsResult<Schema> {
        self.to_lazy_frame()?
            .collect_schema()
            .map(|arc| arc.as_ref().clone())
    }
}

impl Source for CsvSource {
    fn to_lazy_frame(&self) -> PolarsResult<LazyFrame> {
        LazyCsvReader::new(self.path.clone())
            .with_separator(self.options.delimiter)
            .with_has_header(self.options.has_header)
            .with_quote_char(self.options.quote_char)
            .with_infer_schema_length(self.options.infer_schema_length)
            .finish()
    }

    fn box_clone(&self) -> Box<dyn Source> {
        Box::new(self.clone())
    }
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
                default: None,
            },
            ProviderParamInfo {
                name: "has_header",
                required: false,
                default: None,
            },
        ]
    }

    fn provide(
        &self,
        args: &ProviderArgs,
        interner: &mut Interner,
        type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError> {
        let path_str = args.require_string("path", "csv", loc)?;
        let path = resolve_path(path_str);
        validate_extension(path.as_ref(), &["csv"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let mut options = CsvOptions::default();
        if let Some(delim_str) = args.get_string("delimiter") {
            if !delim_str.is_empty() {
                options.delimiter = delim_str.as_bytes()[0];
            }
        }
        if let Some(has_header) = args.get_bool("has_header") {
            options.has_header = has_header;
        }

        let csv_source = CsvSource::new(path, options);
        let schema = csv_source
            .infer_schema()
            .map_err(|e| FossilError::data_error(e.to_string(), loc))?;

        let fields = polars_schema_to_field_specs(&schema, interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new("load", CsvLoadFunction {
                source: csv_source,
                type_name: type_name.to_string(),
                loc,
            })],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields })
            .with_module(module_spec)
            .as_data())
    }
}

pub struct CsvLoadFunction {
    source: CsvSource,
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
        use fossil_lang::runtime::value::Plan;

        let schema = self
            .source
            .infer_schema()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;

        let type_def_id = ctx
            .gcx
            .interner
            .lookup(&self.type_name)
            .and_then(|sym| ctx.gcx.definitions.get_by_symbol(sym).map(|d| d.id()));

        let plan = match type_def_id {
            Some(def_id) => Plan::from_source_with_type(self.source.box_clone(), schema, def_id),
            None => Plan::from_source(self.source.box_clone(), schema),
        };

        Ok(Value::Plan(plan))
    }
}
