use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};

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

use oxttl::TurtleParser;
use polars::prelude::*;

use crate::utils::{
    lookup_type_id, polars_schema_to_field_specs, validate_extension, validate_path,
};

const TURTLE_EXTENSIONS: &[&str] = &["ttl"];

#[derive(Debug, Clone)]
pub struct TurtleSource {
    pub path: PathBuf,
}

impl TurtleSource {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Source for TurtleSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        let df = parse_turtle_to_frame(&self.path)
            .map_err(|e| PolarsError::ComputeError(e.into()))?;
        Ok(df.lazy())
    }

    /// Turtle has a fixed triple shape — return the schema without parsing
    /// the file. Keeps compile-time provider resolution O(1).
    fn infer_schema(&self) -> PolarsResult<Schema> {
        let mut schema = Schema::with_capacity(3);
        schema.with_column("subject".into(), DataType::String);
        schema.with_column("predicate".into(), DataType::String);
        schema.with_column("object".into(), DataType::String);
        Ok(schema)
    }
}

fn parse_turtle_to_frame(path: &Path) -> Result<DataFrame, String> {
    let file = File::open(path).map_err(|e| format!("Failed to open Turtle file: {e}"))?;
    let parser = TurtleParser::new().for_reader(BufReader::new(file));

    let mut subjects: Vec<String> = Vec::new();
    let mut predicates: Vec<String> = Vec::new();
    let mut objects: Vec<String> = Vec::new();

    for triple in parser {
        let triple = triple.map_err(|e| format!("Turtle parse error: {e}"))?;
        subjects.push(triple.subject.to_string());
        predicates.push(triple.predicate.to_string());
        objects.push(triple.object.to_string());
    }

    let columns = vec![
        Column::new(PlSmallStr::from("subject"), &subjects),
        Column::new(PlSmallStr::from("predicate"), &predicates),
        Column::new(PlSmallStr::from("object"), &objects),
    ];
    DataFrame::new(columns).map_err(|e| format!("Failed to build DataFrame: {e}"))
}

pub struct TurtleProvider;

impl TypeProviderImpl for TurtleProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo {
            extensions: vec!["ttl"],
            kind: ProviderKind::Both,
        }
    }

    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![ProviderParamInfo {
            name: "path",
            required: true,
            default: None,
            expected_type: Some("string"),
        }]
    }

    fn provide(
        &self,
        args: &ProviderArgs,
        ctx: &mut ProviderContext,
        type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError> {
        let (raw_path, resolved) = args.resolve_path("path", ctx.path_resolver, "turtle", loc)?;
        let path = resolved.pl_path().clone();
        validate_extension(path.as_ref(), TURTLE_EXTENSIONS, loc)?;
        validate_path(path.as_ref(), loc)?;

        let local_path = match path.as_ref() {
            PlPathRef::Local(p) => p.to_path_buf(),
            _ => {
                return Err(FossilError::data_error(
                    "Turtle provider only supports local files".to_string(),
                    loc,
                ))
            }
        };

        let source = TurtleSource::new(local_path);
        let schema = source
            .infer_schema()
            .map_err(|e| FossilError::data_error(e.to_string(), loc))?;

        let fields = polars_schema_to_field_specs(&schema, ctx.interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new(
                "load",
                TurtleLoadFunction {
                    raw_path,
                    type_name: type_name.to_string(),
                    loc,
                },
            )],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields }).with_module(module_spec))
    }
}

pub struct TurtleLoadFunction {
    raw_path: String,
    type_name: String,
    loc: Loc,
}

impl FunctionImpl for TurtleLoadFunction {
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
        let resolved = ctx
            .gcx
            .path_resolver
            .resolve(&self.raw_path)
            .map_err(|e| FossilError::data_error(e, self.loc))?;

        let local_path = match resolved.pl_path().as_ref() {
            PlPathRef::Local(p) => p.to_path_buf(),
            _ => {
                return Err(FossilError::data_error(
                    "Turtle provider only supports local files".to_string(),
                    self.loc,
                ))
            }
        };

        let source = TurtleSource::new(local_path);
        let frame = source
            .scan()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;
        Ok(Value::Frame(frame))
    }
}
