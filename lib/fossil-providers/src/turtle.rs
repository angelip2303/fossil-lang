use std::io::{BufReader, Read};

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
use fossil_lang::traits::resolver::ResolvedPath;
use fossil_lang::traits::source::Source;

use oxttl::TurtleParser;
use polars::prelude::*;

use crate::utils::{
    lookup_type_id, polars_schema_to_field_specs, validate_extension, validate_path,
};

const TURTLE_EXTENSIONS: &[&str] = &["ttl"];

#[derive(Debug, Clone)]
pub struct TurtleSource {
    pub resolved: ResolvedPath,
}

impl TurtleSource {
    pub fn new(resolved: ResolvedPath) -> Self {
        Self { resolved }
    }
}

impl Source for TurtleSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        let reader =
            open_for_read(&self.resolved).map_err(|e| PolarsError::ComputeError(e.into()))?;
        let df = parse_turtle_to_frame(reader)
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

/// Open a resolved path for reading — local file via `File`, cloud object
/// downloaded eagerly into memory via Polars' object_store wiring so the
/// `CloudOptions` carrying host credentials (keasy cloud-accounts) apply.
fn open_for_read(resolved: &ResolvedPath) -> Result<Box<dyn Read + Send>, String> {
    match resolved.pl_path().as_ref() {
        PlPathRef::Local(p) => {
            let f = std::fs::File::open(p).map_err(|e| format!("open {}: {e}", p.display()))?;
            Ok(Box::new(f))
        }
        _ => {
            let bytes = download_cloud(resolved)?;
            Ok(Box::new(std::io::Cursor::new(bytes)))
        }
    }
}

fn download_cloud(resolved: &ResolvedPath) -> Result<Vec<u8>, String> {
    use polars::io::cloud::{build_object_store, object_path_from_str};
    use polars::io::pl_async::get_runtime;

    let plpath = resolved.pl_path().clone();
    let options = resolved.cloud_options().cloned();

    let runtime = get_runtime();
    runtime
        .block_on(async move {
            let (loc, store) =
                build_object_store(plpath.as_ref(), options.as_ref(), false).await?;
            let path = object_path_from_str(&loc.prefix)?;
            let meta = store.head(&path).await?;
            let bytes = store.get_range(&path, 0..meta.size as usize).await?;
            PolarsResult::Ok(bytes.to_vec())
        })
        .map_err(|e: PolarsError| format!("cloud download: {e}"))
}

fn parse_turtle_to_frame<R: Read>(reader: R) -> Result<DataFrame, String> {
    let parser = TurtleParser::new().for_reader(BufReader::new(reader));

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

        // Schema is static (subject, predicate, object) so we don't need to
        // touch the file at compile time, even for cloud paths.
        let source = TurtleSource::new(resolved.clone());
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

        let source = TurtleSource::new(resolved);
        let frame = source
            .scan()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;
        Ok(Value::Frame(frame))
    }
}

