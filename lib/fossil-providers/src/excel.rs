use std::path::{Path, PathBuf};

use calamine::{open_workbook_auto, Data, Reader};
use fossil_lang::ast::Loc;
use fossil_lang::context::DefId;
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

use crate::utils::{
    lookup_type_id, polars_schema_to_field_specs, validate_extension, validate_path,
};

const EXCEL_EXTENSIONS: &[&str] = &["xlsx", "xlsm", "xls", "xlsb", "ods"];

#[derive(Debug, Clone)]
pub struct ExcelSource {
    pub path: PathBuf,
    pub sheet: Option<String>,
    pub has_header: bool,
}

impl ExcelSource {
    pub fn new(path: PathBuf, sheet: Option<String>, has_header: bool) -> Self {
        Self { path, sheet, has_header }
    }
}

impl Source for ExcelSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        let df = range_to_dataframe(&self.path, self.sheet.as_deref(), self.has_header)
            .map_err(|e| PolarsError::ComputeError(e.into()))?;
        Ok(df.lazy())
    }
}

fn range_to_dataframe(
    path: &Path,
    sheet: Option<&str>,
    has_header: bool,
) -> Result<DataFrame, String> {
    let mut workbook =
        open_workbook_auto(path).map_err(|e| format!("Failed to open Excel file: {e}"))?;

    let range = match sheet {
        Some(name) => workbook
            .worksheet_range(name)
            .map_err(|e| format!("Failed to read sheet '{name}': {e}"))?,
        None => {
            let sheets = workbook.sheet_names().to_vec();
            if sheets.is_empty() {
                return Err("Workbook has no sheets".to_string());
            }
            workbook
                .worksheet_range(&sheets[0])
                .map_err(|e| format!("Failed to read first sheet: {e}"))?
        }
    };

    let (height, width) = range.get_size();
    if width == 0 {
        return Ok(DataFrame::empty());
    }

    let start_row = if has_header { 1 } else { 0 };

    // Build column headers
    let headers: Vec<String> = if has_header && height > 0 {
        (0..width)
            .map(|c| match range.get((0, c)) {
                Some(Data::String(s)) => s.clone(),
                Some(Data::Int(i)) => i.to_string(),
                Some(Data::Float(f)) => f.to_string(),
                _ => format!("column_{c}"),
            })
            .collect()
    } else {
        (0..width).map(|c| format!("column_{c}")).collect()
    };

    let data_rows: Vec<_> = (start_row..height)
        .map(|r| (0..width).map(|c| range.get((r, c))).collect::<Vec<_>>())
        .collect();

    // Infer column types
    #[derive(Clone, Copy, PartialEq)]
    enum ColType {
        Unknown,
        Int,
        Float,
        Bool,
        Str,
    }

    let mut col_types = vec![ColType::Unknown; width];

    for row in &data_rows {
        for (c, cell) in row.iter().enumerate() {
            let cell_type = match cell {
                Some(Data::Int(_)) => ColType::Int,
                Some(Data::Float(_)) => ColType::Float,
                Some(Data::Bool(_)) => ColType::Bool,
                Some(Data::String(_)) => ColType::Str,
                Some(Data::DateTime(_) | Data::DateTimeIso(_) | Data::DurationIso(_)) => {
                    ColType::Str
                }
                Some(Data::Error(_)) => ColType::Str,
                Some(Data::Empty) | None => continue,
            };

            col_types[c] = match (col_types[c], cell_type) {
                (ColType::Unknown, t) => t,
                (ColType::Int, ColType::Float) | (ColType::Float, ColType::Int) => ColType::Float,
                (existing, new) if existing == new => existing,
                _ => ColType::Str,
            };
        }
    }

    // Build series per column
    let columns: Vec<Column> = (0..width)
        .map(|c| {
            let col_type = if col_types[c] == ColType::Unknown {
                ColType::Str
            } else {
                col_types[c]
            };
            let name = PlSmallStr::from(headers[c].as_str());

            match col_type {
                ColType::Int => {
                    let values: Vec<Option<i64>> = data_rows
                        .iter()
                        .map(|row| match row[c] {
                            Some(Data::Int(i)) => Some(*i),
                            Some(Data::Float(f)) => Some(*f as i64),
                            _ => None,
                        })
                        .collect();
                    Column::new(name, &values)
                }
                ColType::Float => {
                    let values: Vec<Option<f64>> = data_rows
                        .iter()
                        .map(|row| match row[c] {
                            Some(Data::Float(f)) => Some(*f),
                            Some(Data::Int(i)) => Some(*i as f64),
                            _ => None,
                        })
                        .collect();
                    Column::new(name, &values)
                }
                ColType::Bool => {
                    let values: Vec<Option<bool>> = data_rows
                        .iter()
                        .map(|row| match row[c] {
                            Some(Data::Bool(b)) => Some(*b),
                            _ => None,
                        })
                        .collect();
                    Column::new(name, &values)
                }
                ColType::Str | ColType::Unknown => {
                    let values: Vec<Option<String>> = data_rows
                        .iter()
                        .map(|row| match row[c] {
                            Some(Data::String(s)) => Some(s.clone()),
                            Some(Data::Int(i)) => Some(i.to_string()),
                            Some(Data::Float(f)) => Some(f.to_string()),
                            Some(Data::Bool(b)) => Some(b.to_string()),
                            Some(Data::DateTime(f)) => Some(f.to_string()),
                            Some(Data::DateTimeIso(s)) => Some(s.clone()),
                            Some(Data::DurationIso(s)) => Some(s.clone()),
                            Some(Data::Error(e)) => Some(format!("{e:?}")),
                            Some(Data::Empty) | None => None,
                        })
                        .collect();
                    Column::new(name, &values)
                }
            }
        })
        .collect();

    DataFrame::new(columns).map_err(|e| format!("Failed to create DataFrame: {e}"))
}

pub struct ExcelProvider;

impl TypeProviderImpl for ExcelProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo {
            extensions: EXCEL_EXTENSIONS.to_vec(),
            kind: ProviderKind::Both,
        }
    }

    fn infer_schema(
        &self,
        path: polars::prelude::PlPath,
        _cloud_opts: Option<polars::prelude::cloud::CloudOptions>,
    ) -> Result<Schema, String> {
        let local = match path.as_ref() {
            PlPathRef::Local(p) => p.to_path_buf(),
            _ => return Err("Excel provider only supports local files".into()),
        };
        ExcelSource::new(local, None, true)
            .infer_schema()
            .map_err(|e| e.to_string())
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
                name: "sheet",
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
        let (_, resolved) = args.resolve_path("path", ctx.path_resolver, "excel", loc)?;
        let path = resolved.pl_path().clone();
        validate_extension(path.as_ref(), EXCEL_EXTENSIONS, loc)?;
        validate_path(path.as_ref(), loc)?;

        let local_path = match path.as_ref() {
            PlPathRef::Local(p) => p.to_path_buf(),
            _ => {
                return Err(FossilError::data_error(
                    "Excel provider only supports local files".to_string(),
                    loc,
                ))
            }
        };

        let sheet = args.get_string("sheet").map(|s| s.to_string());
        let has_header = args.get_bool("has_header").unwrap_or(true);

        let source = ExcelSource::new(local_path, sheet, has_header);
        let schema = source
            .infer_schema()
            .map_err(|e| FossilError::data_error(e.to_string(), loc))?;

        let fields = polars_schema_to_field_specs(&schema, ctx.interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new(
                "load",
                ExcelLoadFunction {
                    source,
                    type_name: type_name.to_string(),
                },
            )],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields }).with_module(module_spec))
    }
}

pub struct ExcelLoadFunction {
    source: ExcelSource,
    type_name: String,
}

impl FunctionImpl for ExcelLoadFunction {
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

    fn call(&self, _args: Vec<Value>, _type_args: &[DefId], _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let loc = fossil_lang::ast::Loc::generated();
        let frame = self.source.scan()
            .map_err(|e| FossilError::data_error(e.to_string(), loc))?;

        Ok(Value::Frame(frame))
    }
}
