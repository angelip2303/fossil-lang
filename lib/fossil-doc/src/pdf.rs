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
use fossil_providers::utils::{lookup_type_id, validate_extension, validate_path};

use polars::prelude::*;

use crate::element::{self, element_field_specs};

/// Parse a PDF file into a list of semantic elements as a Polars DataFrame.
fn parse_pdf(path: &std::path::Path) -> Result<DataFrame, String> {
    let bytes = std::fs::read(path)
        .map_err(|e| format!("failed to read PDF: {e}"))?;

    let pdfium = pdfium_render::prelude::Pdfium::default();
    let document = pdfium
        .load_pdf_from_byte_slice(&bytes, None)
        .map_err(|e| format!("failed to parse PDF: {e}"))?;

    let mut ids = Vec::new();
    let mut types = Vec::new();
    let mut texts = Vec::new();
    let mut page_nums: Vec<Option<i64>> = Vec::new();
    let mut sections: Vec<Option<String>> = Vec::new();
    let mut metadata = Vec::new();

    let mut current_section: Option<String> = None;

    for (page_idx, page) in document.pages().iter().enumerate() {
        let page_num = (page_idx + 1) as i64;
        let page_text = page.text().map(|t| t.all()).unwrap_or_default();

        // Split page text by double-newlines to approximate paragraphs
        let paragraphs: Vec<&str> = page_text
            .split("\n\n")
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect();

        for para in paragraphs {
            let element_id = uuid::Uuid::new_v4().to_string();

            // Heuristic: short text in ALL CAPS or ending with colon → Title
            let element_type = if para.len() < 100
                && (para.chars().filter(|c| c.is_alphabetic()).all(|c| c.is_uppercase())
                    || para.ends_with(':'))
            {
                current_section = Some(para.to_string());
                element::ELEMENT_TITLE
            } else {
                element::ELEMENT_NARRATIVE_TEXT
            };

            ids.push(element_id);
            types.push(element_type.to_string());
            texts.push(para.to_string());
            page_nums.push(Some(page_num));
            sections.push(current_section.clone());
            metadata.push(format!(r#"{{"page":{page_num}}}"#));
        }
    }

    DataFrame::new(vec![
        Column::new(element::COL_ELEMENT_ID.into(), ids),
        Column::new(element::COL_ELEMENT_TYPE.into(), types),
        Column::new(element::COL_TEXT.into(), texts),
        Column::new(element::COL_PAGE_NUM.into(), page_nums),
        Column::new(element::COL_SECTION.into(), sections),
        Column::new(element::COL_METADATA.into(), metadata),
    ])
    .map_err(|e| format!("failed to build DataFrame: {e}"))
}

#[derive(Debug, Clone)]
pub struct PdfSource {
    pub path: String,
}

impl Source for PdfSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        let path = std::path::Path::new(&self.path);
        let df = parse_pdf(path).map_err(|e| polars::error::PolarsError::ComputeError(e.into()))?;
        Ok(df.lazy())
    }
}

pub struct PdfProvider;

impl TypeProviderImpl for PdfProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo {
            extensions: vec!["pdf"],
            kind: ProviderKind::Data,
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
        let (raw_path, resolved) = args.resolve_path("path", ctx.path_resolver, "pdf", loc)?;
        let path = resolved.pl_path().clone();
        validate_extension(path.as_ref(), &["pdf"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let fields = element_field_specs(ctx.interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new(
                "load",
                PdfLoadFunction {
                    raw_path,
                    type_name: type_name.to_string(),
                    loc,
                },
            )],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields }).with_module(module_spec))
    }
}

pub struct PdfLoadFunction {
    raw_path: String,
    type_name: String,
    loc: Loc,
}

impl FunctionImpl for PdfLoadFunction {
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

    fn call(&self, _args: Vec<Value>, _type_args: &[fossil_lang::context::DefId], ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let resolved = ctx
            .gcx
            .path_resolver
            .resolve(&self.raw_path)
            .map_err(|e| FossilError::data_error(e, self.loc))?;

        let source = PdfSource {
            path: resolved.to_str().to_string(),
        };
        let frame = source
            .scan()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;

        Ok(Value::Frame(frame))
    }
}
