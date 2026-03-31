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

/// Parse a DOCX file into a list of semantic elements as a Polars DataFrame.
fn parse_docx(path: &std::path::Path) -> Result<DataFrame, String> {
    let bytes = std::fs::read(path)
        .map_err(|e| format!("failed to read DOCX: {e}"))?;

    let docx = docx_rs::read_docx(&bytes)
        .map_err(|e| format!("failed to parse DOCX: {e}"))?;

    let mut ids = Vec::new();
    let mut types = Vec::new();
    let mut texts = Vec::new();
    let mut page_nums: Vec<Option<i64>> = Vec::new();
    let mut sections: Vec<Option<String>> = Vec::new();
    let mut metadata = Vec::new();

    let mut current_section: Option<String> = None;

    for child in &docx.document.children {
        if let docx_rs::DocumentChild::Paragraph(para) = child {
            // Extract text from all runs in the paragraph
            let text: String = para
                .children
                .iter()
                .filter_map(|child| {
                    if let docx_rs::ParagraphChild::Run(run) = child {
                        Some(
                            run.children
                                .iter()
                                .filter_map(|rc| {
                                    if let docx_rs::RunChild::Text(t) = rc {
                                        Some(t.text.as_str())
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(""),
                        )
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .join("");

            let text = text.trim().to_string();
            if text.is_empty() {
                continue;
            }

            // Detect element type from paragraph style
            let style_id = para
                .property
                .style
                .as_ref()
                .map(|s| s.val.as_str())
                .unwrap_or("");

            let element_type = if style_id.starts_with("Heading") || style_id.starts_with("heading")
            {
                current_section = Some(text.clone());
                element::ELEMENT_TITLE
            } else if style_id == "ListParagraph" || style_id.contains("List") {
                element::ELEMENT_LIST_ITEM
            } else {
                element::ELEMENT_NARRATIVE_TEXT
            };

            ids.push(uuid::Uuid::new_v4().to_string());
            types.push(element_type.to_string());
            texts.push(text);
            page_nums.push(None); // DOCX doesn't have reliable page info without rendering
            sections.push(current_section.clone());
            metadata.push("{}".to_string());
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
pub struct DocxSource {
    pub path: String,
}

impl Source for DocxSource {
    fn scan(&self) -> PolarsResult<LazyFrame> {
        let path = std::path::Path::new(&self.path);
        let df =
            parse_docx(path).map_err(|e| polars::error::PolarsError::ComputeError(e.into()))?;
        Ok(df.lazy())
    }
}

pub struct DocxProvider;

impl TypeProviderImpl for DocxProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo {
            extensions: vec!["docx"],
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
        let (raw_path, resolved) = args.resolve_path("path", ctx.path_resolver, "docx", loc)?;
        let path = resolved.pl_path().clone();
        validate_extension(path.as_ref(), &["docx"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let fields = element_field_specs(ctx.interner);

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new(
                "load",
                DocxLoadFunction {
                    raw_path,
                    type_name: type_name.to_string(),
                    loc,
                },
            )],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields }).with_module(module_spec))
    }
}

pub struct DocxLoadFunction {
    raw_path: String,
    type_name: String,
    loc: Loc,
}

impl FunctionImpl for DocxLoadFunction {
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

        let source = DocxSource {
            path: resolved.to_str().to_string(),
        };
        let frame = source
            .scan()
            .map_err(|e| FossilError::data_error(e.to_string(), self.loc))?;

        Ok(Value::Frame(frame))
    }
}
