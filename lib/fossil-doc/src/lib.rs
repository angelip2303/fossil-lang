//! Fossil document providers (PDF, DOCX).
//!
//! Dual API: legacy init() + new register() for FunctionDef.
//! Parsing functions (parse_pdf, parse_docx) are exported as library
//! functions for the host executor to call.

pub mod docx;
pub mod element;
pub mod pdf;

use fossil_lang::passes::GlobalContext;
use fossil_lang::registry::{FunctionDef, ParamDef, Registry};

// ── Legacy init ──────────────────────────────────────────────────────

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("pdf", pdf::PdfProvider);
    gcx.register_provider("docx", docx::DocxProvider);
}

// ── New: const FunctionDef registrations ─────────────────────────────

pub const PDF_DEF: FunctionDef = FunctionDef::preprocess(
    "pdf",
    "pdf_extract",
    &[ParamDef::required("path")],
);

pub const DOCX_DEF: FunctionDef = FunctionDef::preprocess(
    "docx",
    "docx_extract",
    &[ParamDef::required("path")],
);

pub const DOC_SOURCES: &[FunctionDef] = &[PDF_DEF, DOCX_DEF];

pub fn register(r: &mut Registry) {
    r.add_functions(DOC_SOURCES);
}
