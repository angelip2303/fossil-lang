pub mod docx;
pub mod element;
pub mod pdf;

use fossil_lang::passes::GlobalContext;

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("pdf", pdf::PdfProvider);
    gcx.register_provider("docx", docx::DocxProvider);
}
