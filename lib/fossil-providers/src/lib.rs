pub mod cloud_options;
pub mod csv;
pub mod excel;
pub mod shapes;
pub mod utils;

use fossil_lang::passes::GlobalContext;

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("csv", csv::CsvProvider);
    gcx.register_provider("excel", excel::ExcelProvider);
    gcx.register_provider("shex", shapes::shex::ShexProvider);
}
