pub mod csv;
pub mod excel;
pub mod turtle;
pub mod utils;

use fossil_lang::passes::GlobalContext;

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("csv", csv::CsvProvider);
    gcx.register_provider("excel", excel::ExcelProvider);
    gcx.register_provider("turtle", turtle::TurtleProvider);
}
