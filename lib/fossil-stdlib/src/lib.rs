pub mod clean;
pub mod rdf;
pub mod report;
pub mod string;

pub use rdf::RdfSerializeFunction;
pub use report::ReportCsvFunction;

use fossil_lang::passes::GlobalContext;
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_module("Rdf", ModuleSpec {
        functions: vec![FunctionDef::new("serialize", RdfSerializeFunction)],
    });
    gcx.register_module("Report", ModuleSpec {
        functions: vec![FunctionDef::new("csv", ReportCsvFunction)],
    });

    gcx.module_generators.push(clean::clean_module_generator());
}
