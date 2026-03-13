pub mod clean;
pub mod rdf;
pub mod string;

pub use rdf::RdfMaterializeFunction;

use fossil_lang::passes::GlobalContext;
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_module("Rdf", ModuleSpec {
        functions: vec![
            FunctionDef::new("materialize", RdfMaterializeFunction),
        ],
    });

    gcx.module_generators.push(clean::clean_module_generator());
}
