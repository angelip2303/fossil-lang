pub mod ops;
pub mod rdf;
pub mod string;

pub use rdf::RdfMaterializeFunction;

use fossil_lang::passes::GlobalContext;
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

use ops::make_module_generator;

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_module("Rdf", ModuleSpec {
        functions: vec![
            FunctionDef::new("materialize", RdfMaterializeFunction),
        ],
    });

    gcx.module_generators.push(make_module_generator("clean", ops::clean::clean_functions));
    gcx.module_generators.push(make_module_generator("anon", ops::anon::anon_functions));
}
