pub mod rdf;
pub mod report;
pub mod string;
pub mod validate;

pub use rdf::{RdfMetadata, RdfSerializeFunction};
pub use report::ReportCsvFunction;

use std::sync::Arc;

use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::common::PrimitiveType;
use fossil_lang::passes::GlobalContext;
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_module("Rdf", ModuleSpec {
        functions: vec![FunctionDef::new("serialize", RdfSerializeFunction)],
    });
    gcx.register_module("Report", ModuleSpec {
        functions: vec![FunctionDef::new("csv", ReportCsvFunction)],
    });
    gcx.register_module("String", ModuleSpec {
        functions: vec![
            FunctionDef::new("extract", string::StringExtractFunction),
            FunctionDef::new("replace", string::StringReplaceFunction),
            FunctionDef::new("replace_all", string::StringReplaceAllFunction),
        ],
    });

    let validation_error_def_id = gcx.register_record_type_with_optionality(
        "ValidationError",
        vec![
            ("source_type", BuiltInFieldType::Required(PrimitiveType::String)),
            ("field", BuiltInFieldType::Required(PrimitiveType::String)),
            ("constraint", BuiltInFieldType::Required(PrimitiveType::String)),
            ("expected", BuiltInFieldType::Required(PrimitiveType::String)),
            ("actual", BuiltInFieldType::Required(PrimitiveType::String)),
        ],
    );

    gcx.module_generators
        .push(validate::validate_module_generator(validation_error_def_id));

    gcx.ref_resolver = Some(Arc::new(|def_id, gcx| {
        let metadata = gcx.type_metadata.get(&def_id)?;
        let rdf_sym = gcx.interner.lookup("rdf")?;
        let base_sym = gcx.interner.lookup("base")?;
        let attr = metadata.get_type_attribute(rdf_sym)?;
        Some(attr.get_string(base_sym, &gcx.interner)?.to_string())
    }));
}
