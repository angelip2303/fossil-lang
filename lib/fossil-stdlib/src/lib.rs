pub mod rdf;
pub mod report;
pub mod string;
pub mod validate;

pub use rdf::RdfSerializeFunction;
pub use report::ReportCsvFunction;

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
            FunctionDef::new("replace", string::StringReplaceFunction),
            FunctionDef::new("trim", string::StringTrimFunction),
            FunctionDef::new("upper", string::StringUpperFunction),
            FunctionDef::new("lower", string::StringLowerFunction),
            FunctionDef::new("length", string::StringLengthFunction),
            FunctionDef::new("contains", string::StringContainsFunction),
            FunctionDef::new("starts_with", string::StringStartsWithFunction),
            FunctionDef::new("ends_with", string::StringEndsWithFunction),
            FunctionDef::new("slug", string::StringSlugFunction),
            FunctionDef::new("concat", string::StringConcatFunction),
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
}
