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

    gcx.identity_resolver = Some(Arc::new(|def_id, ctor_args, gcx| {
        use polars::prelude::{lit, concat_str, Expr};
        use fossil_lang::runtime::value::Value;

        let metadata = gcx.type_metadata.get(&def_id)?;
        let rdf_sym = gcx.interner.lookup("rdf")?;
        let base_sym = gcx.interner.lookup("base")?;
        let attr = metadata.get_type_attribute(rdf_sym)?;
        let base = attr.get_string(base_sym, &gcx.interner)?.to_string();

        // Check for custom subject template in #[rdf(subject = "...")]
        let subject_sym = gcx.interner.lookup("subject");
        let custom_subject = subject_sym
            .and_then(|sym| attr.get_string(sym, &gcx.interner))
            .map(|s| s.to_string());

        let type_def = gcx.definitions.get(def_id);
        let type_name = gcx.interner.resolve(type_def.name).to_string();

        if let Some(template) = custom_subject {
            // Static replacements
            let resolved = template
                .replace("{base}", &base)
                .replace("{TypeName}", &type_name);

            // Split by positional placeholders {0}, {1}, ... and build concat_str
            let mut parts: Vec<Expr> = Vec::new();
            let mut remaining = resolved.as_str();

            for (i, arg) in ctor_args.iter().enumerate() {
                let placeholder = format!("{{{}}}", i);
                if let Some(pos) = remaining.find(&placeholder) {
                    let before = &remaining[..pos];
                    if !before.is_empty() {
                        parts.push(lit(before.to_string()));
                    }
                    if let Value::Expr(expr) = arg {
                        parts.push(expr.clone().cast(polars::prelude::DataType::String));
                    }
                    remaining = &remaining[pos + placeholder.len()..];
                }
            }
            if !remaining.is_empty() {
                parts.push(lit(remaining.to_string()));
            }

            return Some(Value::Expr(concat_str(parts, "", true)));
        }

        // Default: {base}{TypeName}_{arg1}_{arg2}_...

        let mut parts: Vec<Expr> = Vec::new();
        parts.push(lit(format!("{}{}", base, type_name)));

        for arg in ctor_args {
            if let Value::Expr(expr) = arg {
                parts.push(lit("_"));
                parts.push(expr.clone().cast(polars::prelude::DataType::String));
            }
        }

        Some(Value::Expr(concat_str(parts, "", true)))
    }));
}
