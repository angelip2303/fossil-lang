use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::module::ModuleRegistry;
use fossil_providers::csv::CsvProvider;

pub fn main() {
    let src = r#"
        type Person = data::Csv<"lib/fossil-providers/examples/csv/data.csv">;
    "#;

    let mut registry = ModuleRegistry::default();

    registry
        .module("data")
        .provider("Csv", Arc::new(CsvProvider))
        .done();

    let compiler = Compiler::new(&registry);
    let _ = compiler.compile(src);
}
