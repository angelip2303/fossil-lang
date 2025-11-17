use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::module::{Binding, ModuleRegistry};
use fossil_providers::csv::CsvProvider;

pub fn main() {
    let src = r#"
        type Person = Csv<"examples/csv/data.csv">;
    "#;

    let mut registry = ModuleRegistry::default();
    registry.alloc(Binding::Provider(Arc::new(CsvProvider)));
    let compiler = Compiler::new(registry);
    let _ = compiler.compile(src);
}
