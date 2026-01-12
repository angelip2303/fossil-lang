use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::passes::GlobalContext;
use fossil_providers::csv::CsvProvider;

pub fn main() {
    let src = r#"
        type Person = csv<"lib/fossil-providers/examples/csv/data.csv">
    "#;

    // Create GlobalContext and register CSV provider (F# style)
    let mut gcx = GlobalContext::new();
    gcx.register_provider("csv", Arc::new(CsvProvider));

    // Create compiler with custom GlobalContext
    let compiler = Compiler::with_context(gcx);

    match compiler.compile(src) {
        Ok(_thir) => println!("✓ CSV provider example compiled successfully"),
        Err(e) => eprintln!("✗ Compilation failed: {:?}", e),
    }
}
