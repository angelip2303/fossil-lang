use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::passes::GlobalContext;
use fossil_providers::json::JsonProvider;

pub fn main() {
    let src = r#"
        type Person = json<"lib/fossil-providers/examples/json/data.json">
    "#;

    // Create GlobalContext and register JSON provider (F# style)
    let mut gcx = GlobalContext::new();
    gcx.register_provider("json", Arc::new(JsonProvider));

    // Create compiler with custom GlobalContext
    let compiler = Compiler::with_context(gcx);

    match compiler.compile(src) {
        Ok(_thir) => println!("✓ JSON provider example compiled successfully"),
        Err(e) => eprintln!("✗ Compilation failed: {:?}", e),
    }
}
