use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::module::ModuleRegistry;
use fossil_providers::json::JsonProvider;

pub fn main() {
    let src = r#"
        type Person = data::Json<"lib/fossil-providers/examples/json/data.json">
    "#;

    let mut registry = ModuleRegistry::default();

    registry
        .module("data")
        .provider("Json", Arc::new(JsonProvider))
        .done();

    let compiler = Compiler::new(&registry);
    let _ = compiler.compile(src);
}
