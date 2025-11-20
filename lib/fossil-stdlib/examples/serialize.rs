use std::sync::Arc;

use fossil_lang::compiler::Compiler;
use fossil_lang::error::FossilError;
use fossil_lang::module::ModuleRegistry;
use fossil_lang::runtime::interpreter::Interpreter;
use fossil_stdlib::builtin::CsvWriteFunction;

pub fn main() -> Result<(), FossilError> {
    let src = r#"
        let data = [{
            name = "John",
            surname = "Doe"
        }, {
            name = "Jane",
            surname = "Doe"
        }];

        csv::write(data, "tmp/out.csv")
    "#;

    let mut registry = ModuleRegistry::default();

    registry
        .module("csv")
        .function("write", Arc::new(CsvWriteFunction))
        .done();

    let compiler = Compiler::new(&registry);
    let program = compiler.compile(src)?;
    let mut interpreter = Interpreter::new(&registry);
    interpreter.execute(program)?;

    Ok(())
}
