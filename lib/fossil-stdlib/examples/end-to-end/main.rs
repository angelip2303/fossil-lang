use fossil_lang::compiler::Compiler;
use fossil_lang::error::CompileError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::ThirExecutor;
use std::sync::Arc;

const PROGRAM: &str = "lib/fossil-stdlib/examples/end-to-end/end-to-end.fossil";

pub fn main() -> Result<(), CompileError> {
    let src = std::fs::read_to_string(PROGRAM).unwrap();

    let mut gcx = GlobalContext::new();

    // Register CSV provider
    gcx.register_provider("csv", Arc::new(fossil_providers::csv::CsvProvider));

    // Register stdlib functions
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    match compiler.compile(&src) {
        Ok(thir) => {
            let _ = ThirExecutor::execute(thir)?;
            Ok(())
        }
        Err(e) => {
            eprintln!("Compilation error: {:?}", e);
            Err(e)
        }
    }
}
