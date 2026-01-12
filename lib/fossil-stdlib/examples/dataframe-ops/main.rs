use fossil_lang::compiler::Compiler;
use fossil_lang::error::CompileError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::ThirExecutor;
use std::sync::Arc;

const PROGRAM: &str = "lib/fossil-stdlib/examples/dataframe-ops/dataframe-ops.fossil";

pub fn main() -> Result<(), CompileError> {
    let src = std::fs::read_to_string(PROGRAM).unwrap();

    let mut gcx = GlobalContext::new();
    gcx.register_provider("csv", Arc::new(fossil_providers::csv::CsvProvider));
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let thir = compiler.compile(&src)?;
    let _ = ThirExecutor::execute(thir)?;
    Ok(())
}
