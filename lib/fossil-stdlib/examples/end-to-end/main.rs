use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::CompileError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::ThirExecutor;

const PROGRAM: &str = "./lib/fossil-stdlib/examples/end-to-end/end-to-end.fossil";

pub fn main() -> Result<(), CompileError> {
    let mut gcx = GlobalContext::new();
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let thir = compiler.compile(CompilerInput::File(PathBuf::from(PROGRAM)))?;
    let _ = ThirExecutor::execute(thir)?;
    Ok(())
}
