use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

const PROGRAM: &str = "./lib/fossil-stdlib/examples/mobility/mapping.fossil";

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut gcx = GlobalContext::default();
    fossil_providers::init(&mut gcx);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let result = compiler.compile(CompilerInput::File(PathBuf::from(PROGRAM)))?;
    let _ = IrExecutor::execute(result.program)?;
    Ok(())
}
