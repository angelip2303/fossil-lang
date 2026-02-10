use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

const PROGRAM: &str = "./lib/fossil-ifc/examples/basic-sample/mapping.fossil";

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut gcx = GlobalContext::default();
    fossil_ifc::register(&mut gcx);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::new(gcx);
    let result = compiler.compile(CompilerInput::File(PathBuf::from(PROGRAM)))?;
    let _ = IrExecutor::execute(result.program)?;
    Ok(())
}
