use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::CompileError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

const PROGRAM: &str = "lib/fossil-stdlib/examples/sql-ops/sql-ops.fossil";

pub fn main() -> Result<(), CompileError> {
    let mut gcx = GlobalContext::new();
    gcx.register_provider("sql", fossil_providers::sql::SqlProvider);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let program = compiler.compile(CompilerInput::File(PathBuf::from(PROGRAM)))?;
    let _ = IrExecutor::execute(program)?;
    Ok(())
}
