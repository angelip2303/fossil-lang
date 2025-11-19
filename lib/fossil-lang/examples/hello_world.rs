use fossil_lang::compiler::Compiler;
use fossil_lang::error::FossilError;
use fossil_lang::module::ModuleRegistry;
use fossil_lang::runtime::interpreter::Interpreter;

pub fn main() -> Result<(), FossilError> {
    let src = r#"
        open IO as io
        io::println("Hello, World!")
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    let program = compiler.compile(src)?;
    let mut interpreter = Interpreter::new(&registry);
    interpreter.execute(program)?;

    Ok(())
}
