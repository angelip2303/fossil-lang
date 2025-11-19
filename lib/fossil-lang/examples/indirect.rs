use fossil_lang::compiler::Compiler;
use fossil_lang::error::FossilError;
use fossil_lang::module::ModuleRegistry;
use fossil_lang::runtime::interpreter::Interpreter;

pub fn main() -> Result<(), FossilError> {
    let src = r#"
        let to_list = fun (x) -> [x]
        to_list(42)
        to_list([1, 2, 3])
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    let program = compiler.compile(src)?;
    let mut interpreter = Interpreter::new(&registry);
    interpreter.execute(program)?;

    Ok(())
}
