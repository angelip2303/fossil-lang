use crate::error::CompileError;
use crate::module::ModuleRegistry;
use crate::phases::typecheck::TypeChecker;
use crate::phases::typegen::TypeGenerator;
use crate::phases::{parse::Parser, resolve::Resolver};

pub struct Compiler;

impl Compiler {
    pub fn compile(&self, src: &str) -> Result<(), CompileError> {
        let registry = ModuleRegistry::default();

        let program = Parser::parse(src)?;

        let mut resolver = Resolver::new(&registry);
        let resolved = resolver.resolve(program)?;

        let typegen = TypeGenerator::new(&registry);
        let generated = typegen.generate(resolved)?;

        let typecheck = TypeChecker::new(&registry);
        let typechecked = typecheck.check(generated)?;

        println!("{}", typechecked);

        Ok(())
    }
}
