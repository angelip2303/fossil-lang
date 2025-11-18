use crate::error::CompileError;
use crate::module::ModuleRegistry;
use crate::phases::TypedProgram;
use crate::phases::typecheck::TypeChecker;
use crate::phases::typegen::TypeGenerator;
use crate::phases::{parse::Parser, resolve::Resolver};

pub struct Compiler<'a> {
    registry: &'a ModuleRegistry,
}

impl<'a> Compiler<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Compiler<'a> {
        Compiler { registry }
    }

    pub fn compile(&self, src: &str) -> Result<TypedProgram, CompileError> {
        let program = Parser::parse(src)?;

        let mut resolver = Resolver::new(&self.registry);
        let resolved = resolver.resolve(program)?;

        let typegen = TypeGenerator::new(&self.registry);
        let generated = typegen.generate(resolved)?;

        let typecheck = TypeChecker::new(&self.registry);
        let typechecked = typecheck.check(generated)?;

        Ok(typechecked)
    }
}
