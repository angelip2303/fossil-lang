use crate::error::CompileError;
use crate::module::ModuleRegistry;
use crate::phases::typecheck::TypeChecker;
use crate::phases::typegen::TypeGenerator;
use crate::phases::{parse::Parser, resolve::Resolver};

#[derive(Default)]
pub struct Compiler {
    registry: ModuleRegistry,
}

impl Compiler {
    pub fn new(registry: ModuleRegistry) -> Self {
        Self { registry }
    }

    pub fn compile(&self, src: &str) -> Result<(), CompileError> {
        let program = Parser::parse(src)?;

        let mut resolver = Resolver::new(&self.registry);
        let resolved = resolver.resolve(program)?;

        let typegen = TypeGenerator::new(&self.registry);
        let generated = typegen.generate(resolved)?;

        let typecheck = TypeChecker::new(&self.registry);
        let typechecked = typecheck.check(generated)?;

        println!("{}", typechecked);

        Ok(())
    }
}
