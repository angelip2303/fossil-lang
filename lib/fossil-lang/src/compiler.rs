use chumsky::prelude::*;
use logos::Logos;

use crate::checked::CheckedAst;
use crate::error::CompileError;
use crate::generator::TypeGenerator;
use crate::module::ModuleRegistry;
use crate::parser::{self, Token};
use crate::resolver::Resolver;
use crate::typechecker::TypeChecker;

#[derive(Default)]
pub struct Compiler {
    registry: ModuleRegistry,
}

impl Compiler {
    pub fn new(registry: ModuleRegistry) -> Self {
        Self { registry }
    }

    /// Compile source code through all phases
    pub fn compile(&self, src: &str) -> Result<CheckedAst, CompileError> {
        // Phase 1: Lexer
        let tokens: Vec<_> = Token::lexer(src).filter_map(|t| t.ok()).collect();

        // Phase 2: Parsing (syntax analysis)
        let context = parser::AstCtx::default();

        parser::decls(&context)
            .repeated()
            .collect::<Vec<_>>()
            .parse(tokens.as_slice())
            .into_result()
            .map_err(|err| {
                eprintln!("{err:?}");
                todo!()
            })
            .unwrap();

        let ast = context.take();

        println!("{:?}", ast);

        // Phase 3: Lowering (name resolution)
        let lowerer = Resolver::new(self.registry.clone());
        let resolved = lowerer.resolve(ast)?;

        println!("{:?}", resolved);

        // Phase 4: Type Generation (execute type providers)
        let typegen = TypeGenerator::new(self.registry.clone());
        let concrete = typegen.generate(resolved)?;

        println!("{:?}", concrete);

        // Phase 4: Type Checking (Hindley-Milner inference)
        let checker = TypeChecker::new(concrete, self.registry.clone());
        let checked = checker.check()?;

        println!("{:?}", checked);

        Ok(checked)
    }
}
