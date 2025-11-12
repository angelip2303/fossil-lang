pub mod ast;
pub mod context;
pub mod ir;
pub mod module;
pub mod traits;
pub mod typechecker;

pub fn compile(src: &str) -> Result<(), String> {
    use crate::ast::Token;
    use chumsky::prelude::*;
    use logos::Logos;

    let registry = module::ModuleRegistry::new();

    let tokens: Vec<_> = Token::lexer(src).filter_map(|t| t.ok()).collect();

    let context = ast::AstCtx::default();

    ast::module(&context)
        .parse(tokens.as_slice())
        .into_result()
        .map_err(|err| {
            eprintln!("{err:?}");
            todo!()
        })
        .unwrap();
    let ast = context.take();

    let resolver = ir::Resolver::new(registry);
    let ir = resolver.resolve(ast).map_err(|err| todo!()).unwrap();

    let typechecker = typechecker::TypeChecker::new(ir);
    let types = typechecker.check().map_err(|err| todo!()).unwrap();

    println!("{types:?}");

    Ok(())
}
