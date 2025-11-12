pub mod ast;
pub mod context;
pub mod module;
pub mod resolved;
pub mod traits;
pub mod typechecker;

pub fn compile(src: &str) -> Result<(), String> {
    use crate::ast::Token;
    use chumsky::prelude::*;
    use logos::Logos;

    let tokens: Vec<_> = Token::lexer(src).filter_map(|t| t.ok()).collect();

    let context = ast::AstCtx::default();

    ast::decls(&context)
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

    let resolver = resolved::Resolver::new(module::ModuleRegistry::new());
    let ir = resolver.resolve(ast).map_err(|_| todo!()).unwrap();

    let typechecker = typechecker::TypeChecker::new(ir, module::ModuleRegistry::new());
    let types = typechecker.check().map_err(|_| todo!()).unwrap();

    println!("{types:?}");

    Ok(())
}
