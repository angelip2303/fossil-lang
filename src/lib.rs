pub mod ast;
pub mod const_eval;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod module;
pub mod parser;
pub mod providers;
pub mod runtime;
pub mod solver;

pub fn compile(source: &str) -> error::Result<()> {
    use std::sync::Arc;

    use chumsky::prelude::*;
    use lexer::Token;
    use logos::Logos;
    use parser::AstContext;
    use solver::{Context, Globals, TypeVarGen};

    let tokens: Vec<_> = Token::lexer(source).filter_map(|t| t.ok()).collect();

    #[cfg(debug_assertions)]
    println!("Tokens: {:?}", tokens);

    let ast_ctx = AstContext::default();
    let parser = parser::statement_parser(&ast_ctx)
        .repeated()
        .collect::<Vec<_>>();

    let stmts = parser
        .parse(tokens.as_slice())
        .into_result()
        .map_err(|_| todo!())?;

    let ast = Arc::new(ast_ctx.ast.borrow().clone());

    #[cfg(debug_assertions)]
    println!("AST: {:#?}", ast);

    let globals = Globals::new();
    let mut context = Context::new(globals, ast);
    let mut tvg = TypeVarGen::new();

    for stmt_id in stmts {
        let ty = context.type_inference(stmt_id, &mut tvg)?;
        println!("Type: {}", ty);
    }

    Ok(())
}
