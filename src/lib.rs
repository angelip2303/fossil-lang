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

pub fn compile_and_run(source: &str) -> crate::error::Result<()> {
    use chumsky::prelude::*;
    use interpreter::Interpreter;
    use lexer::Token;
    use logos::Logos;
    use parser::AstContext;
    use solver::{Context, Globals, TypeVarGen};
    use std::sync::Arc;

    // 1. Lexing
    let tokens: Vec<_> = Token::lexer(source).filter_map(|t| t.ok()).collect();

    #[cfg(debug_assertions)]
    println!("=== TOKENS ===");
    #[cfg(debug_assertions)]
    println!("{:?}\n", tokens);

    // 2. Parsing
    let ast_ctx = AstContext::default();
    let parser = parser::statement_parser(&ast_ctx)
        .repeated()
        .collect::<Vec<_>>();

    let stmts = parser
        .parse(tokens.as_slice())
        .into_result()
        .map_err(|_| todo!("Parse error"))
        .unwrap();

    let ast = Arc::new(ast_ctx.ast.borrow().clone());

    #[cfg(debug_assertions)]
    println!("=== AST ===");
    #[cfg(debug_assertions)]
    println!("{:#?}\n", ast);

    // 3. Type Checking
    let globals = Globals::new();
    let mut context = Context::new(globals.clone(), ast.clone());
    let mut tvg = TypeVarGen::new();

    #[cfg(debug_assertions)]
    println!("=== TYPE INFERENCE ===");

    for stmt_id in &stmts {
        let ty = context.type_inference(*stmt_id, &mut tvg)?;

        #[cfg(debug_assertions)]
        println!("Statement {:?}: {}", stmt_id, ty);
    }

    #[cfg(debug_assertions)]
    println!();

    // 4. Execution
    #[cfg(debug_assertions)]
    println!("=== EXECUTION ===");

    let mut interpreter = Interpreter::new(context);

    for stmt_id in stmts {
        let result = interpreter.eval_stmt(stmt_id)?;

        // Solo imprimir si no es Unit (para evitar spam de outputs)
        match result {
            crate::runtime::Value::Unit => {}
            _ => {
                #[cfg(debug_assertions)]
                println!("Result: {:?}", result);
            }
        }
    }

    Ok(())
}
