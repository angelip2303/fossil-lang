use std::cell::RefCell;
use std::rc::Rc;

use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use logos::Logos;

use crate::ast::{ast::Ast, Loc};
use crate::error::{CompileError, CompileErrorKind, ParseError};
use crate::parser::{grammar::{AstCtx, parse_stmt}, lexer::Token};
use crate::passes::{GlobalContext, ParsedProgram};

pub struct Parser;

impl Parser {
    pub fn parse(src: &str, source_id: usize) -> Result<ParsedProgram, ParseError> {
        Self::parse_with_context(src, source_id, GlobalContext::new())
    }

    pub fn parse_with_context(
        src: &str,
        source_id: usize,
        mut gcx: GlobalContext,
    ) -> Result<ParsedProgram, ParseError> {

        // Tokenize with Logos - collect tokens into a vec for Stream
        let lexer = Token::lexer(src);
        let tokens: Vec<Token> = lexer
            .map(|token_result| {
                match token_result {
                    Ok(t) => t,
                    Err(_) => Token::Let, // placeholder for errors
                }
            })
            .collect();

        // Create AST and interner in Rc for shared access during parsing
        let ast = Rc::new(RefCell::new(Ast::default()));
        let interner = Rc::new(RefCell::new(std::mem::take(&mut gcx.interner)));

        let ctx = AstCtx {
            ast: ast.clone(),
            interner: interner.clone(),
            source_id,
        };

        // Create parser for multiple statements
        // Clone ctx so the parser doesn't borrow the original
        let ctx_for_parser = ctx.clone();
        let parser = parse_stmt(&ctx_for_parser).repeated().collect::<Vec<_>>();

        // Create Chumsky stream from tokens
        // Stream::from_iter() automatically handles span tracking with SimpleSpan
        let stream = chumsky::input::Stream::from_iter(tokens.into_iter());

        match parser.parse(stream).into_result() {
            Ok(root_stmts) => {
                // Extract AST and interner from Rc<RefCell<>>
                // We can't unwrap the Rc due to borrow checker limitations,
                // so we take the value from the RefCell instead
                let mut final_ast = ast.borrow_mut();

                // Store root statements
                final_ast.root = root_stmts;

                let final_ast = std::mem::take(&mut *final_ast);

                let mut final_interner = interner.borrow_mut();
                let final_interner = std::mem::take(&mut *final_interner);

                gcx.interner = final_interner;

                Ok(ParsedProgram {
                    ast: final_ast,
                    gcx,
                })
            }
            Err(errors) => {
                // Get the first error
                let err = errors.into_iter().next().unwrap();
                // Use the interner from the Rc<RefCell<>> since gcx.interner was moved
                let error_message = format!("Parse error: {:?}", err.reason());
                let error_msg = interner.borrow_mut().intern(&error_message);
                // Convert SimpleSpan to Loc
                let simple_span = err.span();
                let loc = Loc {
                    source: source_id,
                    span: simple_span.into_range(),
                };
                Err(CompileError::new(
                    CompileErrorKind::Parse(error_msg),
                    loc,
                ))
            }
        }
    }
}
