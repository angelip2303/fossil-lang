use std::cell::RefCell;
use std::rc::Rc;

use chumsky::Parser as ChumskyParser;
use chumsky::error::RichReason;
use chumsky::input::IterInput;
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::{Loc, ast::Ast};
use crate::error::{FossilError, FossilErrors};
use crate::parser::{
    grammar::{AstCtx, parse_stmt},
    lexer::Token,
};
use crate::passes::{GlobalContext, ParsedProgram};

pub struct Parser;

impl Parser {
    pub fn parse(src: &str, source_id: usize) -> Result<ParsedProgram, FossilErrors> {
        Self::parse_with_context(src, source_id, Default::default())
    }

    pub fn parse_with_context(
        src: &str,
        source_id: usize,
        mut gcx: GlobalContext,
    ) -> Result<ParsedProgram, FossilErrors> {
        // Tokenize with Logos - collect tokens WITH their original byte spans
        // Lexer errors are converted to Token::Error and reported separately
        let lexer = Token::lexer(src);
        let len = src.len();
        let mut lexer_errors = Vec::new();
        let tokens: Vec<(Token, SimpleSpan)> = lexer
            .spanned()
            .map(|(token_result, span)| {
                let simple_span = SimpleSpan::from(span);
                let token = match token_result {
                    Ok(t) => t,
                    Err(_) => {
                        lexer_errors.push(simple_span);
                        Token::Error
                    }
                };
                (token, simple_span)
            })
            .collect();

        // Report lexer errors before parsing
        if !lexer_errors.is_empty() {
            let mut compile_errors = FossilErrors::new();
            for span in lexer_errors {
                let loc = Loc::new(source_id, span.into_range());
                compile_errors.push(FossilError::syntax("Invalid token", loc));
            }
            return Err(compile_errors);
        }

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

        // Create input with proper byte spans from tokens
        // Using IterInput which preserves the original source positions from the lexer
        let eoi = SimpleSpan::from(len..len);
        let input = IterInput::new(tokens.into_iter(), eoi);

        match parser.parse(input).into_result() {
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
            Err(chumsky_errors) => {
                // Collect all parse errors
                let mut compile_errors = FossilErrors::new();

                for err in chumsky_errors {
                    // Extract custom messages cleanly, fall back to debug format for others
                    let error_message = match err.reason() {
                        RichReason::Custom(msg) => msg.to_string(),
                        reason => format!("Parse error: {:?}", reason),
                    };
                    // Convert SimpleSpan to Loc
                    let simple_span = err.span();
                    let loc = Loc::new(source_id, simple_span.into_range());
                    compile_errors.push(FossilError::syntax(error_message, loc));
                }

                Err(compile_errors)
            }
        }
    }
}
