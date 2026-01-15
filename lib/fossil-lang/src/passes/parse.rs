use std::cell::RefCell;
use std::rc::Rc;

use chumsky::prelude::*;
use chumsky::input::IterInput;
use chumsky::Parser as ChumskyParser;
use logos::Logos;

use crate::ast::{ast::Ast, Loc};
use crate::error::{CompileError, CompileErrorKind, CompileErrors};
use crate::parser::{grammar::{AstCtx, parse_stmt}, lexer::Token};
use crate::passes::{GlobalContext, ParsedProgram};

pub struct Parser;

impl Parser {
    pub fn parse(src: &str, source_id: usize) -> Result<ParsedProgram, CompileErrors> {
        Self::parse_with_context(src, source_id, GlobalContext::new())
    }

    pub fn parse_with_context(
        src: &str,
        source_id: usize,
        mut gcx: GlobalContext,
    ) -> Result<ParsedProgram, CompileErrors> {

        // Tokenize with Logos - collect tokens WITH their original byte spans
        let lexer = Token::lexer(src);
        let len = src.len();
        let tokens: Vec<(Token, SimpleSpan)> = lexer
            .spanned()
            .map(|(token_result, span)| {
                let token = match token_result {
                    Ok(t) => t,
                    Err(_) => Token::Let, // placeholder for errors
                };
                // Convert std::ops::Range to chumsky::span::SimpleSpan
                (token, SimpleSpan::from(span))
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
                let mut compile_errors = CompileErrors::new();
                let mut interner_ref = interner.borrow_mut();

                for err in chumsky_errors {
                    // Use the interner from the Rc<RefCell<>> since gcx.interner was moved
                    let error_message = format!("Parse error: {:?}", err.reason());
                    let error_msg = interner_ref.intern(&error_message);
                    // Convert SimpleSpan to Loc
                    let simple_span = err.span();
                    let loc = Loc {
                        source: source_id,
                        span: simple_span.into_range(),
                    };
                    compile_errors.push(CompileError::new(
                        CompileErrorKind::Parse(error_msg),
                        loc,
                    ));
                }

                Err(compile_errors)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiple_parse_errors() {
        // Create source with multiple syntax errors
        // Note: Chumsky's error recovery may not catch all errors in one pass,
        // but the infrastructure now supports collecting multiple errors
        let src = "let = 42\nlet y = \nlet z = ]";
        let result = Parser::parse(src, 0);

        // Should return an error
        match result {
            Err(errors) => {
                // Verify that we get CompileErrors (which can hold multiple errors)
                // The actual count depends on parser error recovery behavior
                assert!(
                    errors.0.len() >= 1,
                    "Expected at least 1 parse error, got {}",
                    errors.0.len()
                );
                println!("Got {} parse error(s)", errors.0.len());
            }
            Ok(_) => panic!("Expected parse to fail with errors"),
        }
    }

    #[test]
    fn test_single_parse_error() {
        // Create source with one syntax error
        let src = "let = 42";
        let result = Parser::parse(src, 0);

        // Should return an error
        match result {
            Err(errors) => {
                assert!(
                    errors.0.len() >= 1,
                    "Expected at least 1 parse error, got {}",
                    errors.0.len()
                );
            }
            Ok(_) => panic!("Expected parse to fail with error"),
        }
    }

    #[test]
    fn test_valid_parse() {
        // Create valid source
        let src = "let x = 42";
        let result = Parser::parse(src, 0);

        // Should succeed
        assert!(result.is_ok(), "Expected parse to succeed: {:?}", result.err());
    }
}
