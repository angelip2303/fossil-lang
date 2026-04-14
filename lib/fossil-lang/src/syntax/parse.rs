use std::cell::RefCell;
use std::rc::Rc;

use chumsky::Parser as ChumskyParser;
use chumsky::error::RichReason;
use chumsky::input::IterInput;
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::{Loc, ast::Ast};
use crate::db::Db;
use crate::error::FossilError;
use crate::parser::{
    grammar::{AstCtx, parse_stmt},
    lexer::Token,
};

/// Output of `Parser::parse`: a best-effort AST plus any syntax errors
/// collected during parsing. When recovery succeeds, the AST contains
/// the successfully-parsed statements and `errors` lists the failures;
/// callers are expected to emit each error via their diagnostic channel
/// and still consume the partial AST for downstream phases.
pub struct ParseOutput {
    pub ast: Ast,
    pub errors: Vec<FossilError>,
}

pub struct Parser;

impl Parser {
    pub fn parse(db: &dyn Db, src: &str, source_id: usize) -> ParseOutput {
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

        // Lexer errors are surfaced but parsing still proceeds with the
        // `Token::Error` placeholders — the parser's recovery strategy
        // will skip them and still deliver a partial AST for the rest.
        let mut compile_errors: Vec<FossilError> = lexer_errors
            .into_iter()
            .map(|span| {
                FossilError::syntax("Invalid token", Loc::new(source_id, span.into_range()))
            })
            .collect();

        // Create AST in Rc for shared access during parsing
        let ast = Rc::new(RefCell::new(Ast::default()));

        let ctx = AstCtx {
            db,
            ast: ast.clone(),
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

        // `into_output_errors()` returns both the (possibly partial) AST and
        // any errors collected during parsing. Stmt-level recovery in
        // `parse_stmt` lets the outer `.repeated()` keep producing AST nodes
        // even when individual stmts fail, so we surface a best-effort AST
        // alongside the complete error list instead of giving up on first
        // error.
        let (output, chumsky_errors) = parser.parse(input).into_output_errors();
        for err in chumsky_errors {
            let error_message = match err.reason() {
                RichReason::Custom(msg) => msg.to_string(),
                reason => format!("Parse error: {:?}", reason),
            };
            let simple_span = err.span();
            let loc = Loc::new(source_id, simple_span.into_range());
            compile_errors.push(FossilError::syntax(error_message, loc));
        }

        let mut final_ast = std::mem::take(&mut *ast.borrow_mut());
        if let Some(root_stmts) = output {
            final_ast.root = root_stmts;
        }
        ParseOutput {
            ast: final_ast,
            errors: compile_errors,
        }
    }
}
