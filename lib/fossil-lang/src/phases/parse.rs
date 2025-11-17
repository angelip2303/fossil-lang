use chumsky::Parser as _;
use chumsky::prelude::*;
use logos::Logos;

use crate::error::ParseError;
use crate::parser::grammar::decls;
use crate::parser::lexer::Token;
use crate::phases::{AstCtx, ParsedProgram};

pub struct Parser;

impl Parser {
    pub fn parse(src: &str) -> Result<ParsedProgram, ParseError> {
        let ctx = AstCtx::default();
        let tokens: Vec<_> = Token::lexer(src).filter_map(|t| t.ok()).collect();

        decls(&ctx)
            .repeated()
            .collect::<Vec<_>>()
            .parse(tokens.as_slice())
            .into_result()
            .map_err(|e| {
                eprintln!("Parse error: {:?}", e);
                todo!()
            })?;

        let (ast, symbols) = ctx.take();

        Ok(ParsedProgram { ast, symbols })
    }
}
