use crate::ast::Loc;
use crate::error::{CompileError, CompileErrorKind, ParseError};
use crate::passes::{GlobalContext, ParsedProgram};

pub struct Parser;

impl Parser {
    pub fn parse(_src: &str, source_id: usize) -> Result<ParsedProgram, ParseError> {
        let mut gcx = GlobalContext::new();

        // TODO: Implement actual parser integration with grammar module
        // The Chumsky parser integration requires careful handling of Input traits
        // The parser grammar is already implemented in parser/grammar.rs but needs
        // proper stream/token integration.
        let error_msg = gcx
            .interner
            .intern("Parser not yet integrated - needs Chumsky Input trait work");
        Err(CompileError::new(
            CompileErrorKind::Parse(error_msg),
            Loc {
                source: source_id,
                span: 0..0,
            },
        ))
    }
}
