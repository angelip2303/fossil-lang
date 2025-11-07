use std::fmt;

use logos::Logos;

pub type SourceId = usize;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*")]
pub enum Token {
    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("open")]
    Import,
    #[token("as")]
    As,
    #[token("fun")]
    Fun,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("string")]
    StringType,
    #[token("int")]
    IntegerType,

    #[token("=")]
    Eq,
    #[token("->")]
    Arrow,
    #[token("|>")]
    Pipe,
    #[token(":>")]
    Cast,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..(lex.slice().len()-1)].to_string())]
    String(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
