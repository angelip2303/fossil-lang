use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*")]
pub enum Token<'a> {
    /// Lexer error - invalid token
    Error,

    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("type")]
    Type,
    #[token("fn")]
    Func,
    #[token("+")]
    Plus,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("int")]
    IntType,
    #[token("bool")]
    BoolType,
    #[token("string")]
    StringType,

    #[token("=")]
    Eq,
    #[token("->")]
    Arrow,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token(":>")]
    Cast,
    #[token("|>")]
    Pipe,
    #[token(",")]
    Comma,
    #[token("#")]
    Hash,
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
    #[token("::")]
    ModuleSep,
    #[token("!")]
    Bang,
    #[token("@")]
    At,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice(), priority = 2)]
    Identifier(&'a str),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..(lex.slice().len()-1)])]
    String(&'a str),
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
}
