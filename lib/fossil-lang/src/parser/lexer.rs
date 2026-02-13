use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*")]
pub enum Token<'a> {
    Error,

    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("each")]
    Each,
    #[token("do")]
    Do,
    #[token("end")]
    End,
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
    #[token("|>")]
    Pipe,
    #[token("+>")]
    PlusGt,
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
    #[token("!")]
    Bang,
    #[token("?")]
    Question,
    #[token("@")]
    At,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice(), priority = 2)]
    Identifier(&'a str),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..(lex.slice().len()-1)])]
    String(&'a str),
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
}
