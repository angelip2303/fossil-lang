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
    #[token("join")]
    Join,
    #[token("on")]
    On,
    #[token("suffix")]
    Suffix,
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
    #[token("float")]
    FloatType,
    #[token("ref")]
    Ref,

    #[token("=")]
    Eq,
    #[token("->")]
    Arrow,
    #[token("..")]
    DotDot,
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
    #[token("#[")]
    HashBracket,
    #[token("]")]
    RBracket,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice(), priority = 2)]
    Identifier(&'a str),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..(lex.slice().len()-1)])]
    String(&'a str),
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
}

impl Token<'_> {
    /// Returns the string representation of keyword tokens, or None for non-keyword tokens.
    pub fn as_keyword_str(&self) -> Option<&str> {
        match self {
            Token::Let => Some("let"),
            Token::Type => Some("type"),
            Token::Each => Some("each"),
            Token::Do => Some("do"),
            Token::End => Some("end"),
            Token::Join => Some("join"),
            Token::On => Some("on"),
            Token::Suffix => Some("suffix"),
            Token::True => Some("true"),
            Token::False => Some("false"),
            Token::IntType => Some("int"),
            Token::BoolType => Some("bool"),
            Token::StringType => Some("string"),
            Token::FloatType => Some("float"),
            Token::Ref => Some("ref"),
            _ => None,
        }
    }
}
