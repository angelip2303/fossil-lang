use std::collections::HashMap;

use chumsky::prelude::*;
use logos::Logos;
use paste::paste;

pub type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, SimpleSpan>>;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*")]
pub enum Token<'a> {
    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("fun")]
    Func,
    #[token("true")]
    True,
    #[token("false")]
    False,

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

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'a str),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..(lex.slice().len()-1)])]
    String(&'a str),
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
}

trait ArenaAllocated<'a> {
    fn alloc(self, context: &'a AstCtx<'a>) -> &'a Self
    where
        Self: 'a;
}

macro_rules! declare_arena {
    ($t: ident <$a: lifetime>) => {
        paste! {
            pub type [<$t Arena>]<$a> = ::typed_arena::Arena<$t<$a>>;
            pub type [<$t Ref>]<$a> = &$a $t<$a>;

            impl<'a> ArenaAllocated<'a> for $t<'a> {
                fn alloc(self, context: &'a AstCtx<'a>) -> &'a $t<'a> where Self: 'a {
                    context. [<$t:lower s>].alloc(self)
                }
            }
        }
    };

    ($($t: ident $(< $a: lifetime >)?),+) => {
        $(
            declare_arena!($t $(<$a>)?);
        )+
    };
}

declare_arena!(Decl<'a>, Expr<'a>, Type<'a>);

pub struct AstCtx<'a> {
    pub decls: DeclArena<'a>,
    pub exprs: ExprArena<'a>,
    pub types: TypeArena<'a>,
}

impl<'a> AstCtx<'a> {
    pub fn alloc<T>(&'a self, t: T) -> &'a T
    where
        T: ArenaAllocated<'a>,
    {
        &*t.alloc(self)
    }
}

pub type Name<'a> = &'a str;

pub fn name<'a, I>() -> impl Parser<'a, I, Name<'a>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ident }
}

pub enum Decl<'a> {
    // TODO: import
    Let(Name<'a>, ExprRef<'a>),
    Type(Name<'a>, TypeRef<'a>),
    Expr(ExprRef<'a>),
}

pub fn decls<'a, I>(ctx: &'a AstCtx<'a>) -> impl Parser<'a, I, DeclRef<'a>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let let_decl = just(Token::Let)
        .ignore_then(name())
        .then_ignore(just(Token::Eq))
        .then(exprs(ctx))
        .map(|(name, expr)| ctx.alloc(Decl::Let(name, expr)));

    let type_decl = just(Token::Type)
        .ignore_then(name())
        .then_ignore(just(Token::Eq))
        .then(types(ctx))
        .map(|(name, ty)| ctx.alloc(Decl::Type(name, ty)));

    let expr_decl = exprs(ctx).map(|expr| ctx.alloc(Decl::Expr(expr)));

    choice((let_decl, type_decl, expr_decl))
}

pub enum Expr<'a> {
    Identifier(Name<'a>),
    Qualified(Vec<Name<'a>>),
    Literal(Literal<'a>),
    List(Vec<ExprRef<'a>>),
    Record(Vec<(Name<'a>, ExprRef<'a>)>), // TODO: maybe BTreeMap?
    Function {
        params: Vec<Name<'a>>,
        body: ExprRef<'a>,
    }, // TODO: maybe in the future we could support Params with default values, named parameters, etc.
    Application {
        callee: ExprRef<'a>,
        args: Vec<ExprRef<'a>>,
    }, // TODO: In the future args could be their own struct
       // TODO: pipe
       // TODO: member access
       // TODO: type annotation
       // TODO: cast
       // TODO: unary op
       // TODO: binary op
}

pub fn exprs<'a, I>(ctx: &'a AstCtx<'a>) -> impl Parser<'a, I, ExprRef<'a>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let identifier = name().map(|name| ctx.alloc(Expr::Identifier(name)));

        let qualified = name()
            .separated_by(just(Token::Dot))
            .at_least(1)
            .collect()
            .map(|names| ctx.alloc(Expr::Qualified(names)));

        let literal = literal().map(|lit| ctx.alloc(Expr::Literal(lit)));

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|items| ctx.alloc(Expr::List(items)));

        let record = name()
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.alloc(Expr::Record(fields)));

        let function = name()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(params, body)| ctx.alloc(Expr::Function { params, body }));

        let application = expr
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(callee, args)| ctx.alloc(Expr::Application { callee, args }));

        let atom = choice((
            identifier,
            qualified,
            literal,
            list,
            record,
            function,
            application,
        ));

        // let pratt = atom.pratt((
        //     postfix(
        //         10,
        //         just(Token::Dot).ignore_then(name_parser(ctx)),
        //         |lhs, field, _| todo(),
        //     ),
        //     postfix(
        //         3,
        //         just(Token::Cast).ignore_then(type_parser(ctx)),
        //         |expr, ty, _| todo(),
        //     ),
        //     postfix(
        //         2,
        //         just(Token::Colon).ignore_then(type_parser(ctx)),
        //         |expr, ty, _| todo(),
        //     ),
        //     infix(right(1), just(Token::Pipe), |lhs, _, rhs, _| todo()),
        // ));

        atom
    })
}

pub enum Literal<'a> {
    Integer(i64),
    String(&'a str),
    Boolean(bool),
}

pub fn literal<'a, I>() -> impl Parser<'a, I, Literal<'a>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(s),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

pub enum Type<'a> {
    Provider(Name<'a>, Vec<Literal<'a>>),
    Named(Name<'a>),
    Function(Vec<TypeRef<'a>>, TypeRef<'a>),
    List(TypeRef<'a>),
    Record(Vec<(Name<'a>, TypeRef<'a>)>),
}

pub fn types<'a, I>(ctx: &'a AstCtx<'a>) -> impl Parser<'a, I, TypeRef<'a>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let provider = name()
        .then(
            literal()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect(),
        )
        .map(|(name, args)| ctx.alloc(Type::Provider(name, args)));

    let ty = recursive(|ty| {
        let named = name().map(|name| ctx.alloc(Type::Named(name)));

        let function = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(ty.clone())
            .map(|(args, ty)| ctx.alloc(Type::Function(args, ty)));

        let list = ty
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|ty| ctx.alloc(Type::List(ty)));

        let record = name()
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.alloc(Type::Record(fields)));

        choice((named, function, list, record))
    });

    choice((provider, ty))
}
