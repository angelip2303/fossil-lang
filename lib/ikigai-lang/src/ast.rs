use std::cell::RefCell;
use std::rc::Rc;

use chumsky::prelude::*;
use logos::Logos;

use crate::context::*;

type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, SimpleSpan>>;
type ParserContext<'a> = &'a Rc<RefCell<AstBuilder>>;

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

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Debug, Clone)]
pub enum Decl {
    Let(Symbol, ExprId),
    Type(Symbol, TypeId),
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(Symbol),
    Qualified(Vec<Symbol>),
    Literal(Literal),
    List(Vec<ExprId>),
    Record(Vec<(Symbol, ExprId)>),
    Function { params: Vec<Symbol>, body: ExprId }, // TODO: maybe in the future we could support Params with default values, named parameters, etc.
    Application { callee: ExprId, args: Vec<ExprId> }, // TODO: In the future args could be their own struct
                                                       // TODO: pipe
                                                       // TODO: member access
                                                       // TODO: type annotation
                                                       // TODO: cast
                                                       // TODO: unary op
                                                       // TODO: binary op
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub enum Type {
    Provider(Symbol, Vec<Literal>),
    Named(Symbol),
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
}

#[derive(Default)]
pub struct Ast {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub symbols: Interner,
}

#[derive(Default)]
pub struct AstBuilder {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub symbols: Interner,
}

impl AstBuilder {
    pub fn alloc_decl(&mut self, decl: Decl) -> DeclId {
        self.decls.alloc(decl)
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        self.exprs.alloc(expr)
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeId {
        self.types.alloc(ty)
    }

    pub fn build(self) -> Ast {
        Ast {
            decls: self.decls,
            exprs: self.exprs,
            types: self.types,
            symbols: self.symbols,
        }
    }
}

pub fn symbol<'a, I>(ctx: ParserContext<'a>) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ctx.borrow_mut().symbols.intern(ident) }
}

pub fn decls<'a, I>(ctx: ParserContext<'a>) -> impl Parser<'a, I, DeclId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let let_decl = just(Token::Let)
        .ignore_then(symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(exprs(ctx))
        .map(|(name, expr)| ctx.borrow_mut().alloc_decl(Decl::Let(name, expr)));

    let type_decl = just(Token::Type)
        .ignore_then(symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(types(ctx))
        .map(|(name, ty)| ctx.borrow_mut().alloc_decl(Decl::Type(name, ty)));

    let expr_decl = exprs(ctx).map(|expr| ctx.borrow_mut().alloc_decl(Decl::Expr(expr)));

    choice((let_decl, type_decl, expr_decl))
}

pub fn exprs<'a, I>(ctx: ParserContext<'a>) -> impl Parser<'a, I, ExprId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let identifier =
            symbol(ctx).map(|name| ctx.borrow_mut().alloc_expr(Expr::Identifier(name)));

        let qualified = symbol(ctx)
            .separated_by(just(Token::Dot))
            .at_least(1)
            .collect()
            .map(|names| ctx.borrow_mut().alloc_expr(Expr::Qualified(names)));

        let literal = literal(ctx).map(|lit| ctx.borrow_mut().alloc_expr(Expr::Literal(lit)));

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|items| ctx.borrow_mut().alloc_expr(Expr::List(items)));

        let record = symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.borrow_mut().alloc_expr(Expr::Record(fields)));

        let function = symbol(ctx)
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(params, body)| ctx.borrow_mut().alloc_expr(Expr::Function { params, body }));

        let application = expr
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(callee, args)| {
                ctx.borrow_mut()
                    .alloc_expr(Expr::Application { callee, args })
            });

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

pub fn literal<'a, I>(
    ctx: ParserContext<'a>,
) -> impl Parser<'a, I, Literal, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(ctx.borrow_mut().symbols.intern(s)),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

pub fn types<'a, I>(ctx: ParserContext<'a>) -> impl Parser<'a, I, TypeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let provider = symbol(ctx)
        .then(
            literal(ctx)
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect(),
        )
        .map(|(name, args)| ctx.borrow_mut().alloc_type(Type::Provider(name, args)));

    let ty = recursive(|ty| {
        let named = symbol(ctx).map(|name| ctx.borrow_mut().alloc_type(Type::Named(name)));

        let function = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(ty.clone())
            .map(|(args, ty)| ctx.borrow_mut().alloc_type(Type::Function(args, ty)));

        let list = ty
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|ty| ctx.borrow_mut().alloc_type(Type::List(ty)));

        let record = symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.borrow_mut().alloc_type(Type::Record(fields)));

        choice((named, function, list, record))
    });

    choice((provider, ty))
}
