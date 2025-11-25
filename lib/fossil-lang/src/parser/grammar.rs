//! Module containing the grammar for the Fossil language.
//!
//! This provides several functions for parsing Fossil code. Each of these is
//! designed to handle specific aspects of the language's syntax and semantics.
//! All the functions are named in a consistent manner; namely, they all start with `parse_`.

use std::cell::RefCell;
use std::rc::Rc;

use chumsky::pratt::{infix, right};
use chumsky::prelude::*;

use crate::ast::ast::*;
use crate::ast::{Loc, SourceId};
use crate::context::{Interner, Symbol};
use crate::parser::lexer::Token;

type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, Loc>>;

pub struct AstCtx {
    pub ast: Rc<RefCell<Ast>>,
    pub interner: Rc<RefCell<Interner>>,
    pub source_id: SourceId,
}

impl AstCtx {
    pub fn intern(&self, ident: &str) -> Symbol {
        self.interner.borrow_mut().intern(ident)
    }

    pub fn alloc_stmt(&self, kind: StmtKind, loc: Loc) -> StmtId {
        self.ast.borrow_mut().stmts.alloc(Stmt { loc, kind })
    }

    pub fn alloc_expr(&self, kind: ExprKind, loc: Loc) -> ExprId {
        self.ast.borrow_mut().exprs.alloc(Expr { loc, kind })
    }

    pub fn alloc_type(&self, kind: TypeKind, loc: Loc) -> TypeId {
        self.ast.borrow_mut().types.alloc(Type { loc, kind })
    }
}

pub fn parse_stmt<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, StmtId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    let import_stmt = just(Token::Open)
        .ignore_then(parse_path(ctx))
        .then_ignore(just(Token::As))
        .then(parse_symbol(ctx))
        .map_with(|(module, alias), e| {
            ctx.alloc_stmt(StmtKind::Import { module, alias }, e.span())
        });

    let let_stmt = just(Token::Let)
        .ignore_then(parse_symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(parse_expr(ctx))
        .map_with(|(name, value), e| ctx.alloc_stmt(StmtKind::Let { name, value }, e.span()));

    let type_stmt = just(Token::Type)
        .ignore_then(parse_symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(parse_type(ctx))
        .map_with(|(name, ty), e| ctx.alloc_stmt(StmtKind::Type { name, ty }, e.span()));

    let expr_stmt =
        parse_expr(ctx).map_with(|expr, e| ctx.alloc_stmt(StmtKind::Expr(expr), e.span()));

    choice((import_stmt, let_stmt, type_stmt, expr_stmt))
}

fn parse_symbol<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    select! { Token::Identifier(ident) => ctx.intern(ident) }
}

fn parse_path<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Path, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    let identifier = parse_symbol(ctx).map(|n| Path::simple(n));

    let qualified = parse_symbol(ctx)
        .separated_by(just(Token::ModuleSep))
        .at_least(1)
        .collect()
        .map(|path| Path::qualified(path));

    // qualified must go first, or else identifier will match on qualified paths
    choice((qualified, identifier))
}

fn parse_param<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Param, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    parse_symbol(ctx).map(|name| Param { name })
}

fn parse_primitive_type<'a, I>() -> impl Parser<'a, I, PrimitiveType, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    select! {
        Token::IntType => PrimitiveType::Int,
        Token::BoolType => PrimitiveType::Bool,
        Token::StringType => PrimitiveType::String,
    }
}

fn parse_expr<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, ExprId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    recursive(|expr| {
        let path = parse_path(ctx)
            .map_with(|path, e| ctx.alloc_expr(ExprKind::Identifier(path), e.span()));

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map_with(|_, e| ctx.alloc_expr(ExprKind::Unit, e.span()));

        let literal =
            parse_literal(ctx).map_with(|lit, e| ctx.alloc_expr(ExprKind::Literal(lit), e.span()));

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|items, e| ctx.alloc_expr(ExprKind::List(items), e.span()));

        let record = parse_symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|fields, e| ctx.alloc_expr(ExprKind::Record(fields), e.span()));

        let function = just(Token::Func)
            .ignore_then(
                parse_param(ctx)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map_with(|(params, body), e| {
                ctx.alloc_expr(ExprKind::Function { params, body }, e.span())
            });

        let application = path
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map_with(|(callee, args), e| {
                ctx.alloc_expr(ExprKind::Application { callee, args }, e.span())
            });

        let atom = choice((application, unit, literal, list, record, function, path))
            .map_with(|expr, e| (expr, e.span()))
            .boxed();

        let pratt = atom
            .pratt((
                //     postfix(
                //         10,
                //         just(Token::Dot).ignore_then(name_parser(ctx)),
                //         |lhs, field, _| todo(),
                //     ),
                infix(
                    right(1),
                    just(Token::Pipe),
                    |(lhs, lhs_span): (ExprId, Loc), _, (rhs, rhs_span): (ExprId, Loc), _| {
                        let s = lhs_span.merge(rhs_span);
                        (ctx.alloc_expr(ExprKind::Pipe { lhs, rhs }, s.clone()), s)
                    },
                ),
            ))
            .map(|expr| expr.0);

        pratt
    })
}

fn parse_literal<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Literal, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(ctx.intern(s)),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

fn parse_type<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, TypeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = Loc>,
{
    let provider = parse_path(ctx)
        .then(
            parse_literal(ctx)
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect()
                .delimited_by(just(Token::LAngle), just(Token::RAngle)),
        )
        .map_with(|(provider, args), e| {
            ctx.alloc_type(TypeKind::Provider { provider, args }, e.span())
        });

    let ty = recursive(|ty| {
        let primitive = parse_primitive_type()
            .map_with(|ty, e| ctx.alloc_type(TypeKind::Primitive(ty), e.span()));

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map_with(|_, e| ctx.alloc_type(TypeKind::Unit, e.span()));

        let function = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(ty.clone())
            .map_with(|(params, ret), e| ctx.alloc_type(TypeKind::Function(params, ret), e.span()));

        let list = ty
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|ty, e| ctx.alloc_type(TypeKind::List(ty), e.span()));

        let record = parse_symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|fields, e| ctx.alloc_type(TypeKind::Record(fields), e.span()));

        let named =
            parse_path(ctx).map_with(|name, e| ctx.alloc_type(TypeKind::Named(name), e.span()));

        choice((primitive, unit, function, list, record, named))
    });

    choice((provider, ty))
}
