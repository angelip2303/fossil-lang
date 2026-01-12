//! Module containing the grammar for the Fossil language.
//!
//! This provides several functions for parsing Fossil code. Each of these is
//! designed to handle specific aspects of the language's syntax and semantics.
//! All the functions are named in a consistent manner; namely, they all start with `parse_`.

use std::cell::RefCell;
use std::rc::Rc;

use chumsky::pratt::{infix, left, postfix};
use chumsky::prelude::*;

use crate::ast::ast::*;
use crate::ast::{Loc, SourceId};
use crate::context::{Interner, Symbol};
use crate::parser::lexer::Token;

type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, SimpleSpan>>;

#[derive(Clone)]
pub struct AstCtx {
    pub ast: Rc<RefCell<Ast>>,
    pub interner: Rc<RefCell<Interner>>,
    pub source_id: SourceId,
}

impl AstCtx {
    pub fn intern(&self, ident: &str) -> Symbol {
        self.interner.borrow_mut().intern(ident)
    }

    /// Convert a SimpleSpan to a Loc with this context's source_id
    pub fn to_loc(&self, span: SimpleSpan) -> Loc {
        Loc {
            source: self.source_id,
            span: span.into_range(),
        }
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
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    // For top-level statements, use parse_expr(ctx)
    parse_stmt_impl(ctx, parse_expr(ctx))
}

/// Internal implementation of statement parsing
/// Accepts an expression parser to enable mutual recursion with parse_expr
fn parse_stmt_impl<'a, I>(
    ctx: &'a AstCtx,
    expr: impl Parser<'a, I, ExprId, ParserError<'a>> + Clone + 'a,
) -> impl Parser<'a, I, StmtId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let import_stmt = just(Token::Open)
        .ignore_then(parse_path(ctx))
        .then(just(Token::As).ignore_then(parse_symbol(ctx)).or_not())
        .map_with(|(module, alias), e| {
            ctx.alloc_stmt(StmtKind::Import { module, alias }, ctx.to_loc(e.span()))
        });

    let let_stmt = just(Token::Let)
        .ignore_then(parse_symbol(ctx))
        .then(
            just(Token::Colon)
                .ignore_then(parse_type(ctx))
                .or_not()
        )
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map_with(|((name, ty), value), e| {
            ctx.alloc_stmt(StmtKind::Let { name, ty, value }, ctx.to_loc(e.span()))
        });

    let type_stmt = just(Token::Type)
        .ignore_then(parse_symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(parse_type(ctx))
        .map_with(|(name, ty), e| {
            ctx.alloc_stmt(StmtKind::Type { name, ty }, ctx.to_loc(e.span()))
        });

    let expr_stmt =
        expr.map_with(|expr, e| ctx.alloc_stmt(StmtKind::Expr(expr), ctx.to_loc(e.span())));

    choice((import_stmt, let_stmt, type_stmt, expr_stmt))
}

fn parse_symbol<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ctx.intern(ident) }
}

fn parse_path<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Path, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    // Parse first symbol, then optionally parse :: and more symbols
    parse_symbol(ctx)
        .then(
            just(Token::ModuleSep)
                .ignore_then(parse_symbol(ctx))
                .repeated()
                .collect::<Vec<_>>()
        )
        .map(|(first, rest)| {
            if rest.is_empty() {
                Path::simple(first)
            } else {
                let mut path = vec![first];
                path.extend(rest);
                Path::qualified(path)
            }
        })
}

fn parse_param<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Param, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    parse_symbol(ctx).map(|name| Param { name })
}

fn parse_primitive_type<'a, I>() -> impl Parser<'a, I, PrimitiveType, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::IntType => PrimitiveType::Int,
        Token::BoolType => PrimitiveType::Bool,
        Token::StringType => PrimitiveType::String,
    }
}

fn parse_expr<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, ExprId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let path = parse_path(ctx)
            .map_with(|path, e| ctx.alloc_expr(ExprKind::Identifier(path), ctx.to_loc(e.span())));

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map_with(|_, e| ctx.alloc_expr(ExprKind::Unit, ctx.to_loc(e.span())));

        let literal = parse_literal(ctx)
            .map_with(|lit, e| ctx.alloc_expr(ExprKind::Literal(lit), ctx.to_loc(e.span())));

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|items, e| ctx.alloc_expr(ExprKind::List(items), ctx.to_loc(e.span())));

        let record = parse_symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|fields, e| ctx.alloc_expr(ExprKind::Record(fields), ctx.to_loc(e.span())));

        // Block expression as general expression (not just in functions)
        // Uses parse_stmt_impl to avoid unbounded mutual recursion
        let block = just(Token::LBrace)
            .then(
                parse_stmt_impl(ctx, expr.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RBrace))
            .map_with(|(_lbrace, stmts), e| {
                ctx.alloc_expr(ExprKind::Block { stmts }, ctx.to_loc(e.span()))
            });

        let function = just(Token::Func)
            .ignore_then(
                parse_param(ctx)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::Arrow))
            .then(expr.clone()) // Body is just any expression (including blocks!)
            .map_with(|(params, body), e| {
                ctx.alloc_expr(ExprKind::Function { params, body }, ctx.to_loc(e.span()))
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
                ctx.alloc_expr(ExprKind::Application { callee, args }, ctx.to_loc(e.span()))
            });

        // Block must come BEFORE record since both start with {
        // Block: { stmt* } parses as statements
        // Record: { field = expr, ... } requires field names with =
        // Function must come BEFORE application and path to avoid parsing 'fn' as identifier
        // Use .or() instead of choice() to avoid backtracking that pollutes the arena
        let atom = function
            .or(application)
            .or(unit)
            .or(literal)
            .or(list)
            .or(block)
            .or(record)
            .or(path)
            .map_with(|expr, e| (expr, e.span()))
            .boxed();

        let pratt = atom
            .pratt((
                postfix(
                    10, // Highest precedence
                    just(Token::Dot).ignore_then(parse_symbol(ctx)),
                    |(lhs, lhs_span): (ExprId, SimpleSpan), field, _| {
                        let loc = ctx.to_loc(lhs_span);
                        (
                            ctx.alloc_expr(ExprKind::FieldAccess { expr: lhs, field }, loc.clone()),
                            lhs_span,
                        )
                    },
                ),
                infix(
                    left(1),
                    just(Token::Pipe),
                    |(lhs, lhs_span): (ExprId, SimpleSpan),
                     _,
                     (rhs, rhs_span): (ExprId, SimpleSpan),
                     _| {
                        // Merge spans: create a span from start of lhs to end of rhs
                        let merged_span = lhs_span.start()..rhs_span.end();
                        let merged_loc = ctx.to_loc(SimpleSpan::from(merged_span.clone()));
                        (
                            ctx.alloc_expr(ExprKind::Pipe { lhs, rhs }, merged_loc),
                            SimpleSpan::from(merged_span),
                        )
                    },
                ),
            ))
            .map(|expr| expr.0);

        pratt
    })
}

fn parse_literal<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Literal, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(ctx.intern(s)),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

/// Parse attribute: #[name("arg1", "arg2")]
fn parse_attribute<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Attribute, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    just(Token::Hash)
        .ignore_then(just(Token::LBracket))
        .ignore_then(parse_symbol(ctx))
        .then(
            parse_literal(ctx)
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or_not(),
        )
        .then_ignore(just(Token::RBracket))
        .map(|(name, args)| Attribute {
            name,
            args: args.unwrap_or_default(),
        })
}

fn parse_type<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, TypeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|ty| {
        let primitive = parse_primitive_type()
            .map_with(|ty, e| ctx.alloc_type(TypeKind::Primitive(ty), ctx.to_loc(e.span())));

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map_with(|_, e| ctx.alloc_type(TypeKind::Unit, ctx.to_loc(e.span())));

        let function = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(ty.clone())
            .map_with(|(params, ret), e| {
                ctx.alloc_type(TypeKind::Function(params, ret), ctx.to_loc(e.span()))
            });

        let list = ty
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|ty, e| ctx.alloc_type(TypeKind::List(ty), ctx.to_loc(e.span())));

        // Parse record type: { #[attr] field: Type, ... }
        let record = {
            // Parse single field with optional attributes
            let field = parse_attribute(ctx)
                .repeated()
                .collect::<Vec<_>>()
                .then(parse_symbol(ctx))
                .then_ignore(just(Token::Colon))
                .then(ty.clone())
                .map(|((attrs, name), ty)| RecordField { name, ty, attrs });

            field
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|fields, e| ctx.alloc_type(TypeKind::Record(fields), ctx.to_loc(e.span())))
        };

        // Parse type provider invocation: csv<"file.csv", "option">
        let provider = parse_path(ctx)
            .then(
                parse_literal(ctx)
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect()
                    .delimited_by(just(Token::LAngle), just(Token::RAngle))
            )
            .try_map(|(path, literals), span| {
                // Type providers use literals as arguments
                Ok(ctx.alloc_type(TypeKind::Provider { provider: path, args: literals }, ctx.to_loc(span)))
            });

        // Parse Name or Name<Type, Type, ...>
        let named_or_app = parse_path(ctx)
            .then(
                ty.clone()
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect()
                    .delimited_by(just(Token::LAngle), just(Token::RAngle))
                    .or_not()
            )
            .map_with(|(path, opt_args), e| {
                match opt_args {
                    Some(args) => ctx.alloc_type(TypeKind::App { ctor: path, args }, ctx.to_loc(e.span())),
                    None => ctx.alloc_type(TypeKind::Named(path), ctx.to_loc(e.span())),
                }
            });

        choice((primitive, unit, function, list, record, provider, named_or_app))
    })
}
