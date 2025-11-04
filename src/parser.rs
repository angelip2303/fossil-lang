use std::{cell::RefCell, rc::Rc};

use chumsky::prelude::*;

use crate::{ast::*, lexer::Token};

type ParserError<'a> = extra::Err<Rich<'a, Token, SimpleSpan>>;

#[derive(Clone, Default)]
pub struct AstContext {
    pub ast: Rc<RefCell<Ast>>,
}

impl AstContext {
    pub fn add_expr(&self, expr: ExprKind) -> NodeId {
        self.ast.borrow_mut().add_expr(expr)
    }

    pub fn add_stmt(&self, stmt: StmtKind) -> NodeId {
        self.ast.borrow_mut().add_stmt(stmt)
    }

    pub fn add_type(&self, ty: TypeKind) -> NodeId {
        self.ast.borrow_mut().add_type(ty)
    }

    pub fn add_name(&self, name: impl Into<String>) -> NodeId {
        self.ast.borrow_mut().add_name(name)
    }
}

// --- Helper Parsers ---

fn ident_parser<'a, I>(ctx: &'a AstContext) -> impl Parser<'a, I, NodeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ident }.map(|name| ctx.add_name(name))
}

fn path_parser<'a, I>(
    ctx: &'a AstContext,
) -> impl Parser<'a, I, Vec<NodeId>, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = SimpleSpan>,
{
    ident_parser(ctx)
        .separated_by(just(Token::Dot))
        .at_least(1)
        .collect()
}

fn param_parser<'a, I>(ctx: &'a AstContext) -> impl Parser<'a, I, Param, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = SimpleSpan>,
{
    ident_parser(ctx).map(|name| Param {
        name,
        default_value: None,
        ty: None,
    })
}

// --- Main Parsers ---

pub fn statement_parser<'a, I>(
    ctx: &'a AstContext,
) -> impl Parser<'a, I, NodeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = SimpleSpan>,
{
    let (expr_parser, type_parser) = expr_and_type_parsers(ctx);

    let import = just(Token::Import)
        .ignore_then(path_parser(ctx))
        .then(just(Token::As).ignore_then(ident_parser(ctx)).or_not())
        .map(|(path, alias)| {
            let path_expr = ctx.add_expr(ExprKind::Path(path));
            ctx.add_stmt(StmtKind::Import {
                path: path_expr,
                alias,
            })
        })
        .labelled("import statement");

    let r#let = just(Token::Let)
        .ignore_then(ident_parser(ctx))
        .then_ignore(just(Token::Eq))
        .then(expr_parser.clone())
        .map(|(name, value)| ctx.add_stmt(StmtKind::Let { name, value }))
        .labelled("let statement");

    let r#type = just(Token::Type)
        .ignore_then(ident_parser(ctx))
        .then_ignore(just(Token::Eq))
        .then(type_parser.clone())
        .map(|(name, ty)| ctx.add_stmt(StmtKind::Type { name, ty }))
        .labelled("type statement");

    let expr = expr_parser.map(|e| ctx.add_stmt(StmtKind::Expr(e)));

    choice((import, r#let, r#type, expr)).labelled("statement")
}

pub fn expr_and_type_parsers<'a, I>(
    ctx: &'a AstContext,
) -> (
    impl Parser<'a, I, NodeId, ParserError<'a>> + Clone,
    impl Parser<'a, I, NodeId, ParserError<'a>> + Clone,
)
where
    I: Input<'a, Token = Token, Span = SimpleSpan>,
{
    use chumsky::pratt::{infix, left, postfix, right};

    let mut expr = Recursive::declare();
    let mut r#type = Recursive::declare();

    let expr_impl = recursive(|expr_inner| {
        let literal = select! {
            Token::Integer(i) => ExprKind::Integer(i),
            Token::String(s) => ExprKind::String(s),
            Token::True => ExprKind::Boolean(true),
            Token::False => ExprKind::Boolean(false),
        }
        .map(|e| ctx.add_expr(e));

        let list = expr_inner
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|items| ctx.add_expr(ExprKind::List(items)));

        let record = {
            let field = ident_parser(ctx)
                .then_ignore(just(Token::Eq))
                .then(expr_inner.clone());

            field
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(|fields| ctx.add_expr(ExprKind::Record(fields)))
        };

        let function = just(Token::Fun)
            .ignore_then(param_parser(ctx).repeated().at_least(1).collect::<Vec<_>>())
            .then_ignore(just(Token::Arrow))
            .then(expr_inner.clone())
            .map(|(params, body)| {
                params.into_iter().rev().fold(body, |acc, param| {
                    ctx.add_expr(ExprKind::Function { param, body: acc })
                })
            });

        let parens = expr_inner
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let atom = choice((
            literal,
            list,
            record,
            function,
            parens,
            path_parser(ctx).map(|path| ctx.add_expr(ExprKind::Path(path))),
        ));

        atom.pratt((
            postfix(
                10,
                just(Token::Dot).ignore_then(ident_parser(ctx)),
                |lhs, field, _| ctx.add_expr(ExprKind::MemberAccess { object: lhs, field }),
            ),
            infix(left(9), empty(), |lhs, _, rhs, _| {
                ctx.add_expr(ExprKind::Call {
                    callee: lhs,
                    arg: rhs,
                })
            }),
            postfix(
                3,
                just(Token::Cast).ignore_then(r#type.clone()),
                |expr_id, target_type, _| {
                    ctx.add_expr(ExprKind::Cast {
                        expr: expr_id,
                        ty: target_type,
                    })
                },
            ),
            postfix(
                2,
                just(Token::Colon).ignore_then(r#type.clone()),
                |expr_id, annotated_type, _| {
                    ctx.add_expr(ExprKind::TypeAnnotation {
                        expr: expr_id,
                        ty: annotated_type,
                    })
                },
            ),
            infix(right(1), just(Token::Pipe), |lhs, _, rhs, _| {
                ctx.add_expr(ExprKind::Pipe {
                    left: lhs,
                    right: rhs,
                })
            }),
        ))
    });

    let type_impl = recursive(|type_inner| {
        let literal = select! {
            Token::StringType => TypeKind::String,
            Token::IntegerType => TypeKind::Int,
        }
        .map(|t| ctx.add_type(t));

        let list = type_inner
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|ty_id| ctx.add_type(TypeKind::List(ty_id)));

        let record = {
            let field = ident_parser(ctx)
                .then_ignore(just(Token::Colon))
                .then(type_inner.clone());

            field
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(|fields| ctx.add_type(TypeKind::Record(fields)))
        };

        let provider_or_var = path_parser(ctx)
            .then(
                expr.clone()
                    .map(|value| Arg { name: None, value })
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LAngle), just(Token::RAngle))
                    .or_not(),
            )
            .map(|(path, maybe_args)| match maybe_args {
                Some(args) => ctx.add_type(TypeKind::Provider(path, args)),
                None => ctx.add_type(TypeKind::Var(path)),
            });

        let atom = choice((literal, list, record, provider_or_var));

        atom.pratt(infix(right(10), just(Token::Arrow), |lhs, _, rhs, _| {
            ctx.add_type(TypeKind::Function { from: lhs, to: rhs })
        }))
    });

    expr.define(expr_impl);
    r#type.define(type_impl);

    (expr, r#type)
}
