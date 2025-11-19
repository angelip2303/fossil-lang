use chumsky::prelude::*;

use crate::ast::*;
use crate::context::Symbol;
use crate::parser::lexer::Token;
use crate::phases::AstCtx;

type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, SimpleSpan>>;

fn symbol<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ctx.symbols().intern(ident) }
}

fn path<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Path, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let identifier = symbol(ctx).map(|n| Path::simple(n));

    let qualified = symbol(ctx)
        .separated_by(just(Token::Dot))
        .at_least(1)
        .collect()
        .map(|path| Path::qualified(path));

    // qualified must go first, or else identifier will match on qualified paths
    choice((qualified, identifier))
}

pub fn decls<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, DeclId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let let_decl = just(Token::Let)
        .ignore_then(symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(exprs(ctx))
        .map(|(name, value)| ctx.ast().decls.alloc(Decl::Let { name, value }));

    let type_decl = just(Token::Type)
        .ignore_then(symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(types(ctx))
        .map(|(name, ty)| ctx.ast().decls.alloc(Decl::Type { name, ty }));

    let expr_decl = exprs(ctx).map(|expr| ctx.ast().decls.alloc(Decl::Expr(expr)));

    choice((let_decl, type_decl, expr_decl))
}

fn exprs<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, ExprId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let identifier = path(ctx).map(|path| ctx.ast().exprs.alloc(Expr::Identifier(path)));

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map(|_| ctx.ast().exprs.alloc(Expr::Unit));

        let literal = literal(ctx).map(|lit| ctx.ast().exprs.alloc(Expr::Literal(lit)));

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|items| ctx.ast().exprs.alloc(Expr::List(items)));

        let record = symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.ast().exprs.alloc(Expr::Record(fields)));

        let function = just(Token::Func)
            .ignore_then(
                symbol(ctx)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(params, body)| ctx.ast().exprs.alloc(Expr::Function { params, body }));

        let application = identifier
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(callee, args)| ctx.ast().exprs.alloc(Expr::Application { callee, args }));

        let atom = choice((
            application,
            unit,
            literal,
            list,
            record,
            function,
            identifier,
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

fn literal<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Literal, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        Token::String(s) => Literal::String(ctx.symbols().intern(s)),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

fn types<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, TypeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let provider = path(ctx)
        .then(
            literal(ctx)
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect()
                .delimited_by(just(Token::LAngle), just(Token::RAngle)),
        )
        .map(|(provider, args)| ctx.ast().types.alloc(Type::Provider { provider, args }));

    let ty = recursive(|ty| {
        let primitive = select! {
            Token::IntType => ctx.ast().types.alloc(Type::Primitive(PrimitiveType::Int)),
            Token::BoolType => ctx.ast().types.alloc(Type::Primitive(PrimitiveType::Bool)),
            Token::StringType => ctx.ast().types.alloc(Type::Primitive(PrimitiveType::String)),
        };

        let named = symbol(ctx).map(|name| ctx.ast().types.alloc(Type::Named(name)));

        let function = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(ty.clone())
            .map(|(params, ret)| ctx.ast().types.alloc(Type::Function(params, ret)));

        let list = ty
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|ty| ctx.ast().types.alloc(Type::List(ty)));

        let record = symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|fields| ctx.ast().types.alloc(Type::Record(fields)));

        choice((primitive, named, function, list, record))
    });

    choice((provider, ty))
}
