use std::cell::RefCell;
use std::rc::Rc;

use chumsky::pratt::postfix;
use chumsky::prelude::*;

use crate::ast::{
    Argument, Ast, Attribute, AttributeArg, ConstructorParam, Expr, ExprId, ExprKind, Literal,
    Path, PrimitiveType, ProviderArgument, RecordField, Stmt, StmtId, StmtKind, Type,
    TypeId, TypeKind,
};
use crate::ast::Loc;
use crate::ast::SourceId;
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
        Loc::new(self.source_id, span.into_range())
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
    // Use .then() instead of .ignore_then() to preserve full span
    let let_stmt = just(Token::Let)
        .then(parse_symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map_with(|((_, name), value), e| {
            ctx.alloc_stmt(StmtKind::Let { name, value }, ctx.to_loc(e.span()))
        });

    // Type statement with optional leading attributes and constructor params:
    // #[rdf(type = "...", base = "...")]
    // type Name(id: string, graph: string) = Type
    //
    // Constructor params are optional and provide metadata for record construction.

    // Parse a single constructor param: name: type
    let ctor_param = parse_symbol(ctx)
        .then_ignore(just(Token::Colon))
        .then(parse_type(ctx))
        .map(|(name, ty)| ConstructorParam { name, ty });

    // Parse optional constructor params list: (id: string, graph: string)
    let ctor_params = ctor_param
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .or_not()
        .map(|opt| opt.unwrap_or_default());

    // do...end record syntax: type Name(params) do @attr Field: Type ... end
    let do_end_field = parse_attribute(ctx)
        .repeated()
        .collect::<Vec<_>>()
        .then(parse_symbol(ctx))
        .then_ignore(just(Token::Colon))
        .then(parse_type(ctx))
        .map(|((attrs, name), ty)| RecordField { name, ty, attrs });

    let do_end_body = just(Token::Do)
        .ignore_then(
            do_end_field
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::End))
        .map_with(|fields, e| {
            ctx.alloc_type(TypeKind::Record(fields), ctx.to_loc(e.span()))
        });

    // = Type syntax (for providers and named types, NOT records)
    let eq_body = just(Token::Eq).ignore_then(parse_type(ctx));

    let type_stmt = parse_attribute(ctx)
        .repeated()
        .collect::<Vec<_>>()
        .then(just(Token::Type))
        .then(parse_symbol(ctx))
        .then(ctor_params)
        .then(do_end_body.or(eq_body))
        .map_with(|((((attrs, _), name), ctor_params), ty), e| {
            ctx.alloc_stmt(
                StmtKind::Type {
                    name,
                    ty,
                    attrs,
                    ctor_params,
                },
                ctx.to_loc(e.span()),
            )
        });

    let expr_stmt =
        expr.map_with(|expr, e| ctx.alloc_stmt(StmtKind::Expr(expr), ctx.to_loc(e.span())));

    choice((let_stmt, type_stmt, expr_stmt))
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
            just(Token::Dot)
                .ignore_then(parse_symbol(ctx))
                .repeated()
                .collect::<Vec<_>>(),
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

#[derive(Clone, Copy)]
enum ChainOp {
    Pipe,
    AddOutput,
}

enum PipeRhs {
    Projection(Symbol, ExprId),
    Call(ExprId),
}

/// Suffix after a parsed Path, used for longest-match atom parsing.
enum PathSuffix {
    Provider(Vec<ProviderArgument>),
    CtorRecord(Vec<Argument>, Vec<(Symbol, ExprId)>),
    Application(Vec<Argument>),
    Record(Vec<(Symbol, ExprId)>),
}

fn parse_expr<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, ExprId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .map_with(|_, e| ctx.alloc_expr(ExprKind::Unit, ctx.to_loc(e.span())));

        // Non-string literals (integers, booleans)
        let non_string_literal = select! {
            Token::Integer(i) => Literal::Integer(i),
            Token::True => Literal::Boolean(true),
            Token::False => Literal::Boolean(false),
        }
        .map_with(|lit, e| ctx.alloc_expr(ExprKind::Literal(lit), ctx.to_loc(e.span())));

        // String literal with potential interpolation
        // Simple paths allowed: "Hello ${name}" or "Hello ${row.Name}"
        let string_expr = select! { Token::String(s) => s }.map_with(move |s: &str, e| {
            let loc = ctx.to_loc(e.span());

            if let Some(segments) = parse_interpolation_segments(s) {
                let interned_parts: Vec<_> = segments.parts.iter().map(|p| ctx.intern(p)).collect();
                let exprs: Vec<_> = segments
                    .paths
                    .iter()
                    .map(|path_segments| {
                        // Build expression: "row.Name" → FieldAccess(Identifier(row), Name)
                        let mut expr = {
                            let sym = ctx.intern(path_segments[0]);
                            ctx.alloc_expr(ExprKind::Identifier(Path::simple(sym)), loc)
                        };
                        for field in &path_segments[1..] {
                            let field_sym = ctx.intern(field);
                            expr = ctx.alloc_expr(
                                ExprKind::FieldAccess {
                                    expr,
                                    field: field_sym,
                                },
                                loc,
                            );
                        }
                        expr
                    })
                    .collect();

                ctx.alloc_expr(
                    ExprKind::StringInterpolation {
                        parts: interned_parts,
                        exprs,
                    },
                    loc,
                )
            } else {
                ctx.alloc_expr(ExprKind::Literal(Literal::String(ctx.intern(s))), loc)
            }
        });

        let literal = non_string_literal.or(string_expr);

        let record_field = parse_symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone());

        let named_arg = parse_symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(name, value)| Argument::Named { name, value });

        let positional_arg = expr.clone().map(Argument::Positional);

        let argument = named_arg.or(positional_arg);

        // Longest-match path-based atom: parse Path once, then check suffix
        let bang_suffix = just(Token::Bang)
            .ignore_then(
                parse_provider_argument(ctx)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(PathSuffix::Provider);

        let paren_suffix = argument
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(
                record_field.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not(),
            )
            .map(|(args, fields)| match fields {
                Some(fields) => PathSuffix::CtorRecord(args, fields),
                None => PathSuffix::Application(args),
            });

        let brace_suffix = record_field
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(PathSuffix::Record);

        let path_based = parse_path(ctx)
            .map_with(|path, e| (path, e.span()))
            .then(
                bang_suffix
                    .or(paren_suffix)
                    .or(brace_suffix)
                    .or_not(),
            )
            .map_with(|((path, path_span), suffix), e| {
                let full_loc = ctx.to_loc(e.span());
                match suffix {
                    Some(PathSuffix::Provider(args)) => {
                        ctx.alloc_expr(ExprKind::ProviderInvocation { provider: path, args }, full_loc)
                    }
                    Some(PathSuffix::CtorRecord(ctor_args, fields)) => {
                        ctx.alloc_expr(
                            ExprKind::RecordInstance { type_path: path, ctor_args, fields },
                            full_loc,
                        )
                    }
                    Some(PathSuffix::Application(args)) => {
                        let callee_loc = ctx.to_loc(path_span);
                        let callee = ctx.alloc_expr(ExprKind::Identifier(path), callee_loc);
                        ctx.alloc_expr(ExprKind::Application { callee, args }, full_loc)
                    }
                    Some(PathSuffix::Record(fields)) => {
                        ctx.alloc_expr(
                            ExprKind::RecordInstance { type_path: path, ctor_args: vec![], fields },
                            full_loc,
                        )
                    }
                    None => {
                        ctx.alloc_expr(ExprKind::Identifier(path), full_loc)
                    }
                }
            });

        let atom = choice((path_based, unit, literal))
            .map_with(|expr, e| (expr, e.span()))
            .boxed();

        // Dot field access via pratt parser
        let base = atom.pratt((
            postfix(
                10, // Highest precedence
                just(Token::Dot)
                    .ignore_then(parse_symbol(ctx))
                    .map_with(|field, e| (field, e.span())),
                |(lhs, lhs_span): (ExprId, SimpleSpan),
                 (field, field_span): (Symbol, SimpleSpan),
                 _| {
                    // Merge spans: create a span from start of lhs to end of field
                    let merged_span = lhs_span.start()..field_span.end();
                    let merged_loc = ctx.to_loc(SimpleSpan::from(merged_span.clone()));
                    (
                        ctx.alloc_expr(ExprKind::FieldAccess { expr: lhs, field }, merged_loc),
                        SimpleSpan::from(merged_span),
                    )
                },
            ),
        ))
        .map(|expr| expr.0)
        .boxed();

        // Pipe chain: base (|> | +>) (each param -> expr | call_expr)
        let projection_rhs = just(Token::Each)
            .ignore_then(parse_symbol(ctx))
            .then_ignore(just(Token::Arrow))
            .then(base.clone());

        let chain_op = just(Token::Pipe).to(ChainOp::Pipe)
            .or(just(Token::PlusGt).to(ChainOp::AddOutput));

        let pipe_rhs = chain_op.then(choice((
            projection_rhs.map(|(param, output)| PipeRhs::Projection(param, output)),
            base.clone().map(PipeRhs::Call),
        )));

        base.then(pipe_rhs.repeated().collect::<Vec<_>>())
            .map(|(source, stages)| {
                if stages.is_empty() {
                    return source;
                }
                build_pipe_chain(ctx, source, &stages)
            })
    })
}

/// Build a pipe chain from a source expression and a list of stages.
/// Groups consecutive `+> each` blocks after a `|> each` into a single Projection.
fn build_pipe_chain(ctx: &AstCtx, source: ExprId, stages: &[(ChainOp, PipeRhs)]) -> ExprId {
    let mut current = source;
    let mut i = 0;

    while i < stages.len() {
        let (op, rhs) = &stages[i];

        match (op, rhs) {
            (_, PipeRhs::Call(rhs)) => {
                // Desugar pipe to Application directly:
                // x |> f(a)  →  f(x, a)
                // x |> f     →  f(x)
                let (callee, mut args) = {
                    let ast = ctx.ast.borrow();
                    let rhs_expr = ast.exprs.get(*rhs);
                    match &rhs_expr.kind {
                        ExprKind::Application { callee, args } => (*callee, args.clone()),
                        _ => (*rhs, vec![]),
                    }
                };
                args.insert(0, Argument::Positional(current));
                let loc = {
                    let ast = ctx.ast.borrow();
                    ast.exprs.get(current).loc.merge(ast.exprs.get(*rhs).loc)
                };
                current = ctx.alloc_expr(ExprKind::Application { callee, args }, loc);
                i += 1;
            }
            (ChainOp::Pipe, PipeRhs::Projection(param, output)) => {
                // Collect this output + any consecutive +> each blocks
                let mut outputs = vec![*output];
                let param = *param;
                let mut last_output = *output;
                while i + 1 < stages.len() {
                    if let (ChainOp::AddOutput, PipeRhs::Projection(_, next_output)) = &stages[i + 1] {
                        outputs.push(*next_output);
                        last_output = *next_output;
                        i += 1;
                    } else {
                        break;
                    }
                }
                let loc = {
                    let ast = ctx.ast.borrow();
                    ast.exprs.get(current).loc.merge(ast.exprs.get(last_output).loc)
                };
                current = ctx.alloc_expr(
                    ExprKind::Projection { source: current, param, outputs },
                    loc,
                );
                i += 1;
            }
            (ChainOp::AddOutput, PipeRhs::Projection(param, output)) => {
                // A bare +> each without a preceding |> each — treat as single Projection
                let loc = {
                    let ast = ctx.ast.borrow();
                    ast.exprs.get(current).loc.merge(ast.exprs.get(*output).loc)
                };
                current = ctx.alloc_expr(
                    ExprKind::Projection { source: current, param: *param, outputs: vec![*output] },
                    loc,
                );
                i += 1;
            }
        }
    }

    current
}

fn parse_literal<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Literal, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Integer(i) => Literal::Integer(i),
        // Note: For literals used in provider arguments, we don't parse interpolation
        // Interpolation is only handled in expression context via parse_string_expr
        Token::String(s) => Literal::String(ctx.intern(s)),
        Token::True => Literal::Boolean(true),
        Token::False => Literal::Boolean(false),
    }
}

/// Result of parsing string interpolation segments
struct InterpolationSegments<'a> {
    /// Literal string parts between interpolations
    parts: Vec<&'a str>,
    /// Paths extracted from ${...} (e.g., "name" or "row.Name")
    paths: Vec<Vec<&'a str>>,
}

/// Parse interpolation segments from a string like "Hello ${name}" or "Hello ${row.Name}"
/// Only simple paths are allowed: identifiers optionally separated by dots
fn parse_interpolation_segments(s: &str) -> Option<InterpolationSegments<'_>> {
    if !s.contains("${") {
        return None;
    }

    let mut parts = Vec::new();
    let mut paths = Vec::new();
    let mut current_pos = 0;

    while current_pos < s.len() {
        if let Some(start) = s[current_pos..].find("${") {
            let start_abs = current_pos + start;
            parts.push(&s[current_pos..start_abs]);

            let path_start = start_abs + 2;
            let end = s[path_start..].find('}')?;
            let path_str = s[path_start..path_start + end].trim();

            // Parse as dot-separated path: "row.Name" -> ["row", "Name"]
            let segments: Vec<&str> = path_str.split('.').collect();
            if segments.is_empty() || segments.iter().any(|s| !is_valid_identifier(s)) {
                return None;
            }

            paths.push(segments);
            current_pos = path_start + end + 1;
        } else {
            parts.push(&s[current_pos..]);
            break;
        }
    }

    if parts.len() == paths.len() {
        parts.push("");
    }

    Some(InterpolationSegments { parts, paths })
}

/// Check if a string is a valid identifier
fn is_valid_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

fn parse_attr_key<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Identifier(ident) => ctx.intern(ident),
        Token::Type => ctx.intern("type"),
        Token::Let => ctx.intern("let"),
    }
}

/// Parse attribute: @name(key = value, ...) or @name("positional")
fn parse_attribute<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Attribute, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let named_arg = parse_attr_key(ctx)
        .then_ignore(just(Token::Eq))
        .then(parse_literal(ctx))
        .map(|(key, value)| AttributeArg::Named { key, value });

    let positional_arg = parse_literal(ctx).map(AttributeArg::Positional);

    let attr_arg = named_arg.or(positional_arg);

    just(Token::At)
        .ignore_then(parse_symbol(ctx))
        .then(
            attr_arg
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or_not(),
        )
        .map_with(|(name, args), e| Attribute {
            name,
            args: args.unwrap_or_default(),
            loc: ctx.to_loc(e.span()),
        })
}

fn parse_provider_argument<'a, I>(
    ctx: &'a AstCtx,
) -> impl Parser<'a, I, ProviderArgument, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let named = parse_symbol(ctx)
        .then_ignore(just(Token::Colon))
        .then(parse_literal(ctx))
        .map(|(name, value)| ProviderArgument::Named { name, value });

    let positional = parse_literal(ctx).map(ProviderArgument::Positional);

    named.or(positional)
}

fn parse_type<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, TypeId, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let primitive = parse_primitive_type()
        .map_with(|ty, e| ctx.alloc_type(TypeKind::Primitive(ty), ctx.to_loc(e.span())));

    let unit = just(Token::LParen)
        .then(just(Token::RParen))
        .map_with(|_, e| ctx.alloc_type(TypeKind::Unit, ctx.to_loc(e.span())));

    // Longest-match: parse Path once, then check for ! suffix (provider vs named)
    let path_based_type = parse_path(ctx)
        .then(
            just(Token::Bang)
                .ignore_then(
                    parse_provider_argument(ctx)
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect()
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .or_not(),
        )
        .map_with(|(path, provider_args), e| {
            let loc = ctx.to_loc(e.span());
            match provider_args {
                Some(args) => ctx.alloc_type(TypeKind::Provider { provider: path, args }, loc),
                None => ctx.alloc_type(TypeKind::Named(path), loc),
            }
        });

    choice((
        primitive,
        unit,
        path_based_type,
    ))
    .then(just(Token::Question).or_not())
    .map_with(|(inner, opt_q), e| {
        if opt_q.is_some() {
            ctx.alloc_type(TypeKind::Optional(inner), ctx.to_loc(e.span()))
        } else {
            inner
        }
    })
}
