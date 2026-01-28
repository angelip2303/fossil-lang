//! Module containing the grammar for the Fossil language.
//!
//! This provides several functions for parsing Fossil code. Each of these is
//! designed to handle specific aspects of the language's syntax and semantics.
//! All the functions are named in a consistent manner; namely, they all start with `parse_`.

use std::cell::RefCell;
use std::rc::Rc;

use chumsky::input::IterInput;
use chumsky::pratt::{infix, left, postfix};
use chumsky::prelude::*;
use logos::Logos;

use crate::ast::ast::{
    self as ast, Argument, Ast, Attribute, AttributeArg, Expr, ExprId, ExprKind, Literal, Param,
    Path, PrimitiveType, ProviderArgument, RecordField, Stmt, StmtId, StmtKind, Type, TypeId,
    TypeKind,
};
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
    // Use .then() instead of .ignore_then() to preserve full span
    // Use parse_binding_name to allow both identifiers and underscore (discard pattern)
    let let_stmt = just(Token::Let)
        .then(parse_binding_name(ctx))
        .then(just(Token::Colon).ignore_then(parse_type(ctx)).or_not())
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map_with(|(((_, name), ty), value), e| {
            ctx.alloc_stmt(StmtKind::Let { name, ty, value }, ctx.to_loc(e.span()))
        });

    // Const statement: const name = expr
    let const_stmt = just(Token::Const)
        .then(parse_symbol(ctx))
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map_with(|((_, name), value), e| {
            ctx.alloc_stmt(StmtKind::Const { name, value }, ctx.to_loc(e.span()))
        });

    // Type statement with optional leading attributes:
    // #[rdf(type = "...", id = "...")]
    // type Name = Type
    let type_stmt = parse_attribute(ctx)
        .repeated()
        .collect::<Vec<_>>()
        .then(just(Token::Type))
        .then(parse_symbol(ctx))
        .then(
            // Detect invalid type annotation pattern: `: SomeType`
            just(Token::Colon)
                .ignore_then(parse_type(ctx))
                .map_with(|_, e| Some(e.span()))
                .or_not()
                .map(|opt| opt.flatten())
        )
        .then_ignore(just(Token::Eq))
        .then(parse_type(ctx))
        .validate(|((((attrs, _), name), invalid_annotation_span), ty), e, emitter| {
            if let Some(annotation_span) = invalid_annotation_span {
                emitter.emit(Rich::custom(
                    annotation_span,
                    "Type declarations cannot have type annotations. Remove the `: <type>` after the name."
                ));
            }
            ctx.alloc_stmt(StmtKind::Type { name, ty, attrs }, ctx.to_loc(e.span()))
        });

    // Trait method signature: `name: type`
    let trait_method = parse_symbol(ctx)
        .then_ignore(just(Token::Colon))
        .then(parse_type(ctx))
        .map(|(name, ty)| ast::TraitMethod { name, ty });

    // Trait statement: trait Name { method: type, ... }
    let trait_stmt = just(Token::Trait)
        .then(parse_symbol(ctx))
        .then(
            trait_method
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with(|((_, name), methods), e| {
            ctx.alloc_stmt(StmtKind::Trait { name, methods }, ctx.to_loc(e.span()))
        });

    // Impl method: `name = expr`
    let impl_method = parse_symbol(ctx)
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map(|(name, value)| (name, value));

    // Impl statement: impl Trait for Type { method = expr, ... }
    let impl_stmt = just(Token::Impl)
        .then(parse_path(ctx))
        .then_ignore(just(Token::For))
        .then(parse_path(ctx))
        .then(
            impl_method
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with(|(((_, trait_name), type_name), methods), e| {
            ctx.alloc_stmt(
                StmtKind::Impl {
                    trait_name,
                    type_name,
                    methods,
                },
                ctx.to_loc(e.span()),
            )
        });

    let expr_stmt =
        expr.map_with(|expr, e| ctx.alloc_stmt(StmtKind::Expr(expr), ctx.to_loc(e.span())));

    choice((
        let_stmt, const_stmt, type_stmt, trait_stmt, impl_stmt, expr_stmt,
    ))
}

fn parse_symbol<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ctx.intern(ident) }
}

/// Parse a binding name for let statements - accepts identifiers OR underscore (discard pattern)
fn parse_binding_name<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Identifier(ident) => ctx.intern(ident),
        Token::Underscore => ctx.intern("_"),
    }
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

/// Parse a parameter name - accepts identifiers or the `self` keyword
fn parse_param_name<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Identifier(ident) => ctx.intern(ident),
        Token::SelfKw => ctx.intern("self"),
    }
}

/// Parse a parameter with optional type annotation and default value:
/// `name`, `name: type`, `name = default`, or `name: type = default`
fn parse_param<'a, I>(
    ctx: &'a AstCtx,
    expr: impl Parser<'a, I, ExprId, ParserError<'a>> + Clone + 'a,
) -> impl Parser<'a, I, Param, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    parse_param_name(ctx)
        .then(just(Token::Colon).ignore_then(parse_type(ctx)).or_not())
        .then(just(Token::Eq).ignore_then(expr).or_not())
        .map(|((name, ty), default)| Param { name, ty, default })
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

        // Parse `self` keyword as an identifier expression
        let self_expr = just(Token::SelfKw).map_with(|_, e| {
            let self_sym = ctx.intern("self");
            ctx.alloc_expr(
                ExprKind::Identifier(Path::Simple(self_sym)),
                ctx.to_loc(e.span()),
            )
        });

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
        let string_expr = select! { Token::String(s) => s }.map_with(move |s: &str, e| {
            let loc = ctx.to_loc(e.span());

            // Try to parse as interpolated string
            if let Some((parts, expr_with_offsets)) = parse_interpolation_segments(s) {
                // Parse each expression string with file-absolute spans
                let mut parsed_exprs = Vec::new();
                // Base offset: string token start + 1 (for opening quote)
                let string_content_base = loc.span.start + 1;

                for (expr_str, offset_in_content) in expr_with_offsets {
                    // Absolute position of this expression's content in the file
                    let expr_base_offset = string_content_base + offset_in_content;

                    // Re-parse the expression string using logos + chumsky
                    // Offset all token spans to be file-absolute
                    let lexer = Token::lexer(expr_str);
                    let tokens: Vec<_> = lexer
                        .spanned()
                        .map(|(tok, span)| {
                            let token = match tok {
                                Ok(t) => t,
                                Err(_) => Token::Identifier("error"),
                            };
                            // Offset span to be file-absolute
                            let abs_span = SimpleSpan::from(
                                (span.start + expr_base_offset)..(span.end + expr_base_offset),
                            );
                            (token, abs_span)
                        })
                        .collect();

                    // Parse as expression using IterInput
                    let len = expr_str.len();
                    let eoi = SimpleSpan::from((len + expr_base_offset)..(len + expr_base_offset));
                    let input = IterInput::new(tokens.into_iter(), eoi);
                    let parsed = parse_expr(ctx).parse(input).into_result();

                    match parsed {
                        Ok(expr_id) => parsed_exprs.push(expr_id),
                        Err(_) => {
                            // If parsing fails, create an error placeholder
                            let error_lit = ctx.alloc_expr(
                                ExprKind::Literal(Literal::String(ctx.intern(expr_str))),
                                loc.clone(),
                            );
                            parsed_exprs.push(error_lit);
                        }
                    }
                }

                // Intern the string parts
                let interned_parts: Vec<_> = parts.iter().map(|p| ctx.intern(p)).collect();

                ctx.alloc_expr(
                    ExprKind::StringInterpolation {
                        parts: interned_parts,
                        exprs: parsed_exprs,
                    },
                    loc,
                )
            } else {
                // Regular string literal
                ctx.alloc_expr(ExprKind::Literal(Literal::String(ctx.intern(s))), loc)
            }
        });

        // Combine all literal types
        let literal = non_string_literal.or(string_expr);

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
                parse_param(ctx, expr.clone())
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

        // Parse named argument: `name: expr`
        let named_arg = parse_symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(name, value)| Argument::Named { name, value });

        // Parse positional argument: just `expr`
        let positional_arg = expr.clone().map(Argument::Positional);

        // Try named first (since it's more specific), fall back to positional
        let argument = named_arg.or(positional_arg);

        let application = path
            .clone()
            .then(
                argument
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
        let atom = choice((
            function,
            application,
            unit,
            literal,
            list,
            block,
            record,
            self_expr,
            path,
        ))
        .map_with(|expr, e| (expr, e.span()))
        .boxed();

        atom.pratt((
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
        .map(|expr| expr.0)
    })
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

/// Parse interpolation segments from a string like "Hello ${name}, you are ${age}!"
/// Returns (parts, expr_with_offsets) where expr_with_offsets contains (expr_str, byte_offset_in_content)
/// The byte offset is the position of the expression content within the string content (after `${`)
fn parse_interpolation_segments(s: &str) -> Option<(Vec<&str>, Vec<(&str, usize)>)> {
    // Quick check: does the string contain any interpolation?
    if !s.contains("${") {
        return None;
    }

    let mut parts = Vec::new();
    let mut exprs = Vec::new();
    let mut current_pos = 0;
    let bytes = s.as_bytes();

    while current_pos < s.len() {
        // Find next ${
        if let Some(start) = s[current_pos..].find("${") {
            let start_abs = current_pos + start;
            // Add the literal part before ${
            parts.push(&s[current_pos..start_abs]);

            // Find matching }
            // We need to handle nested braces for expressions like ${obj.field}
            let expr_start = start_abs + 2;
            let mut brace_depth = 1;
            let mut expr_end = expr_start;

            while expr_end < s.len() && brace_depth > 0 {
                match bytes[expr_end] {
                    b'{' => brace_depth += 1,
                    b'}' => brace_depth -= 1,
                    _ => {}
                }
                if brace_depth > 0 {
                    expr_end += 1;
                }
            }

            if brace_depth != 0 {
                // Unmatched brace - treat as regular string
                return None;
            }

            exprs.push((&s[expr_start..expr_end], expr_start));
            current_pos = expr_end + 1; // Skip past the closing }
        } else {
            // No more interpolations, add remaining text
            parts.push(&s[current_pos..]);
            break;
        }
    }

    // If we ended at an interpolation, add empty trailing part
    if parts.len() == exprs.len() {
        parts.push("");
    }

    Some((parts, exprs))
}

/// Parse attribute key - accepts identifiers AND reserved keywords like 'type'
///
/// Inside attributes, we allow keywords to be used as keys:
/// `#[rdf(type = "...", id = "...")]`
fn parse_attr_key<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Identifier(ident) => ctx.intern(ident),
        Token::Type => ctx.intern("type"),
        Token::Trait => ctx.intern("trait"),
        Token::Impl => ctx.intern("impl"),
        Token::For => ctx.intern("for"),
        Token::Let => ctx.intern("let"),
        Token::Const => ctx.intern("const"),
    }
}

/// Parse attribute: #[name(key = value, key2 = value2)]
///
/// Examples:
/// - `#[rdf(uri = "http://example.com")]`
/// - `#[rdf(type = "http://schema.org/Person", id = "...")]`
fn parse_attribute<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Attribute, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    // Parse a single named argument: key = value
    // Use parse_attr_key to allow keywords like 'type' as keys
    let named_arg = parse_attr_key(ctx)
        .then_ignore(just(Token::Eq))
        .then(parse_literal(ctx))
        .map(|(key, value)| AttributeArg { key, value });

    just(Token::Hash)
        .ignore_then(just(Token::LBracket))
        .ignore_then(parse_symbol(ctx))
        .then(
            named_arg
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or_not(),
        )
        .then_ignore(just(Token::RBracket))
        .map_with(|(name, args), e| Attribute {
            name,
            args: args.unwrap_or_default(),
            loc: ctx.to_loc(e.span()),
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
            .then_ignore(just(Token::Arrow))
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
                .map_with(|fields, e| {
                    ctx.alloc_type(TypeKind::Record(fields), ctx.to_loc(e.span()))
                })
        };

        // Parse named provider argument: `name: literal`
        let named_provider_arg = parse_symbol(ctx)
            .then_ignore(just(Token::Colon))
            .then(parse_literal(ctx))
            .map(|(name, value)| ProviderArgument::Named { name, value });

        // Parse positional provider argument: just `literal`
        let positional_provider_arg = parse_literal(ctx).map(ProviderArgument::Positional);

        // Parse const ref provider argument: bare identifier referencing a const
        let const_ref_provider_arg = parse_symbol(ctx).map(ProviderArgument::ConstRef);

        // Try named first (has `name: value`), then literal, then bare identifier
        let provider_argument = named_provider_arg
            .or(positional_provider_arg)
            .or(const_ref_provider_arg);

        // Parse type provider invocation: csv!("file.csv") or csv!(path: "file.csv", delimiter: ";")
        let provider = parse_path(ctx)
            .then_ignore(just(Token::Bang))
            .then(
                provider_argument
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map_with(|(path, args), e| {
                ctx.alloc_type(
                    TypeKind::Provider {
                        provider: path,
                        args,
                    },
                    ctx.to_loc(e.span()),
                )
            });

        // Parse Name or Name<Type, Type, ...>
        let named_or_app = parse_path(ctx)
            .then(
                ty.clone()
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect()
                    .delimited_by(just(Token::LAngle), just(Token::RAngle))
                    .or_not(),
            )
            .map_with(|(path, opt_args), e| match opt_args {
                Some(args) => {
                    ctx.alloc_type(TypeKind::App { ctor: path, args }, ctx.to_loc(e.span()))
                }
                None => ctx.alloc_type(TypeKind::Named(path), ctx.to_loc(e.span())),
            });

        // Parse `self` keyword as a type (used in trait method signatures)
        let self_type = just(Token::SelfKw).map_with(|_, e| {
            let self_sym = ctx.intern("self");
            ctx.alloc_type(
                TypeKind::Named(Path::Simple(self_sym)),
                ctx.to_loc(e.span()),
            )
        });

        choice((
            primitive,
            unit,
            function,
            list,
            record,
            provider,
            named_or_app,
            self_type,
        ))
    })
}
