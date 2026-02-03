//! Module containing the grammar for the Fossil language.
//!
//! This provides several functions for parsing Fossil code. Each of these is
//! designed to handle specific aspects of the language's syntax and semantics.
//! All the functions are named in a consistent manner; namely, they all start with `parse_`.

use std::cell::RefCell;
use std::rc::Rc;

use chumsky::pratt::{infix, left, postfix};
use chumsky::prelude::*;

use crate::ast::ast::{
    self as ast, Argument, Ast, Attribute, AttributeArg, Expr, ExprId, ExprKind, Literal, Param,
    Path, PrimitiveType, ProviderArgument, RecordField, Stmt, StmtId, StmtKind, Type, TypeId,
    TypeKind,
};
use crate::ast::{Loc, SourceId};
use crate::context::{Interner, Symbol};
use crate::parser::lexer::Token;

type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, SimpleSpan>>;

/// Helper enum for parsing record fields (regular or metadata)
enum RecordFieldOrMeta {
    Field(Symbol, ExprId),
    Meta(Symbol, ExprId),
}

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

    let expr_stmt =
        expr.map_with(|expr, e| ctx.alloc_stmt(StmtKind::Expr(expr), ctx.to_loc(e.span())));

    choice((let_stmt, const_stmt, type_stmt, expr_stmt))
}

fn parse_symbol<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! { Token::Identifier(ident) => ctx.intern(ident) }
}

/// Parse a binding name for let statements
fn parse_binding_name<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
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

/// Parse a parameter name - accepts identifiers
fn parse_param_name<'a, I>(ctx: &'a AstCtx) -> impl Parser<'a, I, Symbol, ParserError<'a>> + Clone
where
    I: Input<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
        Token::Identifier(ident) => ctx.intern(ident),
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
                                ExprKind::FieldAccess { expr, field: field_sym },
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

        // Combine all literal types
        let literal = non_string_literal.or(string_expr);

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|items, e| ctx.alloc_expr(ExprKind::List(items), ctx.to_loc(e.span())));

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

        // Named record construction: TypeName { @id = ..., @graph = ..., field = value, ... }
        // Uses = for field assignment (consistent with let syntax)
        // @id and @graph are special metadata fields

        // Regular field: name = expr
        let regular_field = parse_symbol(ctx)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .map(|(name, value)| RecordFieldOrMeta::Field(name, value));

        // Metadata field: @id = expr or @graph = expr
        let meta_field = just(Token::At)
            .ignore_then(parse_symbol(ctx))
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .map(|(name, value)| RecordFieldOrMeta::Meta(name, value));

        // Either regular field or metadata field
        let record_field = meta_field.or(regular_field);

        let named_record = parse_path(ctx)
            .then(
                record_field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with(|(type_path, items), e| {
                let mut fields = Vec::new();
                let mut metadata: ast::RecordMetadata = Vec::new();

                for item in items {
                    match item {
                        RecordFieldOrMeta::Field(name, value) => {
                            fields.push((name, value));
                        }
                        RecordFieldOrMeta::Meta(name, value) => {
                            metadata.push((name, value));
                        }
                    }
                }

                ctx.alloc_expr(
                    ExprKind::NamedRecordConstruction {
                        type_path,
                        fields,
                        metadata,
                    },
                    ctx.to_loc(e.span()),
                )
            });

        // Function with optional leading attributes: #[rdf(...)] fn(r) -> ...
        // Validates that there is no space between 'fn' and '('
        let function = parse_attribute(ctx)
            .repeated()
            .collect::<Vec<_>>()
            .then(just(Token::Func).map_with(|_, e| e.span()))
            .then(just(Token::LParen).map_with(|_, e| e.span()))
            .validate(
                |((attrs, fn_span), paren_span): ((Vec<Attribute>, SimpleSpan), SimpleSpan),
                 _,
                 emitter| {
                    if fn_span.end != paren_span.start {
                        emitter.emit(Rich::custom(
                            paren_span,
                            "space between 'fn' and '(' is not allowed",
                        ));
                    }
                    (attrs, ())
                },
            )
            .then(
                parse_param(ctx, expr.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect(),
            )
            .then_ignore(just(Token::RParen))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone()) // Body is just any expression (including blocks!)
            .map_with(|(((attrs, _), params), body), e| {
                ctx.alloc_expr(
                    ExprKind::Function {
                        params,
                        body,
                        attrs,
                    },
                    ctx.to_loc(e.span()),
                )
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

        // Ordering matters:
        // - Function must come before application/path to avoid parsing 'fn' as identifier
        // - named_record must come before path (TypeName { ... } vs TypeName)
        // - block must be after named_record since both involve { }
        let atom = choice((
            function,
            application,
            unit,
            literal,
            list,
            named_record,
            block,
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

        // Try named first (has `name: value`), then literal, then bare identifier
        let provider_argument = named_provider_arg.or(positional_provider_arg);

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

        choice((
            primitive,
            unit,
            function,
            list,
            record,
            provider,
            named_or_app,
        ))
    })
}
