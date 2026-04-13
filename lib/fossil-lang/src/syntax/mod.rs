//! Layer 2: parsing (lexer + grammar + AST).
//!
//! Mirror of rust-analyzer's `hir-expand` crate. Exposes the AST types and
//! the entry point `Parser::parse` for source text → AST.
//!
//! This layer is a Salsa firewall for the downstream `def/ty/codegen` layers:
//! edits to the source text recompute the AST, but if the AST hasn't
//! structurally changed (whitespace, comments) downstream queries stay cached.

pub mod ast;
pub mod parse;
pub mod parser;

pub use parse::Parser;
pub use ast::{
    Argument, Ast, Attribute, AttributeArg, ConstructorParam, Expr, ExprId, ExprKind, Literal,
    Loc, Path, PrimitiveType, ProviderArgument, RecordField, Span, Stmt, StmtId, StmtKind, Type,
    TypeId, TypeKind,
};
pub use parser::lexer::Token;
