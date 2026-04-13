//! Layer 2: parsing (lexer + grammar + AST).
//!
//! Facade module mirroring rust-analyzer's `hir-expand` crate. Exposes the
//! AST types and the entry point `Parser::parse` for source text → AST.
//!
//! This layer is a Salsa firewall for the downstream `def/ty/codegen` layers:
//! edits to the source text recompute the AST, but if the AST hasn't
//! structurally changed (whitespace, comments) downstream queries stay cached.

pub use crate::ast::{
    Argument, Ast, Attribute, AttributeArg, ConstructorParam, Expr, ExprId, ExprKind, Literal, Loc,
    Path, PrimitiveType, ProviderArgument, RecordField, Span, Stmt, StmtId, StmtKind, Type,
    TypeId, TypeKind,
};
pub use crate::parser::lexer::Token;
pub use crate::passes::parse::Parser;
