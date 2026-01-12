//! Resolution tables
//!
//! This module contains the resolution table that maps AST nodes to their definitions.

use std::collections::HashMap;

use crate::ast::ast::{Ast, ExprId, StmtId, TypeId};
use crate::context::DefId;
use crate::passes::GlobalContext;

/// Result of name resolution
pub struct ResolvedAst {
    pub ast: Ast,
    pub gcx: GlobalContext,
    pub resolutions: ResolutionTable,
}

/// Resolution table mapping AST nodes to their definitions
#[derive(Default)]
pub struct ResolutionTable {
    pub exprs: HashMap<ExprId, DefId>,
    pub types: HashMap<TypeId, DefId>,
    /// Maps function ExprId to the DefIds of its parameters (in order)
    pub function_params: HashMap<ExprId, Vec<DefId>>,
    /// Maps Let statement ID to the DefId it creates
    pub let_bindings: HashMap<StmtId, DefId>,
}
