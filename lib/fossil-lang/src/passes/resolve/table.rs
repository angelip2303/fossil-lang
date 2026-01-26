//! Resolution tables
//!
//! This module contains the resolution table that maps AST nodes to their definitions.

use std::collections::HashMap;

use crate::ast::ast::{Ast, ExprId, StmtId, TypeId};
use crate::context::{DefId, Symbol};
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
    /// Maps Const statement ID to the DefId it creates
    pub const_bindings: HashMap<StmtId, DefId>,
    /// Maps record constructor DefId to the type DefId it constructs
    pub record_constructors: HashMap<DefId, DefId>,
    /// Maps record constructor DefId to its field names (in order)
    pub constructor_fields: HashMap<DefId, Vec<Symbol>>,
    /// Maps Trait statement ID to the DefId of the trait
    pub trait_defs: HashMap<StmtId, DefId>,
    /// Maps (trait_name, method_name) to the DefId of the method
    pub trait_methods: HashMap<(Symbol, Symbol), DefId>,
    /// Maps Impl statement ID to the DefId of the trait being implemented
    pub impl_trait_defs: HashMap<StmtId, DefId>,
    /// Maps Impl statement ID to the DefId of the type implementing the trait
    pub impl_type_defs: HashMap<StmtId, DefId>,
}
