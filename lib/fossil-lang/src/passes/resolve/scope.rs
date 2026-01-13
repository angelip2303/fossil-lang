//! Scope management for name resolution
//!
//! This module provides the scope stack for tracking bindings during name resolution.

use std::collections::HashMap;

use crate::ast::ast::Path;
use crate::context::{DefId, Symbol};

/// Scope stack for nested scoping
#[derive(Default)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
}

/// A single scope level containing bindings
#[derive(Default)]
pub struct Scope {
    /// Value bindings (let bindings, function parameters)
    pub values: HashMap<Symbol, DefId>,
    /// Type bindings (type definitions)
    pub types: HashMap<Symbol, DefId>,
    /// Import aliases (module imports)
    pub imports: HashMap<Symbol, Path>,
    /// Current module context (for relative path resolution)
    pub current_module: Option<DefId>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    /// Get the current scope depth (for debugging)
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Lookup a value name in the scope stack (searches from innermost to outermost)
    pub fn lookup_value(&self, name: Symbol) -> Option<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.values.get(&name).copied())
    }

    /// Lookup a type name in the scope stack
    pub fn lookup_type(&self, name: Symbol) -> Option<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.types.get(&name).copied())
    }

    /// Resolve an import alias to its path
    #[allow(dead_code)]
    pub fn resolve_import(&self, alias: Symbol) -> Option<&Path> {
        self.scopes.iter().rev().find_map(|s| s.imports.get(&alias))
    }

    /// Get the current module DefId (for relative path resolution)
    pub fn current_module(&self) -> Option<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.current_module)
    }

    /// Set the current module for the current scope
    pub fn set_current_module(&mut self, module_id: DefId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.current_module = Some(module_id);
        }
    }
}
