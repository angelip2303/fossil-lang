//! Scope management for name resolution
//!
//! This module provides the scope stack for tracking bindings during name resolution.

use std::collections::HashMap;

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

    /// Get the current (innermost) scope immutably
    pub fn current(&self) -> &Scope {
        self.scopes
            .last()
            .expect("SAFETY: ScopeStack always has at least one scope (initialized in new())")
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
}
