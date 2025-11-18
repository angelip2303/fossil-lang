use std::collections::HashMap;

use crate::ast::*;
use crate::context::{Interner, Symbol};
use crate::error::ResolveError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingRef, ParsedProgram, ResolutionTable, ResolvedProgram};

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        Self {
            // start with one global scope
            scopes: vec![Default::default()],
        }
    }

    fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    fn pop(&mut self) -> Option<Scope> {
        // cannot pop the global scope
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
        }
    }

    fn current(&self) -> &Scope {
        // we at least have the global scope
        self.scopes.last().unwrap()
    }

    fn current_mut(&mut self) -> &mut Scope {
        // we at least have the global scope
        self.scopes.last_mut().unwrap()
    }
}

#[derive(Default)]
struct Scope {
    values: HashMap<Symbol, BindingRef>,
    types: HashMap<Symbol, BindingRef>,
}

impl Scope {
    fn insert_value(&mut self, name: Symbol, binding: BindingRef) {
        self.values.insert(name, binding);
    }

    fn lookup_value(&self, name: Symbol) -> Option<BindingRef> {
        self.values.get(&name).copied()
    }

    fn insert_type(&mut self, name: Symbol, binding: BindingRef) {
        self.types.insert(name, binding);
    }

    fn lookup_type(&self, name: Symbol) -> Option<BindingRef> {
        self.types.get(&name).copied()
    }
}

pub struct Resolver<'a> {
    registry: &'a ModuleRegistry,
    stack: ScopeStack,
}

impl<'a> Resolver<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            stack: ScopeStack::new(),
        }
    }

    /// Resolve names in a parsed program
    pub fn resolve(&mut self, program: ParsedProgram) -> Result<ResolvedProgram, ResolveError> {
        let ParsedProgram { ast, symbols } = program;
        let mut resolution = ResolutionTable::default();

        // first pass: hoisting (at the global scope)
        for (id, decl) in ast.decls.iter() {
            match decl {
                Decl::Let { name, .. } => self
                    .stack
                    .current_mut()
                    .insert_value(*name, BindingRef::Local(id)),

                Decl::Type { name, .. } => self
                    .stack
                    .current_mut()
                    .insert_type(*name, BindingRef::Local(id)),

                _ => {}
            }
        }

        // second pass: resolve
        for (_, decl) in ast.decls.iter() {
            match decl {
                Decl::Let { value, .. } => {
                    self.resolve_expr(*value, &ast, &symbols, &mut resolution)?;
                }
                Decl::Expr(expr_id) => {
                    self.resolve_expr(*expr_id, &ast, &symbols, &mut resolution)?;
                }
                Decl::Type { ty, .. } => {
                    self.resolve_type(*ty, &ast, &symbols, &mut resolution)?;
                }
            }
        }

        Ok(ResolvedProgram {
            ast,
            symbols,
            resolution,
        })
    }

    fn resolve_expr(
        &mut self,
        expr_id: ExprId,
        ast: &Ast,
        symbols: &Interner,
        resolution: &mut ResolutionTable,
    ) -> Result<(), ResolveError> {
        match ast.exprs.get(expr_id) {
            Expr::Identifier(path) => {
                let binding = match path {
                    Path::Simple(name) => {
                        if let Some(binding_ref) = self.stack.current().lookup_value(*name) {
                            binding_ref
                        } else {
                            // try to resolve the identifier in the registry as a fallback
                            match self.registry.resolve(&[*name], symbols) {
                                Ok(binding_id) => BindingRef::Module(binding_id),
                                Err(_) => {
                                    let name_str = symbols.resolve(*name).to_string();
                                    return Err(ResolveError::UndefinedVariable(name_str));
                                }
                            }
                        }
                    }

                    Path::Qualified(parts) => match self.registry.resolve(parts, symbols) {
                        Ok(binding_id) => BindingRef::Module(binding_id),
                        Err(_) => {
                            let name = parts_to_string(parts, symbols);
                            return Err(ResolveError::UndefinedVariable(name));
                        }
                    },
                };

                resolution.exprs.insert(expr_id, binding);
            }

            Expr::Function { params, body } => {
                let saved_scope = self.scope.clone();

                for param in params {
                    self.scope
                        .insert_value(*param, BindingRef::Local(DeclId::new(usize::MAX)));
                }

                self.resolve_expr(*body, ast, symbols, resolution)?;

                self.scope = saved_scope;
            }

            _ => {}
        }

        Ok(())
    }

    fn resolve_type(
        &mut self,
        type_id: TypeId,
        ast: &Ast,
        symbols: &Interner,
        resolution: &mut ResolutionTable,
    ) -> Result<(), ResolveError> {
        match ast.types.get(type_id) {
            Type::Named(name) => {
                match self.scope.lookup_type(*name) {
                    Some(binding) => resolution.types.insert(type_id, binding),
                    None => {
                        let name = parts_to_string(&[*name], symbols);
                        return Err(ResolveError::UndefinedType(name));
                    }
                };
            }

            Type::Provider { provider, .. } => {
                let path = match provider.clone() {
                    Path::Simple(s) => vec![s],
                    Path::Qualified(q) => q,
                };

                match self.registry.resolve(&path, symbols) {
                    Ok(binding) => match self.registry.get(binding) {
                        Binding::Provider(_) => {
                            resolution
                                .providers
                                .insert(type_id, BindingRef::Module(binding));
                        }

                        _ => {
                            let name = parts_to_string(&path, symbols);
                            return Err(ResolveError::NotAProvider(name));
                        }
                    },
                    Err(_) => {
                        let name = parts_to_string(&path, symbols);
                        return Err(ResolveError::UndefinedProvider(name));
                    }
                }
            }

            _ => {}
        }

        Ok(())
    }
}

fn parts_to_string(parts: &[Symbol], symbols: &Interner) -> String {
    parts
        .iter()
        .map(|s| symbols.resolve(*s))
        .collect::<Vec<_>>()
        .join(".")
}
