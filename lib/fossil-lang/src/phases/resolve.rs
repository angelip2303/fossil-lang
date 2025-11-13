use std::collections::HashMap;

use crate::ast::*;
use crate::context::{Interner, Symbol};
use crate::error::ResolveError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingRef, ParsedProgram, ResolutionTable, ResolvedProgram};

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
    scope: Scope,
}

impl<'a> Resolver<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            scope: Default::default(),
        }
    }

    /// Resolve names in a parsed program
    pub fn resolve(&mut self, program: ParsedProgram) -> Result<ResolvedProgram, ResolveError> {
        let ParsedProgram { mut ast, symbols } = program;
        let mut resolution = ResolutionTable::default();

        // first pass: hoisting
        for (id, decl) in ast.decls.iter() {
            match decl {
                Decl::Let { name, .. } => self.scope.insert_value(*name, BindingRef::Local(id)),
                Decl::Type { name, .. } => self.scope.insert_type(*name, BindingRef::Local(id)),
                _ => {}
            }
        }

        // second pass: resolve
        self.resolve_exprs(&mut ast, &symbols, &mut resolution)?;
        self.resolve_types(&mut ast, &symbols, &mut resolution)?;

        Ok(ResolvedProgram {
            ast,
            symbols,
            resolution,
        })
    }

    fn resolve_exprs(
        &self,
        ast: &Ast,
        symbols: &Interner,
        resolution: &mut ResolutionTable,
    ) -> Result<(), ResolveError> {
        for (expr_id, expr) in ast.exprs.iter() {
            match expr {
                Expr::Identifier(path) => {
                    let binding = match path {
                        Path::Simple(name) => {
                            if let Some(binding_ref) = self.scope.lookup_value(*name) {
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

                _ => {}
            }
        }
        Ok(())
    }

    fn resolve_types(
        &mut self,
        ast: &mut Ast,
        symbols: &Interner,
        resolution: &mut ResolutionTable,
    ) -> Result<(), ResolveError> {
        for (id, ty) in ast.types.iter() {
            match ty.clone() {
                Type::Named(name) => {
                    match self.scope.lookup_type(name) {
                        Some(binding) => resolution.types.insert(id, binding),
                        None => {
                            let name = parts_to_string(&[name], symbols);
                            return Err(ResolveError::UndefinedType(name));
                        }
                    };
                }

                Type::Provider { provider, .. } => {
                    let path = match provider {
                        Path::Simple(s) => vec![s],
                        Path::Qualified(q) => q,
                    };

                    match self.registry.resolve(&path, symbols) {
                        Ok(binding) => match self.registry.get(binding) {
                            Binding::Provider(_) => {
                                resolution.providers.insert(id, BindingRef::Module(binding));
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
