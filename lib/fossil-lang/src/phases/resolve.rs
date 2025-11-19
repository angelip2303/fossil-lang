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

    fn current_mut(&mut self) -> &mut Scope {
        // we at least have the global scope
        self.scopes.last_mut().unwrap()
    }

    /// Lookup a value starting from the innermost scope and moving outward
    fn lookup_value(&self, name: Symbol) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_value(name))
    }

    /// Lookup a type starting from the innermost scope and moving outward
    fn lookup_type(&self, name: Symbol) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_type(name))
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

        // second pass: resolve expressions starting from declarations
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
                let name = match path {
                    Path::Simple(n) => *n,
                    Path::Qualified(parts) => match self.registry.resolve(parts, symbols) {
                        Ok(binding_id) => {
                            resolution
                                .exprs
                                .insert(expr_id, BindingRef::Module(binding_id));
                            return Ok(());
                        }
                        Err(_) => {
                            let name = parts_to_string(parts, symbols);
                            return Err(ResolveError::UndefinedVariable(name));
                        }
                    },
                };

                if let Some(binding) = self.stack.lookup_value(name) {
                    resolution.exprs.insert(expr_id, binding);
                    Ok(())
                } else {
                    // in case a module has been imported globally
                    match self.registry.resolve(&[name], symbols) {
                        Ok(binding_id) => {
                            resolution
                                .exprs
                                .insert(expr_id, BindingRef::Module(binding_id));
                            Ok(())
                        }
                        Err(_) => {
                            let name_str = symbols.resolve(name).to_string();
                            Err(ResolveError::UndefinedVariable(name_str))
                        }
                    }
                }
            }

            Expr::Function { params, body } => {
                let function_id = expr_id;

                let mut function_scope = Scope::default();

                for param in params {
                    function_scope.insert_value(
                        *param,
                        BindingRef::Parameter {
                            function: function_id,
                        },
                    );
                }

                self.stack.push(function_scope);
                self.resolve_expr(*body, ast, symbols, resolution)?;
                self.stack.pop();
                Ok(())
            }

            Expr::List(items) => {
                for item in items {
                    self.resolve_expr(*item, ast, symbols, resolution)?;
                }
                Ok(())
            }

            Expr::Record(fields) => {
                for (_, field_expr) in fields {
                    self.resolve_expr(*field_expr, ast, symbols, resolution)?;
                }
                Ok(())
            }

            Expr::Application { callee, args } => {
                self.resolve_expr(*callee, ast, symbols, resolution)?;
                for arg in args {
                    self.resolve_expr(*arg, ast, symbols, resolution)?;
                }
                Ok(())
            }

            Expr::Unit | Expr::Literal(_) => Ok(()),
        }
    }

    fn resolve_type(
        &mut self,
        type_id: TypeId,
        ast: &Ast,
        symbols: &Interner,
        resolution: &mut ResolutionTable,
    ) -> Result<(), ResolveError> {
        match ast.types.get(type_id) {
            Type::Named(path) => {
                let name = match path {
                    Path::Simple(n) => *n,
                    Path::Qualified(parts) => match self.registry.resolve(parts, symbols) {
                        Ok(binding_id) => {
                            resolution
                                .types
                                .insert(type_id, BindingRef::Module(binding_id));
                            return Ok(());
                        }
                        Err(_) => {
                            let name = parts_to_string(parts, symbols);
                            return Err(ResolveError::UndefinedType(name));
                        }
                    },
                };

                if let Some(binding) = self.stack.lookup_type(name) {
                    resolution.types.insert(type_id, binding);
                    Ok(())
                } else {
                    // in case a module has been imported globally
                    match self.registry.resolve(&[name], symbols) {
                        Ok(binding_id) => {
                            resolution
                                .types
                                .insert(type_id, BindingRef::Module(binding_id));
                            Ok(())
                        }
                        Err(_) => {
                            let name_str = symbols.resolve(name).to_string();
                            Err(ResolveError::UndefinedType(name_str))
                        }
                    }
                }
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
                            Ok(())
                        }

                        _ => {
                            let name = parts_to_string(&path, symbols);
                            Err(ResolveError::NotAProvider(name))
                        }
                    },
                    Err(_) => {
                        let name = parts_to_string(&path, symbols);
                        Err(ResolveError::UndefinedProvider(name))
                    }
                }
            }

            Type::List(inner) => {
                self.resolve_type(*inner, ast, symbols, resolution)?;
                Ok(())
            }

            Type::Record(fields) => {
                for (_, field_ty) in fields {
                    self.resolve_type(*field_ty, ast, symbols, resolution)?;
                }
                Ok(())
            }

            Type::Function(params, ret) => {
                for param in params {
                    self.resolve_type(*param, ast, symbols, resolution)?;
                }

                self.resolve_type(*ret, ast, symbols, resolution)?;
                Ok(())
            }

            Type::Primitive(_) | Type::Var(_) => Ok(()),
        }
    }
}

fn parts_to_string(parts: &[Symbol], symbols: &Interner) -> String {
    parts
        .iter()
        .map(|s| symbols.resolve(*s))
        .collect::<Vec<_>>()
        .join("::")
}
