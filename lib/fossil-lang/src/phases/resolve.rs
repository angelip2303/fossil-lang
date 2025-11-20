use std::collections::HashMap;

use crate::ast::visitor::{Visitor, walk_ast};
use crate::ast::*;
use crate::context::{Interner, Symbol};
use crate::error::ResolveError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingRef, ParsedProgram, ResolutionTable, ResolvedProgram};

pub struct Resolver<'a> {
    registry: &'a ModuleRegistry,
}

impl<'a> Resolver<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self { registry }
    }

    pub fn resolve(&mut self, program: ParsedProgram) -> Result<ResolvedProgram, ResolveError> {
        let visitor = ResolverVisitor::new(self.registry);
        visitor.resolve(program)
    }
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    fn pop(&mut self) -> Option<Scope> {
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
        }
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn lookup_value(&self, name: Symbol) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_value(name))
    }

    fn lookup_type(&self, name: Symbol) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_type(name))
    }

    fn resolve_qualified(&self, alias: Symbol, rest: &[Symbol]) -> Option<Vec<Symbol>> {
        self.scopes.iter().rev().find_map(|scope| {
            scope
                .imports
                .get(&alias)
                .map(|prefix| prefix.iter().chain(rest).copied().collect())
        })
    }
}

#[derive(Default)]
struct Scope {
    values: HashMap<Symbol, BindingRef>,
    types: HashMap<Symbol, BindingRef>,
    imports: HashMap<Symbol, Vec<Symbol>>,
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

    fn add_import(&mut self, alias: Symbol, module_path: Vec<Symbol>) {
        self.imports.insert(alias, module_path);
    }
}

/// Resolver using the elegant visitor pattern
///
/// Notice how clean this is! We only implement the methods for nodes
/// we care about. All the traversal logic is handled automatically.
pub struct ResolverVisitor<'a> {
    registry: &'a ModuleRegistry,
    stack: ScopeStack,
    resolution: ResolutionTable,
}

impl<'a> ResolverVisitor<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            stack: ScopeStack::new(),
            resolution: ResolutionTable::default(),
        }
    }

    pub fn resolve(mut self, program: ParsedProgram) -> Result<ResolvedProgram, ResolveError> {
        let ParsedProgram { ast, symbols } = program;

        // Phase 1: Hoisting
        self.hoist_declarations(&ast, &symbols)?;

        // Phase 2: Resolution using visitor
        walk_ast(&mut self, &ast, &symbols)?;

        Ok(ResolvedProgram {
            ast,
            symbols,
            resolution: self.resolution,
        })
    }

    fn hoist_declarations(&mut self, ast: &Ast, symbols: &Interner) -> Result<(), ResolveError> {
        for (id, decl) in ast.decls.iter() {
            match decl {
                Decl::Let { name, .. } => {
                    self.stack
                        .current_mut()
                        .insert_value(*name, BindingRef::Local(id));
                }

                Decl::Type { name, .. } => {
                    self.stack
                        .current_mut()
                        .insert_type(*name, BindingRef::Local(id));
                }

                Decl::Import { module, alias } => {
                    let module_path = module.as_slice().to_vec();

                    self.registry.resolve(&module_path, symbols).map_err(|_| {
                        let name = parts_to_string(&module_path, symbols);
                        ResolveError::UndefinedModule(name)
                    })?;

                    self.stack.current_mut().add_import(*alias, module_path);
                }

                Decl::Expr(_) => {}
            }
        }
        Ok(())
    }

    fn resolve_value_path(
        &mut self,
        expr_id: ExprId,
        path: &Path,
        interner: &Interner,
    ) -> Result<(), ResolveError> {
        match path {
            Path::Simple(name) => {
                if let Some(binding) = self.stack.lookup_value(*name) {
                    self.resolution.exprs.insert(expr_id, binding);
                    return Ok(());
                }

                match self.registry.resolve(&[*name], interner) {
                    Ok(binding_id) => match self.registry.get(binding_id) {
                        Binding::Function(_) => {
                            self.resolution
                                .exprs
                                .insert(expr_id, BindingRef::Module(binding_id));
                            Ok(())
                        }
                        Binding::Provider(_) => {
                            let name_str = interner.resolve(*name).to_string();
                            Err(ResolveError::NotAFunction(name_str))
                        }
                    },
                    Err(_) => {
                        let name_str = interner.resolve(*name).to_string();
                        Err(ResolveError::UndefinedVariable(name_str))
                    }
                }
            }

            Path::Qualified(parts) => {
                let alias = parts[0];
                let rest = &parts[1..];

                let full_path = self
                    .stack
                    .resolve_qualified(alias, rest)
                    .unwrap_or_else(|| parts.clone());

                match self.registry.resolve(&full_path, interner) {
                    Ok(binding_id) => match self.registry.get(binding_id) {
                        Binding::Function(_) => {
                            self.resolution
                                .exprs
                                .insert(expr_id, BindingRef::Module(binding_id));
                            Ok(())
                        }
                        Binding::Provider(_) => {
                            let name = parts_to_string(&full_path, interner);
                            Err(ResolveError::NotAFunction(name))
                        }
                    },
                    Err(_) => {
                        let name = parts_to_string(&full_path, interner);
                        Err(ResolveError::UndefinedVariable(name))
                    }
                }
            }
        }
    }

    fn resolve_type_path(
        &mut self,
        type_id: TypeId,
        path: &Path,
        interner: &Interner,
    ) -> Result<(), ResolveError> {
        match path {
            Path::Simple(name) => {
                if let Some(binding) = self.stack.lookup_type(*name) {
                    self.resolution.types.insert(type_id, binding);
                    Ok(())
                } else {
                    let name_str = interner.resolve(*name).to_string();
                    Err(ResolveError::UndefinedType(name_str))
                }
            }

            Path::Qualified(parts) => {
                let name = parts_to_string(parts, interner);
                Err(ResolveError::UndefinedType(name))
            }
        }
    }
}

// ============================================================================
// Visitor Implementation - Only override what we care about!
// ============================================================================

impl<'a> Visitor for ResolverVisitor<'a> {
    type Error = ResolveError;

    // ========================================================================
    // Expression visitors - Only special cases!
    // ========================================================================

    fn visit_identifier(
        &mut self,
        expr_id: ExprId,
        path: &Path,
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        // ONLY the resolution logic - no traversal needed
        self.resolve_value_path(expr_id, path, interner)
    }

    fn visit_function(
        &mut self,
        expr_id: ExprId,
        params: &[Symbol],
        body: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        // ONLY the scope management logic
        let mut scope = Scope::default();

        for param in params {
            scope.insert_value(*param, BindingRef::Parameter { function: expr_id });
        }

        self.stack.push(scope);

        // Visit body (default traversal happens here)
        self.visit_expr(body, ast, interner)?;

        self.stack.pop();
        Ok(())
    }

    // For Application, List, Record, Pipe, etc. - we don't need to implement!
    // The default implementation does the traversal for us.

    // ========================================================================
    // Type visitors - Only special cases!
    // ========================================================================

    fn visit_type_named(
        &mut self,
        type_id: TypeId,
        path: &Path,
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        // ONLY the resolution logic
        self.resolve_type_path(type_id, path, interner)
    }

    fn visit_type_provider(
        &mut self,
        type_id: TypeId,
        provider: &Path,
        _args: &[Literal],
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        // ONLY the provider resolution logic
        let path = provider.as_slice();

        match self.registry.resolve(path, interner) {
            Ok(binding_id) => match self.registry.get(binding_id) {
                Binding::Provider(_) => {
                    self.resolution
                        .providers
                        .insert(type_id, BindingRef::Module(binding_id));
                    Ok(())
                }
                Binding::Function(_) => {
                    let name = parts_to_string(path, interner);
                    Err(ResolveError::NotAProvider(name))
                }
            },
            Err(_) => {
                let name = parts_to_string(path, interner);
                Err(ResolveError::UndefinedProvider(name))
            }
        }
    }
}

fn parts_to_string(parts: &[Symbol], interner: &Interner) -> String {
    parts
        .iter()
        .map(|s| interner.resolve(*s))
        .collect::<Vec<_>>()
        .join("::")
}
