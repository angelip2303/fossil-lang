use std::collections::HashMap;

use crate::ast::ast::*;
use crate::ast::visitor::{Visitor, walk_ast};
use crate::context::{Interner, Symbol};
use crate::error::ResolveError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingId, BindingRef, ParsedProgram, ResolutionTable, ResolvedProgram};

/// The name resolution pass - resolves all identifiers to their declarations.
///
/// This pass runs in two sub-passes to support forward references:
///
/// ## Pass 2a: Name-Finding (Forward Reference Collection)
/// Collects declarations that can be referenced before they're defined:
/// - Functions (in `let` declarations)
/// - Type aliases (in `type` declarations)
///
/// ## Pass 2b: Name-Binding
/// Resolves every identifier occurrence to its declaration:
/// - Local variables and functions
/// - Module imports and qualified names
/// - Function parameters
/// - Type names
/// - Type providers
///
/// ## Why Two Sub-Passes?
/// Forward references require knowing about declarations before we encounter their uses.
/// For example:
/// ```fossil
/// f()                 // Use comes first
/// let f = fn() -> 42  // Definition comes later
/// ```
/// Without the first pass, we wouldn't know `f` exists when we try to resolve `f()`.
pub struct Resolver<'a> {
    registry: &'a ModuleRegistry,
}

impl<'a> Resolver<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self { registry }
    }

    pub fn resolve(&mut self, program: ParsedProgram) -> Result<ResolvedProgram, ResolveError> {
        let ParsedProgram { ast, symbols } = program;

        // Pass 2a: Name-Finding
        // Collect all forward-referenceable declarations (functions and type aliases)
        let mut collector = ForwardReferenceCollector::new();
        walk_ast(&mut collector, &ast, &symbols)?;

        // Pass 2b: Name-Binding
        // Resolve all identifier references using the collected declarations
        let mut resolution = ResolutionVisitor::new(self.registry, collector.stack);
        walk_ast(&mut resolution, &ast, &symbols)?;

        Ok(ResolvedProgram {
            ast,
            symbols,
            resolution: resolution.resolution,
        })
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

struct ForwardReferenceCollector {
    stack: ScopeStack,
}

impl ForwardReferenceCollector {
    fn new() -> Self {
        Self {
            stack: ScopeStack::new(),
        }
    }
}

impl Visitor for ForwardReferenceCollector {
    type Error = ResolveError;

    fn visit_let_decl(
        &mut self,
        decl_id: StmtId,
        name: Symbol,
        _value: ExprId,
        ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        let rhs = match ast.decls.get(decl_id) {
            Stmt::Let { value, .. } => *value,
            _ => unreachable!(),
        };

        if let ExprKind::Function { .. } = ast.stmts.get(rhs) {
            self.stack
                .current_mut()
                .insert_value(name, BindingRef::Local(decl_id));
        }

        Ok(())
    }

    fn visit_type_decl(
        &mut self,
        decl_id: StmtId,
        name: Symbol,
        _ty: TypeId,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.stack
            .current_mut()
            .insert_type(name, BindingRef::Local(decl_id));
        Ok(())
    }
}

struct ResolutionVisitor<'a> {
    registry: &'a ModuleRegistry,
    stack: ScopeStack,
    resolution: ResolutionTable,
}

impl<'a> ResolutionVisitor<'a> {
    fn new(registry: &'a ModuleRegistry, stack: ScopeStack) -> Self {
        Self {
            registry,
            stack,
            resolution: ResolutionTable::default(),
        }
    }

    fn resolve_qualified_path(
        &self,
        parts: &[Symbol],
        interner: &Interner,
    ) -> Result<Vec<Symbol>, ResolveError> {
        let alias = parts[0];
        let rest = &parts[1..];

        self.stack.resolve_qualified(alias, rest).ok_or_else(|| {
            let name = parts_to_string(parts, interner);
            ResolveError::UndefinedModule(format!("{} (did you forget to import it?)", name))
        })
    }

    fn resolve_binding<F>(
        &self,
        path: &[Symbol],
        interner: &Interner,
        validator: F,
    ) -> Result<BindingId, ResolveError>
    where
        F: Fn(&Binding) -> Result<(), ResolveError>,
    {
        let binding_id = self.registry.resolve(path, interner).map_err(|_| {
            let name = parts_to_string(path, interner);
            ResolveError::UndefinedVariable(name)
        })?;

        validator(self.registry.get(binding_id))?;
        Ok(binding_id)
    }
}

impl<'a> Visitor for ResolutionVisitor<'a> {
    type Error = ResolveError;

    fn visit_identifier(
        &mut self,
        expr_id: ExprId,
        path: &Path,
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        match path {
            Path::Simple(name) => {
                if let Some(binding) = self.stack.lookup_value(*name) {
                    self.resolution.exprs.insert(expr_id, binding);
                } else {
                    let binding_id =
                        self.resolve_binding(&[*name], interner, |binding| match binding {
                            Binding::Function(_) => Ok(()),
                            Binding::Provider(_) => {
                                let name_str = interner.resolve(*name).to_string();
                                Err(ResolveError::NotAFunction(name_str))
                            }
                        })?;

                    self.resolution
                        .exprs
                        .insert(expr_id, BindingRef::Module(binding_id));
                }

                Ok(())
            }

            Path::Qualified(parts) => {
                let full_path = self.resolve_qualified_path(parts, interner)?;

                let binding_id =
                    self.resolve_binding(&full_path, interner, |binding| match binding {
                        Binding::Function(_) => Ok(()),
                        Binding::Provider(_) => {
                            let name = parts_to_string(&full_path, interner);
                            Err(ResolveError::NotAFunction(name))
                        }
                    })?;

                self.resolution
                    .exprs
                    .insert(expr_id, BindingRef::Module(binding_id));
                Ok(())
            }
        }
    }

    fn visit_function(
        &mut self,
        expr_id: ExprId,
        params: &[Symbol],
        body: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        let mut scope = Scope::default();
        for param in params {
            scope.insert_value(*param, BindingRef::Parameter { function: expr_id });
        }

        self.stack.push(scope);
        self.visit_expr(body, ast, interner)?;
        self.stack.pop();

        Ok(())
    }

    fn visit_type_named(
        &mut self,
        type_id: TypeId,
        path: &Path,
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
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

    fn visit_type_provider(
        &mut self,
        type_id: TypeId,
        provider: &Path,
        _args: &[Literal],
        _ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        let full_path = match provider {
            Path::Simple(name) => vec![*name],
            Path::Qualified(parts) => self.resolve_qualified_path(parts, interner)?,
        };

        let binding_id = self.resolve_binding(&full_path, interner, |binding| match binding {
            Binding::Provider(_) => Ok(()),
            Binding::Function(_) => {
                let name = parts_to_string(&full_path, interner);
                Err(ResolveError::NotAProvider(name))
            }
        })?;

        self.resolution
            .providers
            .insert(type_id, BindingRef::Module(binding_id));
        Ok(())
    }
}

fn parts_to_string(parts: &[Symbol], interner: &Interner) -> String {
    parts
        .iter()
        .map(|s| interner.resolve(*s))
        .collect::<Vec<_>>()
        .join("::")
}
