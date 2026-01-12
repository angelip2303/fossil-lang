use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::thir;
use crate::ast::*;
use crate::context::{DefId, Definitions, Interner, Kind, TypeConstructorInfo, TypeMetadata};
use crate::passes::resolve::scope::Scope;
use crate::traits::function::FunctionImpl;

pub mod expand;
pub mod lower;
pub mod parse;
pub mod resolve;
pub mod typecheck;

#[derive(Clone)]
pub struct GlobalContext {
    pub interner: Interner,
    pub definitions: Definitions,
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    /// Registry of type constructors (generic types like List, Entity, Option)
    pub type_constructors: HashMap<DefId, TypeConstructorInfo>,
    /// Cached DefId for the List type constructor (builtin)
    pub list_type_ctor: Option<DefId>,
}

impl GlobalContext {
    pub fn new() -> Self {
        let mut gcx = Self {
            interner: Interner::default(),
            definitions: Definitions::default(),
            type_metadata: HashMap::new(),
            type_constructors: HashMap::new(),
            list_type_ctor: None,
        };

        // Register builtin type constructors
        gcx.register_builtin_type_constructors();

        gcx
    }

    /// Register builtin type constructors
    ///
    /// Currently only registers `List` as a builtin generic type.
    fn register_builtin_type_constructors(&mut self) {
        use crate::context::DefKind;

        // Register List<T> as a builtin type constructor
        let list_sym = self.interner.intern("List");
        let list_def_id = self.definitions.insert(None, list_sym, DefKind::Type);
        self.type_constructors.insert(
            list_def_id,
            TypeConstructorInfo {
                def_id: list_def_id,
                arity: 1,
                name: list_sym,
                param_kinds: vec![Kind::Type],
            },
        );
        self.list_type_ctor = Some(list_def_id);
    }

    /// Register a type provider
    ///
    /// This allows external crates (like fossil-providers) to register
    /// their providers without creating a circular dependency.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_providers::CsvProvider;
    /// use std::sync::Arc;
    ///
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_provider("csv", Arc::new(CsvProvider));
    /// ```
    pub fn register_provider(
        &mut self,
        name: &str,
        provider: std::sync::Arc<dyn crate::traits::provider::TypeProviderImpl>,
    ) {
        use crate::context::DefKind;

        let symbol = self.interner.intern(name);
        self.definitions
            .insert(None, symbol, DefKind::Provider(provider));
    }

    /// Register a type constructor
    ///
    /// This allows external crates (like fossil-stdlib) to register
    /// generic types like Entity, Option, Result, etc.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the type constructor (e.g., "Entity", "Option")
    /// * `arity` - The number of type parameters (e.g., 1 for Entity<T>, 2 for Map<K,V>)
    ///
    /// # Returns
    ///
    /// The DefId of the registered type constructor, which can be used when
    /// constructing `TypeKind::App` instances.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    ///
    /// let mut gcx = GlobalContext::new();
    /// let entity_def_id = gcx.register_type_constructor("Entity", 1);
    /// // Now Entity can be used as Entity<T> in the type system
    /// ```
    pub fn register_type_constructor(&mut self, name: &str, arity: usize) -> DefId {
        use crate::context::DefKind;

        let sym = self.interner.intern(name);

        // Check if already registered
        if let Some(def) = self.definitions.get_by_symbol(sym) {
            return def.id();
        }

        // Register as a type definition
        let def_id = self.definitions.insert(None, sym, DefKind::Type);

        // Store type constructor metadata
        self.type_constructors.insert(
            def_id,
            TypeConstructorInfo {
                def_id,
                arity,
                name: sym,
                param_kinds: vec![Kind::Type; arity],
            },
        );

        def_id
    }

    /// Register a function
    ///
    /// This allows external crates (like fossil-stdlib) to register
    /// their functions without creating a circular dependency.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_stdlib::EntityWrapFunction;
    /// use std::sync::Arc;
    ///
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_function("Entity", "wrap", Arc::new(EntityWrapFunction));
    /// ```
    pub fn register_function(
        &mut self,
        module_name: &str,
        function_name: &str,
        function: impl FunctionImpl + 'static,
    ) {
        use crate::context::DefKind;

        // Get or create module
        let module_symbol = self.interner.intern(module_name);
        let module_id = self
            .definitions
            .get_by_symbol(module_symbol)
            .map(|def| def.id())
            .unwrap_or_else(|| self.definitions.insert(None, module_symbol, DefKind::Mod));

        // Register function within module
        let function_symbol = self.interner.intern(function_name);
        self.definitions.insert(
            Some(module_id),
            function_symbol,
            DefKind::Func(Some(Arc::new(function))),
        );
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Import resolver - validates that imported modules exist
///
/// This is a validation phase that runs early in compilation to ensure
/// all imports reference valid modules or providers.
pub struct ImportResolver {
    pub ast: ast::Ast,
    pub gcx: GlobalContext,
}

impl ImportResolver {
    pub fn new(ast: ast::Ast, gcx: GlobalContext) -> Self {
        Self { ast, gcx }
    }

    /// Validate all imports in the AST
    ///
    /// Returns the AST and GlobalContext unchanged if all imports are valid,
    /// or an error if any import references an undefined module.
    pub fn resolve(self) -> Result<(ast::Ast, GlobalContext), crate::error::CompileError> {
        use crate::ast::ast::StmtKind;
        use crate::context::DefKind;
        use crate::error::{CompileError, CompileErrorKind};

        // Validate each import statement
        for stmt_id in &self.ast.root {
            let stmt = self.ast.stmts.get(*stmt_id);
            if let StmtKind::Import { module, .. } = &stmt.kind {
                // Try to resolve the module path
                let module_def_id = self.gcx.definitions.resolve(module).ok_or_else(|| {
                    CompileError::new(
                        CompileErrorKind::UndefinedModule(module.clone()),
                        stmt.loc.clone(),
                    )
                })?;

                // Verify it's actually a module or provider
                let def = self.gcx.definitions.get(module_def_id);
                match def.kind {
                    DefKind::Mod | DefKind::Provider(_) => {
                        // Valid import target
                    }
                    _ => {
                        return Err(CompileError::new(
                            CompileErrorKind::NotAModule(module.clone()),
                            stmt.loc.clone(),
                        ));
                    }
                }
            }
        }

        Ok((self.ast, self.gcx))
    }
}

/// Prelude - auto-imports for common providers and types
///
/// The prelude automatically makes certain providers and types available
/// without explicit import statements.
pub struct Prelude {
    /// Names of providers/modules to auto-import
    pub items: Vec<&'static str>,
}

impl Prelude {
    /// Create the standard prelude
    ///
    /// Currently includes: csv, json, Entity, rdf
    pub fn standard() -> Self {
        Self {
            items: vec!["csv", "json", "Entity", "rdf"],
        }
    }

    /// Apply the prelude to a scope
    ///
    /// Adds all prelude items to the scope's imports if they exist in GlobalContext.
    /// Items that don't exist are silently ignored.
    pub fn apply(&self, scope: &mut Scope, gcx: &mut GlobalContext) {
        use crate::ast::ast::Path;

        for item_name in &self.items {
            let sym = gcx.interner.intern(item_name);
            // Only add if it exists in global definitions
            if gcx.definitions.get_by_symbol(sym).is_some() {
                scope.imports.insert(sym, Path::Simple(sym));
            }
        }
    }
}

pub struct ParsedProgram {
    pub ast: ast::Ast,
    pub gcx: GlobalContext,
}

pub struct HirProgram {
    pub hir: hir::Hir,
    pub gcx: GlobalContext,
}

pub struct ThirProgram {
    pub thir: thir::TypedHir,
    pub gcx: GlobalContext,
}
