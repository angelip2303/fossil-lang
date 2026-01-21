use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use crate::ast::thir;
use crate::ast::*;
use crate::context::{DefId, Definitions, Interner, Kind, TypeConstructorInfo, TypeMetadata};
use crate::passes::resolve::scope::Scope;
use crate::passes::resolve::table::ResolutionTable;
use crate::traits::function::FunctionImpl;

pub mod expand;
pub mod lower;
pub mod module_loader;
pub mod parse;
pub mod resolve;
pub mod typecheck;

// Re-export key types for external use
pub use module_loader::ModuleLoader;

#[derive(Clone)]
pub struct GlobalContext {
    pub interner: Interner,
    pub definitions: Definitions,
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    /// Registry of type constructors (generic types like List, Entity, Option)
    pub type_constructors: HashMap<DefId, TypeConstructorInfo>,
    /// Cached DefId for the List type constructor (builtin)
    pub list_type_ctor: Option<DefId>,
    /// Pre-interned wildcard symbol `_` for use in type signatures
    /// This is used in FieldSelector types to represent "any field"
    pub wildcard_symbol: crate::context::Symbol,
}

impl GlobalContext {
    pub fn new() -> Self {
        let mut interner = Interner::default();
        // Pre-intern the wildcard symbol for use in type signatures
        let wildcard_symbol = interner.intern("_");

        let mut gcx = Self {
            interner,
            definitions: Definitions::default(),
            type_metadata: HashMap::new(),
            type_constructors: HashMap::new(),
            list_type_ctor: None,
            wildcard_symbol,
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
        provider: impl crate::traits::provider::TypeProviderImpl + 'static,
    ) {
        use crate::context::DefKind;

        let symbol = self.interner.intern(name);
        self.definitions
            .insert(None, symbol, DefKind::Provider(Arc::new(provider)));
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
            .unwrap_or_else(|| {
                self.definitions.insert(
                    None,
                    module_symbol,
                    DefKind::Mod {
                        file_path: None,
                        is_inline: true,
                    },
                )
            });

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

/// Multi-module AST - represents a complete program with multiple modules
///
/// After import resolution, we have multiple module ASTs that need to be
/// compiled together. This structure holds all modules and their compilation order.
pub struct MultiModuleAst {
    /// Map from module DefId to its AST
    pub modules: HashMap<DefId, ast::Ast>,
    /// Topologically sorted module DefIds (dependencies first)
    pub order: Vec<DefId>,
    /// GlobalContext with all module definitions
    pub gcx: GlobalContext,
}

/// Import resolver - validates imports and loads modules from file system
///
/// This phase runs early in compilation to:
/// - Load all required modules from the file system
/// - Validate that all imports reference valid modules or providers
/// - Detect circular import dependencies
/// - Return modules in topological order (dependencies first)
pub struct ImportResolver {
    pub ast: ast::Ast,
    pub gcx: GlobalContext,
}

impl ImportResolver {
    pub fn new(ast: ast::Ast, gcx: GlobalContext) -> Self {
        Self { ast, gcx }
    }

    /// Create a new ImportResolver with a ModuleLoader for file-based imports
    pub fn new_with_loader(loader: ModuleLoader, gcx: GlobalContext) -> ImportResolverWithLoader {
        ImportResolverWithLoader {
            loader,
            gcx,
            visited: HashSet::new(),
            import_graph: HashMap::new(),
        }
    }

    /// Validate all imports in the AST (legacy, for single-file mode)
    ///
    /// Returns the AST and GlobalContext unchanged if all imports are valid,
    /// or an error if any import references an undefined module.
    pub fn resolve(self) -> Result<(ast::Ast, GlobalContext), crate::error::CompileErrors> {
        use crate::ast::ast::StmtKind;
        use crate::context::DefKind;
        use crate::error::{CompileError, CompileErrorKind, CompileErrors};

        let mut errors = CompileErrors::new();

        // Validate each import statement
        for stmt_id in &self.ast.root {
            let stmt = self.ast.stmts.get(*stmt_id);
            if let StmtKind::Import { module, .. } = &stmt.kind {
                // Try to resolve the module path
                let module_def_id = match self.gcx.definitions.resolve(module) {
                    Some(id) => id,
                    None => {
                        errors.push(CompileError::new(
                            CompileErrorKind::UndefinedModule(module.clone()),
                            stmt.loc.clone(),
                        ));
                        continue;
                    }
                };

                // Verify it's actually a module or provider
                let def = self.gcx.definitions.get(module_def_id);
                match def.kind {
                    DefKind::Mod { .. } | DefKind::Provider(_) => {
                        // Valid import target
                    }
                    _ => {
                        errors.push(CompileError::new(
                            CompileErrorKind::NotAModule(module.clone()),
                            stmt.loc.clone(),
                        ));
                    }
                }
            }
        }

        // Return errors if any occurred
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok((self.ast, self.gcx))
    }
}

/// Import resolver with module loader for multi-file projects
pub struct ImportResolverWithLoader {
    pub(crate) loader: ModuleLoader,
    gcx: GlobalContext,
    visited: HashSet<PathBuf>,                // Cycle detection
    import_graph: HashMap<DefId, Vec<DefId>>, // Dependency graph for topological sort
}

impl ImportResolverWithLoader {
    /// Resolve all imports recursively starting from a root file
    ///
    /// # Arguments
    /// - `root_file`: Path to the entry point file (e.g., main.fossil)
    ///
    /// # Returns
    /// MultiModuleAst with all modules loaded and topologically sorted
    pub fn resolve_all(
        mut self,
        root_file: PathBuf,
    ) -> Result<MultiModuleAst, crate::error::CompileError> {
        use crate::passes::parse::Parser;

        // 1. Parse root module directly (not through loader)
        let src = std::fs::read_to_string(&root_file).map_err(|e| {
            crate::error::CompileError::new(
                crate::error::CompileErrorKind::Runtime(format!("Failed to read root file: {}", e)),
                crate::ast::Loc::generated(),
            )
        })?;

        let source_id = 0;
        let parsed =
            Parser::parse_with_context(&src, source_id, self.gcx.clone()).map_err(|errors| {
                // TODO(Phase 2.2): Handle all errors once pipeline supports CompileErrors
                errors.0.into_iter().next().unwrap_or_else(|| {
                    crate::error::CompileError::new(
                        crate::error::CompileErrorKind::Parse(crate::context::Symbol::synthetic()),
                        crate::ast::Loc::generated(),
                    )
                })
            })?;

        // Update our gcx with the parser's (which has new symbols)
        self.gcx = parsed.gcx;

        // Create module name from file
        let module_name = root_file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main");
        let module_sym = self.gcx.interner.intern(module_name);

        // Create root module DefId
        let root_def_id = self.gcx.definitions.insert(
            None,
            module_sym,
            crate::context::DefKind::Mod {
                file_path: Some(root_file.clone()),
                is_inline: false,
            },
        );

        let root_ast = parsed.ast;

        // Cache this root module in the loader to avoid re-loading
        self.loader
            .cache
            .insert(root_file.clone(), (root_def_id, source_id));

        // 2. Recursively load all imports
        let mut module_asts = HashMap::new();
        self.resolve_module_imports(root_ast, root_def_id, &root_file, &mut module_asts)?;

        // 3. Topologically sort modules by dependencies
        let sorted_modules = self.topological_sort()?;

        Ok(MultiModuleAst {
            modules: module_asts,
            order: sorted_modules,
            gcx: self.gcx,
        })
    }

    /// Recursively resolve imports for a module
    fn resolve_module_imports(
        &mut self,
        ast: ast::Ast,
        module_def_id: DefId,
        current_file: &PathBuf,
        all_asts: &mut HashMap<DefId, ast::Ast>,
    ) -> Result<(), crate::error::CompileError> {
        use crate::ast::ast::StmtKind;
        use crate::error::{CompileError, CompileErrorKind};

        // Cycle detection
        if self.visited.contains(current_file) {
            return Err(CompileError::new(
                CompileErrorKind::Runtime("Circular import detected".into()),
                crate::ast::Loc::generated(),
            )
            .with_context(format!(
                "Module '{}' creates a circular dependency",
                current_file.display()
            )));
        }
        self.visited.insert(current_file.clone());

        // Initialize dependency list for this module
        self.import_graph
            .entry(module_def_id)
            .or_insert_with(Vec::new);

        // Process each import in this module
        for stmt_id in &ast.root {
            let stmt = ast.stmts.get(*stmt_id);
            if let StmtKind::Import { module, .. } = &stmt.kind {
                // Load the imported module
                let (maybe_imported_ast, imported_def_id, _) =
                    self.loader
                        .load_module(module, current_file, &mut self.gcx)?;

                // Track dependency
                self.import_graph
                    .entry(module_def_id)
                    .or_insert_with(Vec::new)
                    .push(imported_def_id);

                // If this is a new module (not cached), recursively process its imports
                if let Some(imported_ast) = maybe_imported_ast {
                    // Get the file path for the imported module
                    let imported_path = {
                        let imported_def = self.gcx.definitions.get(imported_def_id);
                        if let crate::context::DefKind::Mod {
                            file_path: Some(path),
                            ..
                        } = &imported_def.kind
                        {
                            Some(path.clone())
                        } else {
                            None
                        }
                    };

                    if let Some(path) = imported_path {
                        self.resolve_module_imports(
                            imported_ast,
                            imported_def_id,
                            &path,
                            all_asts,
                        )?;
                    }
                }
            }
        }

        // Store this module's AST
        all_asts.insert(module_def_id, ast);

        // Remove from visited (allow other paths to this module)
        self.visited.remove(current_file);

        Ok(())
    }

    /// Topologically sort modules by dependencies
    ///
    /// Returns modules in dependency order (modules with no dependencies first)
    fn topological_sort(&self) -> Result<Vec<DefId>, crate::error::CompileError> {
        let mut sorted = Vec::new();
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        // Visit each module
        for &module_id in self.import_graph.keys() {
            if !visited.contains(&module_id) {
                self.topological_sort_visit(module_id, &mut visited, &mut rec_stack, &mut sorted)?;
            }
        }

        Ok(sorted)
    }

    /// DFS visit for topological sort with cycle detection
    fn topological_sort_visit(
        &self,
        module_id: DefId,
        visited: &mut HashSet<DefId>,
        rec_stack: &mut HashSet<DefId>,
        sorted: &mut Vec<DefId>,
    ) -> Result<(), crate::error::CompileError> {
        // Mark as being visited (for cycle detection)
        rec_stack.insert(module_id);

        // Visit dependencies
        if let Some(deps) = self.import_graph.get(&module_id) {
            for &dep_id in deps {
                if rec_stack.contains(&dep_id) {
                    // Cycle detected
                    return Err(crate::error::CompileError::new(
                        crate::error::CompileErrorKind::Runtime(
                            "Circular import dependency detected".into(),
                        ),
                        crate::ast::Loc::generated(),
                    ));
                }

                if !visited.contains(&dep_id) {
                    self.topological_sort_visit(dep_id, visited, rec_stack, sorted)?;
                }
            }
        }

        // Mark as fully visited
        rec_stack.remove(&module_id);
        visited.insert(module_id);

        // Add to sorted list (dependencies come before dependents)
        sorted.push(module_id);

        Ok(())
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
    pub resolutions: ResolutionTable,
}

pub struct ThirProgram {
    pub thir: thir::TypedHir,
    pub gcx: GlobalContext,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_import_resolver_multi_module() {
        // Create temporary directory structure
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create main.fossil with import
        let main_file = root.join("main.fossil");
        fs::write(&main_file, "open utils\nlet x = Utils::helper()").unwrap();

        // Create utils.fossil
        let utils_file = root.join("utils.fossil");
        fs::write(&utils_file, "let helper = fn() -> 42").unwrap();

        // Create resolver with loader
        let loader = ModuleLoader::new(root.clone());
        let gcx = GlobalContext::default();
        let resolver = ImportResolver::new_with_loader(loader, gcx);

        // Resolve all modules
        let result = resolver.resolve_all(main_file);
        assert!(result.is_ok());

        let multi_ast = result.unwrap();
        assert_eq!(multi_ast.modules.len(), 2); // main + utils
        assert_eq!(multi_ast.order.len(), 2);
    }

    #[test]
    fn test_import_resolver_nested_imports() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create main.fossil
        let main_file = root.join("main.fossil");
        fs::write(&main_file, "open utils").unwrap();

        // Create utils.fossil that imports helpers
        let utils_file = root.join("utils.fossil");
        fs::write(
            &utils_file,
            "open helpers\nlet use_helper = fn() -> Helpers::help()",
        )
        .unwrap();

        // Create helpers.fossil
        let helpers_file = root.join("helpers.fossil");
        fs::write(&helpers_file, "let help = fn() -> 42").unwrap();

        let loader = ModuleLoader::new(root.clone());
        let gcx = GlobalContext::default();
        let resolver = ImportResolver::new_with_loader(loader, gcx);

        let result = resolver.resolve_all(main_file);
        assert!(result.is_ok());

        let multi_ast = result.unwrap();
        assert_eq!(multi_ast.modules.len(), 3); // main + utils + helpers
    }

    #[test]
    fn test_import_resolver_circular_dependency() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create main.fossil that imports utils
        let main_file = root.join("main.fossil");
        fs::write(&main_file, "open utils").unwrap();

        // Create utils.fossil that imports main (circular!)
        let utils_file = root.join("utils.fossil");
        fs::write(&utils_file, "open main").unwrap();

        let loader = ModuleLoader::new(root.clone());
        let gcx = GlobalContext::default();
        let resolver = ImportResolver::new_with_loader(loader, gcx);

        let result = resolver.resolve_all(main_file);
        assert!(result.is_err()); // Should detect circular dependency
    }

    #[test]
    fn test_import_resolver_topological_order() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create dependency chain: main -> b -> c
        let main_file = root.join("main.fossil");
        fs::write(&main_file, "open b").unwrap();

        fs::write(root.join("b.fossil"), "open c").unwrap();
        fs::write(root.join("c.fossil"), "let value = 42").unwrap();

        let loader = ModuleLoader::new(root.clone());
        let gcx = GlobalContext::default();
        let resolver = ImportResolver::new_with_loader(loader, gcx);

        let result = resolver.resolve_all(main_file);
        assert!(result.is_ok());

        let multi_ast = result.unwrap();

        // c should come before b, b should come before main
        let c_idx = multi_ast
            .order
            .iter()
            .position(|&id| {
                let def = multi_ast.gcx.definitions.get(id);
                multi_ast.gcx.interner.resolve(def.name) == "c"
            })
            .unwrap();

        let b_idx = multi_ast
            .order
            .iter()
            .position(|&id| {
                let def = multi_ast.gcx.definitions.get(id);
                multi_ast.gcx.interner.resolve(def.name) == "b"
            })
            .unwrap();

        assert!(c_idx < b_idx, "c should come before b in topological order");
    }
}
