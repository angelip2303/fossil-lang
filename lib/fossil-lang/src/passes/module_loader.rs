//! Module loader - handles file-based module loading with caching
//!
//! This module provides functionality for loading Fossil modules from the file system,
//! resolving import paths, and caching parsed modules to avoid redundant work.

use std::collections::HashMap;
use std::fs;
use std::path::{Path as StdPath, PathBuf};

use crate::ast::ast::{Ast, Path};
use crate::ast::Loc;
use crate::context::{DefId, DefKind, ModuleInfo, Symbol};
use crate::passes::GlobalContext;
use crate::error::{CompileError, CompileErrorKind};
use crate::passes::parse::Parser;

/// Module loader - maps file system to module tree
///
/// Responsible for:
/// - Resolving import paths to file system paths
/// - Loading and parsing module source files
/// - Caching parsed modules to avoid re-parsing
/// - Creating module DefIds in the GlobalContext
pub struct ModuleLoader {
    /// Root directory of the project
    root_dir: PathBuf,
    /// Cache: file path -> (DefId, source_id)
    /// Tracks which modules have been loaded to avoid re-parsing
    pub(crate) cache: HashMap<PathBuf, (DefId, usize)>,
    /// Next source ID for tracking files
    next_source_id: usize,
    /// Module metadata tracking
    modules: HashMap<DefId, ModuleInfo>,
}

impl ModuleLoader {
    /// Create a new module loader with the given project root directory
    pub fn new(root_dir: PathBuf) -> Self {
        Self {
            root_dir,
            cache: HashMap::new(),
            next_source_id: 0,
            modules: HashMap::new(),
        }
    }

    /// Load a module from the file system
    ///
    /// # Arguments
    /// - `import_path`: The import path from the source code (e.g., `./utils`, `data::csv`)
    /// - `current_file`: The file containing the import statement (for relative resolution)
    /// - `gcx`: GlobalContext for interning symbols and registering definitions
    ///
    /// # Returns
    /// A tuple of (Option<AST>, DefId, source_id)
    /// - AST is Some if this is the first load, None if already cached
    pub fn load_module(
        &mut self,
        import_path: &Path,
        current_file: &StdPath,
        gcx: &mut GlobalContext,
    ) -> Result<(Option<Ast>, DefId, usize), CompileError> {
        // 1. Resolve import path to file system path
        let file_path = self.resolve_import_path(import_path, current_file, &gcx.interner)?;

        // 2. Check cache - if already loaded, return None for AST
        if let Some(&(def_id, source_id)) = self.cache.get(&file_path) {
            return Ok((None, def_id, source_id));
        }

        // 3. Read file
        let src = fs::read_to_string(&file_path).map_err(|e| {
            CompileError::new(
                CompileErrorKind::UndefinedModule(import_path.clone()),
                crate::ast::Loc::generated(),
            )
            .with_context(format!(
                "Failed to read module file '{}': {}",
                file_path.display(),
                e
            ))
        })?;

        // 4. Allocate source ID
        let source_id = self.next_source_id;
        self.next_source_id += 1;

        // 5. Parse module
        let mut parsed = Parser::parse_with_context(&src, source_id, gcx.clone()).map_err(|errors| {
            // TODO(Phase 2.2): Handle all errors once pipeline supports CompileErrors
            let first_error = errors.0.into_iter().next()
                .unwrap_or_else(|| CompileError::new(
                    CompileErrorKind::Parse(Symbol::synthetic()),
                    Loc::generated()
                ));
            first_error.with_context(format!(
                "Error parsing module file '{}'",
                file_path.display()
            ))
        })?;

        // 6. Create module name from file path
        let module_name = self.path_to_module_name(&file_path, &mut parsed.gcx.interner);

        // 7. Create module DefId
        let module_def_id = parsed.gcx.definitions.insert(
            None, // parent will be set later if needed
            module_name,
            DefKind::Mod {
                file_path: Some(file_path.clone()),
                is_inline: false,
            },
        );

        // 8. Update the passed gcx with the parsed one (which has new symbols)
        *gcx = parsed.gcx;

        // 8. Store module metadata
        let module_info = ModuleInfo {
            def_id: module_def_id,
            parent: None, // will be updated when we know the parent
            file_path: file_path.clone(),
            children: Vec::new(),
        };
        self.modules.insert(module_def_id, module_info);

        // 9. Cache and return
        self.cache.insert(file_path, (module_def_id, source_id));

        Ok((Some(parsed.ast), module_def_id, source_id))
    }

    /// Resolve an import path to a file system path
    ///
    /// # Path Resolution Rules:
    /// - `./foo` or `Path::Relative { dots: 0, components: [foo] }` -> `<current_dir>/foo.fossil`
    /// - `../foo` or `Path::Relative { dots: 1, components: [foo] }` -> `<parent_dir>/foo.fossil`
    /// - `foo::bar` or `Path::Qualified([foo, bar])` -> `<root>/foo/bar.fossil`
    /// - `foo` or `Path::Simple(foo)` -> `<root>/foo.fossil`
    fn resolve_import_path(
        &self,
        import: &Path,
        current: &StdPath,
        interner: &crate::context::Interner,
    ) -> Result<PathBuf, CompileError> {
        match import {
            Path::Simple(sym) => {
                // Absolute: search from root
                // foo -> <root>/foo.fossil
                let name = interner.resolve(*sym);
                let mut path = self.root_dir.join(name);
                path.set_extension("fossil");

                if path.exists() {
                    Ok(path.canonicalize().unwrap_or(path))
                } else {
                    Err(CompileError::new(
                        CompileErrorKind::UndefinedModule(import.clone()),
                        crate::ast::Loc::generated(),
                    )
                    .with_context(format!(
                        "Module file not found at '{}'",
                        path.display()
                    )))
                }
            }

            Path::Qualified(parts) => {
                // Absolute: search from root
                // foo::bar::baz -> <root>/foo/bar/baz.fossil
                let mut path = self.root_dir.clone();
                for sym in parts {
                    let name = interner.resolve(*sym);
                    path.push(name);
                }
                path.set_extension("fossil");

                if path.exists() {
                    Ok(path.canonicalize().unwrap_or(path))
                } else {
                    Err(CompileError::new(
                        CompileErrorKind::UndefinedModule(import.clone()),
                        crate::ast::Loc::generated(),
                    )
                    .with_context(format!(
                        "Module file not found at '{}'",
                        path.display()
                    )))
                }
            }

            Path::Relative { dots, components } => {
                // Relative: navigate from current
                // ./foo -> <current_dir>/foo.fossil
                // ../foo -> <parent_dir>/foo.fossil
                // ../../foo -> <grandparent_dir>/foo.fossil

                let mut path = current
                    .parent()
                    .ok_or_else(|| {
                        CompileError::new(
                            CompileErrorKind::UndefinedModule(import.clone()),
                            crate::ast::Loc::generated(),
                        )
                        .with_context("Cannot resolve relative import from root")
                    })?
                    .to_path_buf();

                // Navigate up 'dots' times (dots=0 means current dir already handled)
                for _ in 0..*dots {
                    path = path.parent().ok_or_else(|| {
                        CompileError::new(
                            CompileErrorKind::UndefinedModule(import.clone()),
                            crate::ast::Loc::generated(),
                        )
                        .with_context("Relative import goes above project root")
                    })?.to_path_buf();
                }

                // Navigate down through components
                for sym in components {
                    let name = interner.resolve(*sym);
                    path.push(name);
                }
                path.set_extension("fossil");

                if path.exists() {
                    Ok(path.canonicalize().unwrap_or(path))
                } else {
                    Err(CompileError::new(
                        CompileErrorKind::UndefinedModule(import.clone()),
                        crate::ast::Loc::generated(),
                    )
                    .with_context(format!(
                        "Module file not found at '{}'",
                        path.display()
                    )))
                }
            }
        }
    }

    /// Convert a file path to a module name symbol
    ///
    /// Examples:
    /// - `/project/utils.fossil` -> `utils`
    /// - `/project/data/csv.fossil` -> `csv`
    fn path_to_module_name(&self, path: &StdPath, interner: &mut crate::context::Interner) -> Symbol {
        path.file_stem()
            .and_then(|s| s.to_str())
            .map(|s| interner.intern(s))
            .unwrap_or_else(|| interner.intern("unknown"))
    }

    /// Get module info for a DefId
    pub fn get_module_info(&self, def_id: DefId) -> Option<&ModuleInfo> {
        self.modules.get(&def_id)
    }

    /// Update parent for a module
    pub fn set_module_parent(&mut self, module_id: DefId, parent_id: DefId) {
        if let Some(info) = self.modules.get_mut(&module_id) {
            info.parent = Some(parent_id);
        }
    }

    /// Add a child module
    pub fn add_module_child(&mut self, parent_id: DefId, child_id: DefId) {
        if let Some(info) = self.modules.get_mut(&parent_id) {
            if !info.children.contains(&child_id) {
                info.children.push(child_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_module_loader_simple_path() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create test file
        let test_file = root.join("utils.fossil");
        fs::write(&test_file, "let x = 42").unwrap();

        let mut loader = ModuleLoader::new(root.clone());
        let mut gcx = GlobalContext::default();
        let sym = gcx.interner.intern("utils");
        let path = Path::Simple(sym);

        let result = loader.load_module(&path, &root.join("main.fossil"), &mut gcx);
        assert!(result.is_ok());

        let (maybe_ast, _def_id, _source_id) = result.unwrap();
        assert!(maybe_ast.is_some());
        let ast = maybe_ast.unwrap();
        assert!(!ast.root.is_empty());
    }

    #[test]
    fn test_module_loader_qualified_path() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        // Create nested directory structure
        let data_dir = root.join("data");
        fs::create_dir(&data_dir).unwrap();
        let csv_file = data_dir.join("csv.fossil");
        fs::write(&csv_file, "let parse_csv = fn() -> ()").unwrap();

        let mut loader = ModuleLoader::new(root.clone());
        let mut gcx = GlobalContext::default();

        let data_sym = gcx.interner.intern("data");
        let csv_sym = gcx.interner.intern("csv");
        let path = Path::Qualified(vec![data_sym, csv_sym]);

        let result = loader.load_module(&path, &root.join("main.fossil"), &mut gcx);
        assert!(result.is_ok());
    }

    #[test]
    fn test_module_loader_caching() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().to_path_buf();

        let test_file = root.join("utils.fossil");
        fs::write(&test_file, "let x = 42").unwrap();

        let mut loader = ModuleLoader::new(root.clone());
        let mut gcx = GlobalContext::default();
        let sym = gcx.interner.intern("utils");
        let path = Path::Simple(sym);

        // Load once
        let first = loader.load_module(&path, &root.join("main.fossil"), &mut gcx);
        assert!(first.is_ok());
        let (first_ast, first_def_id, _) = first.unwrap();
        assert!(first_ast.is_some()); // First load should return AST

        // Load again - should hit cache
        let second = loader.load_module(&path, &root.join("main.fossil"), &mut gcx);
        assert!(second.is_ok());
        let (second_ast, second_def_id, _) = second.unwrap();
        assert!(second_ast.is_none()); // Cached load should NOT return AST

        // Both should return the same DefId
        assert_eq!(first_def_id, second_def_id);
    }
}
