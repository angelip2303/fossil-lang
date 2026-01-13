//! Compiler orchestration module
//!
//! This module coordinates the compilation pipeline:
//! Source -> Parse -> ImportResolve -> Resolve -> Expand -> Lower -> TypeCheck

use std::path::PathBuf;

use crate::error::CompileError;
use crate::passes::{
    ThirProgram,
    ImportResolver,
    ModuleLoader,
    parse::Parser,
    resolve::NameResolver,
    expand::ProviderExpander,
    lower::HirLowering,
    typecheck::TypeChecker,
};

/// Compiler input options
///
/// Supports three compilation modes:
/// - Single file compilation
/// - Project directory compilation (looks for main.fossil)
/// - String compilation (for REPL/tests)
#[derive(Debug, Clone)]
pub enum CompilerInput {
    /// Compile a single file as entry point
    File(PathBuf),
    /// Compile a project directory (searches for main.fossil)
    Directory(PathBuf),
    /// Compile source string (REPL/testing mode)
    String { src: String, name: String },
}

pub struct Compiler {
    /// The source ID for error reporting
    source_id: usize,
    /// Optional custom GlobalContext (for provider registration, etc.)
    gcx: Option<crate::passes::GlobalContext>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            source_id: 0,
            gcx: None,
        }
    }

    /// Create a compiler with a custom GlobalContext
    ///
    /// This allows registering type providers (F# style):
    /// ```ignore
    /// use fossil_lang::compiler::Compiler;
    /// use fossil_lang::passes::GlobalContext;
    /// use std::sync::Arc;
    ///
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_provider("csv", Arc::new(CsvProvider));
    ///
    /// let compiler = Compiler::with_context(gcx);
    /// compiler.compile(src)?;
    /// ```
    pub fn with_context(gcx: crate::passes::GlobalContext) -> Self {
        Self {
            source_id: 0,
            gcx: Some(gcx),
        }
    }

    /// Compile input through the full pipeline
    ///
    /// Supports three compilation modes via CompilerInput:
    /// - File: Compile a single .fossil file (can have imports)
    /// - Directory: Compile project (searches for main.fossil as entry)
    /// - String: Compile source string (REPL/testing, single module)
    ///
    /// Pipeline: Parse → ImportResolve → Expand → Resolve → Lower → TypeCheck
    pub fn compile(&self, input: CompilerInput) -> Result<ThirProgram, CompileError> {
        match input {
            CompilerInput::File(path) => {
                // Compile file as project with parent directory as root
                let root_dir = path
                    .parent()
                    .map(|p| p.to_path_buf())
                    .unwrap_or_else(|| std::env::current_dir().expect("Failed to get current directory"));
                self.compile_project(root_dir, path)
            }
            CompilerInput::Directory(root_dir) => {
                // Search for main.fossil as entry point
                let entry_point = root_dir.join("main.fossil");
                if !entry_point.exists() {
                    return Err(CompileError::new(
                        crate::error::CompileErrorKind::Runtime(
                            "No main.fossil found in project directory".into()
                        ),
                        crate::ast::Loc::generated(),
                    ));
                }
                self.compile_project(root_dir, entry_point)
            }
            CompilerInput::String { src, name: _ } => {
                // REPL/testing mode: single module without file imports
                self.compile_string(&src)
            }
        }
    }

    /// Compile a multi-file project
    ///
    /// Uses ImportResolverWithLoader to recursively load all imports,
    /// detect circular dependencies, and compile in topological order.
    ///
    /// NOTE: Current implementation processes modules individually in topological order.
    /// Future enhancement: Process all modules together with cross-module type checking.
    fn compile_project(
        &self,
        root_dir: PathBuf,
        entry_point: PathBuf,
    ) -> Result<ThirProgram, CompileError> {
        // Phase 1: Multi-module import resolution with file loading
        let loader = ModuleLoader::new(root_dir);
        let gcx = self.gcx.clone().unwrap_or_default();
        let resolver = ImportResolver::new_with_loader(loader, gcx);

        let mut multi_ast = resolver.resolve_all(entry_point)?;

        // For now, we process each module individually in topological order
        // TODO: Extend pipeline to compile all modules together with cross-module resolution
        //
        // Current strategy: Compile only the entry module (last in topological order)
        // This works because imports are already validated by ImportResolverWithLoader
        let entry_module_id = *multi_ast.order.last()
            .ok_or_else(|| CompileError::new(
                crate::error::CompileErrorKind::Runtime("No modules to compile".into()),
                crate::ast::Loc::generated(),
            ))?;

        let entry_ast = multi_ast.modules.remove(&entry_module_id)
            .ok_or_else(|| CompileError::new(
                crate::error::CompileErrorKind::Runtime("Entry module not found".into()),
                crate::ast::Loc::generated(),
            ))?;

        // Phase 2: Expand type providers
        let expanded = ProviderExpander::new((entry_ast, multi_ast.gcx)).expand()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Provider expansion failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 3: Name Resolution
        let mut name_resolver = NameResolver::new(expanded.0, expanded.1);
        name_resolver.set_module_context(entry_module_id);
        let resolved = name_resolver.resolve()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Name resolution failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 4: Lower AST -> HIR
        let hir = HirLowering::new(resolved).lower()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Lowering failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 5: Type Check HIR -> THIR
        let thir = TypeChecker::new(hir).check()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Type checking failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        Ok(thir)
    }

    /// Compile source string (REPL/testing mode)
    ///
    /// Simplified pipeline for single-module compilation without file imports.
    /// Uses the original ImportResolver for in-memory validation.
    fn compile_string(&self, src: &str) -> Result<ThirProgram, CompileError> {
        // Phase 1: Parse source -> AST
        let parsed = if let Some(ref custom_gcx) = self.gcx {
            Parser::parse_with_context(src, self.source_id, custom_gcx.clone())
                .map_err(|errors| {
                    // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                    errors.0.into_iter().next()
                        .unwrap_or_else(|| CompileError::new(
                            crate::error::CompileErrorKind::Parse(crate::context::Symbol::synthetic()),
                            crate::ast::Loc::generated()
                        ))
                })?
        } else {
            Parser::parse(src, self.source_id)
                .map_err(|errors| {
                    // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                    errors.0.into_iter().next()
                        .unwrap_or_else(|| CompileError::new(
                            crate::error::CompileErrorKind::Parse(crate::context::Symbol::synthetic()),
                            crate::ast::Loc::generated()
                        ))
                })?
        };

        // Phase 2: Import Resolution - validate imports exist (in-memory only)
        let (ast, gcx) = ImportResolver::new(parsed.ast, parsed.gcx).resolve()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Import resolution failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 3: Expand type providers
        let expanded = ProviderExpander::new((ast, gcx)).expand()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Provider expansion failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 4: Name Resolution
        let resolved = NameResolver::new(expanded.0, expanded.1).resolve()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Name resolution failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 5: Lower AST -> HIR
        let hir = HirLowering::new(resolved).lower()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Lowering failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        // Phase 6: Type Check HIR -> THIR
        let thir = TypeChecker::new(hir).check()
            .map_err(|errors| {
                // TODO(Phase 2.2): Return all errors once pipeline supports CompileErrors
                errors.0.into_iter().next()
                    .unwrap_or_else(|| CompileError::new(
                        crate::error::CompileErrorKind::Runtime("Type checking failed".to_string()),
                        crate::ast::Loc::generated()
                    ))
            })?;

        Ok(thir)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
