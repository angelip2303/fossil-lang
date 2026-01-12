//! Compiler orchestration module
//!
//! This module coordinates the compilation pipeline:
//! Source -> Parse -> ImportResolve -> Resolve -> Expand -> Lower -> TypeCheck

use crate::error::CompileError;
use crate::passes::{
    ThirProgram,
    ImportResolver,
    parse::Parser,
    resolve::NameResolver,
    expand::ProviderExpander,
    lower::HirLowering,
    typecheck::TypeChecker,
};

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

    /// Compile source code through the full pipeline
    ///
    /// Pipeline: Parse → ImportResolve → Expand → Resolve → Lower → TypeCheck
    pub fn compile(&self, src: &str) -> Result<ThirProgram, CompileError> {
        // Phase 1: Parse source -> AST
        // If custom GlobalContext was provided, use it for parsing (ensures same interner)
        let parsed = if let Some(ref custom_gcx) = self.gcx {
            Parser::parse_with_context(src, self.source_id, custom_gcx.clone())?
        } else {
            Parser::parse(src, self.source_id)?
        };

        // Phase 2: Import Resolution - validate imports exist
        let (ast, gcx) = ImportResolver::new(parsed.ast, parsed.gcx).resolve()?;

        // Phase 3: Expand type providers (F# style: execute providers, generate types + modules)
        // IMPORTANT: Must happen BEFORE name resolution so generated types/modules are available
        let expanded = ProviderExpander::new((ast, gcx)).expand()?;

        // Phase 4: Name Resolution (2-pass: collect declarations, then resolve)
        // Now can resolve names in provider-generated modules
        let resolved = NameResolver::new(expanded.0, expanded.1).resolve()?;

        // Phase 5: Lower AST -> HIR (desugar pipe, resolve DefIds)
        let hir = HirLowering::new(resolved).lower()?;

        // Phase 6: Type Check HIR -> THIR (Algorithm W inference)
        let thir = TypeChecker::new(hir).check()?;

        Ok(thir)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
