//! Compiler orchestration module
//!
//! This module coordinates the compilation pipeline:
//! Source -> Parse -> Expand -> Convert to IR -> Resolve -> TypeCheck
//!
//! The unified IR-based pipeline:
//! 1. Parse source code into AST
//! 2. Expand type providers (still on AST)
//! 3. Convert AST to IR
//! 4. Resolve names in IR
//! 5. Type check IR

use std::path::PathBuf;

use crate::context::extract_pending_type_metadata;
use crate::error::CompileError;
use crate::ir;
use crate::passes::{
    expand::ProviderExpander, parse::Parser, typecheck::TypeChecker, GlobalContext, IrProgram,
};

/// Result of compilation with access to the GlobalContext
///
/// This struct provides both the compilation result (success or errors)
/// and access to the GlobalContext, which contains the interner needed
/// for formatting error messages with resolved symbol names.
pub struct CompilationResult {
    /// The compiled program (if compilation succeeded)
    pub program: Option<IrProgram>,
    /// Compilation errors (empty if successful)
    pub errors: Vec<CompileError>,
    /// The GlobalContext containing the interner for symbol resolution
    pub gcx: GlobalContext,
}

impl CompilationResult {
    /// Check if compilation was successful
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty() && self.program.is_some()
    }

    /// Check if compilation failed
    pub fn is_err(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// Compiler input options
#[derive(Debug, Clone)]
pub enum CompilerInput {
    /// Compile a single file as entry point
    File(PathBuf),
}

pub struct Compiler {
    /// The source ID for error reporting
    source_id: usize,
    /// Optional custom GlobalContext (for provider registration, etc.)
    gcx: Option<GlobalContext>,
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
    /// This allows registering type providers:
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
    pub fn with_context(gcx: GlobalContext) -> Self {
        Self {
            source_id: 0,
            gcx: Some(gcx),
        }
    }

    /// Compile input through the full pipeline
    ///
    /// Pipeline: Parse → Expand → Convert to IR → Resolve → TypeCheck
    pub fn compile(&self, input: CompilerInput) -> Result<IrProgram, CompileError> {
        match input {
            CompilerInput::File(path) => self.compile_file(path),
        }
    }

    /// Compile a single file
    ///
    /// Pipeline: Parse → Expand → Convert to IR → Resolve → TypeCheck
    fn compile_file(&self, path: PathBuf) -> Result<IrProgram, CompileError> {
        // Read source file
        let src = std::fs::read_to_string(&path).map_err(|e| {
            CompileError::new(
                crate::error::CompileErrorKind::Runtime(format!(
                    "Failed to read file '{}': {}",
                    path.display(),
                    e
                )),
                crate::ast::Loc::generated(),
            )
        })?;

        // Get or create GlobalContext
        let gcx = self.gcx.clone().unwrap_or_default();

        // Phase 1: Parse source → AST
        let parsed = Parser::parse_with_context(&src, self.source_id, gcx).map_err(|errors| {
            errors.0.into_iter().next().unwrap_or_else(|| {
                CompileError::new(
                    crate::error::CompileErrorKind::Runtime("Parse failed".to_string()),
                    crate::ast::Loc::generated(),
                )
            })
        })?;

        // Phase 2: Expand type providers (still on AST)
        let (expanded_ast, mut gcx) = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .map_err(|errors| {
                errors.0.into_iter().next().unwrap_or_else(|| {
                    CompileError::new(
                        crate::error::CompileErrorKind::Runtime(
                            "Provider expansion failed".to_string(),
                        ),
                        crate::ast::Loc::generated(),
                    )
                })
            })?;

        // Phase 2.5: Extract type metadata from AST before conversion
        // This captures field attributes (like #[rdf(uri = "...")]) from record types
        // and stores them by type name. The IrResolver will transfer to DefId keys later.
        extract_pending_type_metadata(&expanded_ast, &mut gcx);

        // Phase 3: Convert AST → IR
        let ir = ir::ast_to_ir(expanded_ast);

        // Phase 4: Resolve names in IR
        let (ir, gcx) =
            ir::IrResolver::new(ir, gcx)
                .resolve()
                .map_err(|errors| {
                    errors.0.into_iter().next().unwrap_or_else(|| {
                        CompileError::new(
                            crate::error::CompileErrorKind::Runtime(
                                "Name resolution failed".to_string(),
                            ),
                            crate::ast::Loc::generated(),
                        )
                    })
                })?;

        // Phase 5: Type check IR
        let program = TypeChecker::new(ir, gcx).check().map_err(|errors| {
            errors.0.into_iter().next().unwrap_or_else(|| {
                CompileError::new(
                    crate::error::CompileErrorKind::Runtime("Type checking failed".to_string()),
                    crate::ast::Loc::generated(),
                )
            })
        })?;

        Ok(program)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
