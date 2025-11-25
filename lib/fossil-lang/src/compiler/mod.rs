//! Compiler orchestration module
//!
//! This module coordinates the compilation pipeline:
//! Source -> Parse -> AST -> Lower -> HIR -> TypeCheck -> THIR -> Interpret/Execute

use crate::error::CompileError;
use crate::passes::{
    HirProgram, ParsedProgram, ThirProgram, lower::Lowering, parse::Parser, typecheck::TypeChecker,
};

pub struct Compiler {
    /// The source ID for error reporting
    source_id: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self { source_id: 0 }
    }

    /// Compile source code through the full pipeline
    pub fn compile(&self, src: &str) -> Result<ThirProgram, CompileError> {
        // Phase 1: Parse source -> AST
        let parsed = Parser::parse(src, self.source_id)?;

        // Phase 2: Lower AST -> HIR (resolve names, desugar syntax)
        let hir = self.lower(parsed)?;

        // Phase 3: TypeCheck HIR -> THIR (type inference and checking)
        let thir = self.typecheck(hir)?;

        Ok(thir)
    }

    /// Lower AST to HIR
    fn lower(&self, parsed: ParsedProgram) -> Result<HirProgram, CompileError> {
        Lowering::lower(parsed)
    }

    /// Type check HIR to THIR
    // fn typecheck(&self, hir: HirProgram) -> Result<ThirProgram, CompileError> {
    //     let checker = TypeChecker::new();
    //     checker.check(hir)
    // }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
