use std::fs::read_to_string;
use std::path::PathBuf;

use crate::ast::Loc;
use crate::context::extract_type_metadata;
use crate::error::{FossilError, FossilErrors, FossilWarnings};
use crate::passes::{
    GlobalContext, IrProgram, expand::ProviderExpander, lower, parse::Parser,
    typecheck::TypeChecker,
};

#[derive(Debug, Clone)]
pub enum CompilerInput {
    File(PathBuf),
    Source { name: String, content: String },
}

/// Result of strict compilation (no type errors).
pub struct CompileResult {
    pub program: IrProgram,
    pub warnings: FossilWarnings,
}

/// Result of tolerant compilation — always includes the program (possibly partial).
pub struct TolerantResult {
    pub program: IrProgram,
    pub errors: FossilErrors,
    pub warnings: FossilWarnings,
}

pub struct Compiler {
    source_id: usize,
    gcx: Option<GlobalContext>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            source_id: 0,
            gcx: None,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_context(gcx: GlobalContext) -> Self {
        Self {
            source_id: 0,
            gcx: Some(gcx),
        }
    }

    /// Strict compilation — fails on any error (parse, lower, or type-check).
    pub fn compile(&self, input: CompilerInput) -> Result<CompileResult, FossilErrors> {
        let result = self.compile_tolerant(input)?;
        if result.errors.is_empty() {
            Ok(CompileResult {
                program: result.program,
                warnings: result.warnings,
            })
        } else {
            Err(result.errors)
        }
    }

    /// Tolerant compilation: parse/expand/lower errors are still fatal,
    /// but type-check errors produce partial results (for LSP completions).
    pub fn compile_tolerant(&self, input: CompilerInput) -> Result<TolerantResult, FossilErrors> {
        let src = match &input {
            CompilerInput::File(path) => {
                let msg = format!("Failed to read file '{}'", path.display());
                let loc = Loc::generated();
                read_to_string(path).map_err(|_| FossilError::internal("io", msg, loc))?
            }
            CompilerInput::Source { content, .. } => content.clone(),
        };
        let gcx = self.gcx.clone().unwrap_or_default();

        let parsed = Parser::parse_with_context(&src, self.source_id, gcx)?;
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx)).expand()?;
        let ty = extract_type_metadata(&expand_result.ast);
        let (ir, gcx, resolutions) =
            lower::lower_with_metadata(expand_result.ast, expand_result.gcx, ty)?;
        let (program, errors) = TypeChecker::new(ir, gcx, resolutions).check_tolerant();

        Ok(TolerantResult {
            program,
            errors,
            warnings: expand_result.warnings,
        })
    }
}
