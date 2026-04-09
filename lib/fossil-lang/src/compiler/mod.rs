#[cfg(feature = "polars")]
use std::fs::read_to_string;
#[cfg(feature = "polars")]
use std::path::PathBuf;

#[cfg(feature = "polars")]
use crate::ast::Loc;
#[cfg(feature = "polars")]
use crate::context::extract_type_metadata;
#[cfg(feature = "polars")]
use crate::error::{FossilError, FossilErrors, FossilWarnings};
#[cfg(feature = "polars")]
use crate::passes::{GlobalContext, IrProgram, lower, parse::Parser, typecheck::TypeChecker};
#[cfg(feature = "polars")]
use crate::passes::expand::ProviderExpander;

#[cfg(feature = "polars")]
#[derive(Debug, Clone)]
pub enum CompilerInput {
    File(PathBuf),
    Source { name: String, content: String },
}

#[cfg(feature = "polars")]
/// Result of strict compilation (no type errors).
pub struct CompileResult {
    pub program: IrProgram,
    pub warnings: FossilWarnings,
}

#[cfg(feature = "polars")]
/// Result of tolerant compilation — always includes the program (possibly partial).
pub struct TolerantResult {
    pub program: IrProgram,
    pub errors: FossilErrors,
    pub warnings: FossilWarnings,
}

#[cfg(feature = "polars")]
pub struct Compiler {
    source_id: usize,
    gcx: Option<GlobalContext>,
}

#[cfg(feature = "polars")]
impl Default for Compiler {
    fn default() -> Self {
        Self {
            source_id: 0,
            gcx: None,
        }
    }
}

#[cfg(feature = "polars")]
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
    #[cfg(feature = "polars")]
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

    /// Tolerant compilation (requires polars feature for ProviderExpander).
    #[cfg(feature = "polars")]
    pub fn compile_tolerant(&self, input: CompilerInput) -> Result<TolerantResult, FossilErrors> {
        let src = match input {
            CompilerInput::File(path) => {
                let msg = format!("Failed to read file '{}'", path.display());
                let loc = Loc::generated();
                read_to_string(&path).map_err(|_| FossilError::internal("io", msg, loc))?
            }
            CompilerInput::Source { content, .. } => content,
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
