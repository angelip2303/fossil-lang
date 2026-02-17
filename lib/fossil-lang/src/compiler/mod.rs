use std::fs::read_to_string;
use std::path::PathBuf;

use crate::ast::Loc;
use crate::context::extract_type_metadata;
use crate::error::{FossilError, FossilErrors, FossilWarnings};
use crate::passes;
use crate::passes::resolve::IrResolver;
use crate::passes::{
    GlobalContext, IrProgram, expand::ProviderExpander, parse::Parser, typecheck::TypeChecker,
};

#[derive(Debug, Clone)]
pub enum CompilerInput {
    File(PathBuf),
    Source { name: String, content: String },
}

/// Result of compilation including the program and any warnings
pub struct CompileResult {
    pub program: IrProgram,
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

    pub fn compile(&self, input: CompilerInput) -> Result<CompileResult, FossilErrors> {
        match input {
            CompilerInput::File(path) => {
                let msg = format!("Failed to read file '{}'", path.display());
                let loc = Loc::generated();
                let src =
                    read_to_string(&path).map_err(|_| FossilError::internal("io", msg, loc))?;
                self.compile_source(&src)
            }
            CompilerInput::Source { content, .. } => self.compile_source(&content),
        }
    }

    fn compile_source(&self, src: &str) -> Result<CompileResult, FossilErrors> {
        let gcx = self.gcx.clone().unwrap_or_default();

        let parsed = Parser::parse_with_context(src, self.source_id, gcx)?;
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx)).expand()?;
        let ty = extract_type_metadata(&expand_result.ast);
        let ir = passes::convert::ast_to_ir(expand_result.ast);
        let (ir, gcx, resolutions) = IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()?;
        let program = TypeChecker::new(ir, gcx, resolutions).check()?;

        Ok(CompileResult {
            program,
            warnings: expand_result.warnings,
        })
    }
}
