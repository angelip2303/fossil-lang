use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::context::GlobalContext;
use fossil_lang::passes::IrProgram;

use crate::completions::{self, CompletionItem};
use crate::cursor::CursorContext;
use crate::diagnostics::{self, DiagnosticItem};

/// Result of an analysis pass — always has diagnostics, sometimes updates the snapshot.
pub struct AnalysisResult {
    pub diagnostics: Vec<DiagnosticItem>,
}

/// Immutable snapshot of a successful compilation.
pub struct AnalysisSnapshot {
    pub program: IrProgram,
}

/// Long-lived analysis host. Holds the last successful compilation.
/// One instance per org/session — survives across requests.
pub struct AnalysisHost {
    snapshot: Option<AnalysisSnapshot>,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self { snapshot: None }
    }

    /// Analyze the source. Returns diagnostics always; updates internal
    /// snapshot on success. Completions use the latest snapshot.
    pub fn analyze(&mut self, source: &str, gcx: GlobalContext) -> AnalysisResult {
        let compiler = Compiler::with_context(gcx);
        let compile_result = compiler.compile(CompilerInput::Source {
            name: "editor".to_string(),
            content: source.to_string(),
        });

        match compile_result {
            Ok(result) => {
                let diagnostics = diagnostics::map_warnings(&result.warnings.0);

                self.snapshot = Some(AnalysisSnapshot {
                    program: result.program,
                });

                AnalysisResult { diagnostics }
            }
            Err(errors) => {
                let diagnostics = diagnostics::map_errors(&errors.0);
                AnalysisResult { diagnostics }
            }
        }
    }

    /// Get completions at offset. Uses cursor context analysis + latest snapshot.
    pub fn completions(&self, source: &str, offset: usize) -> Vec<CompletionItem> {
        let ctx = CursorContext::resolve(source, offset);
        completions::resolve_completions(&ctx, self.snapshot.as_ref())
    }
}

impl Default for AnalysisHost {
    fn default() -> Self {
        Self::new()
    }
}
