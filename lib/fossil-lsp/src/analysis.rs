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
    /// Hash of the last source passed to `analyze()`, used to skip redundant recompilation
    /// when multiple requests (linter + completion) arrive with the same source.
    last_source_hash: Option<u64>,
    last_diagnostics: Vec<DiagnosticItem>,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self {
            snapshot: None,
            last_source_hash: None,
            last_diagnostics: Vec::new(),
        }
    }

    /// Analyze the source. Returns diagnostics always; updates the snapshot
    /// with partial type info even when there are type errors (for completions).
    ///
    /// Skips recompilation when the source hasn't changed since the last call,
    /// returning cached diagnostics and preserving the existing snapshot.
    /// Callers may pass a pre-computed `source_hash` to avoid double-hashing.
    pub fn analyze(&mut self, source: &str, gcx: GlobalContext, source_hash: Option<u64>) -> AnalysisResult {
        let hash = source_hash.unwrap_or_else(|| {
            use std::hash::{Hash, Hasher};
            let mut h = std::collections::hash_map::DefaultHasher::new();
            source.hash(&mut h);
            h.finish()
        });

        if self.last_source_hash == Some(hash) {
            return AnalysisResult {
                diagnostics: self.last_diagnostics.clone(),
            };
        }

        let compiler = Compiler::with_context(gcx);
        let compile_result = compiler.compile_tolerant(CompilerInput::Source {
            name: "editor".to_string(),
            content: source.to_string(),
        });

        let diagnostics = match compile_result {
            Ok(result) => {
                let mut diagnostics = diagnostics::map_warnings(&result.warnings.0);
                diagnostics.extend(diagnostics::map_errors(&result.errors.0));

                self.snapshot = Some(AnalysisSnapshot {
                    program: result.program,
                });

                diagnostics
            }
            // Parse/expand/lower failed — too broken for partial results
            Err(errors) => diagnostics::map_errors(&errors.0),
        };

        self.last_source_hash = Some(hash);
        self.last_diagnostics = diagnostics.clone();
        AnalysisResult { diagnostics }
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
