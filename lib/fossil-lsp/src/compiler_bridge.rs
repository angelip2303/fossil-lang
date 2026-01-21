use fossil_lang::compiler::Compiler;
use fossil_lang::error::CompileError;
use fossil_lang::passes::{GlobalContext, ThirProgram};
use std::sync::Arc;

/// Result of compiling a Fossil document
pub struct CompileResult {
    /// The compiled THIR program (if successful)
    pub thir: Option<Arc<ThirProgram>>,
    /// Compilation errors (if any)
    pub errors: Vec<CompileError>,
    /// The GlobalContext containing the interner for symbol resolution
    pub gcx: GlobalContext,
}

/// Create a GlobalContext with all built-in providers and stdlib registered
fn create_context() -> GlobalContext {
    let mut gcx = GlobalContext::new();

    // Register built-in type providers
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    gcx.register_provider("json", fossil_providers::json::JsonProvider);
    gcx.register_provider("shex", fossil_providers::shex::ShexProvider);
    gcx.register_provider("sql", fossil_providers::sql::SqlProvider);

    // Register stdlib types and functions
    fossil_stdlib::init(&mut gcx);

    gcx
}

/// Compile a Fossil source file and return the result
///
/// Uses the full Fossil compiler pipeline:
/// Parse → ImportResolve → Expand → Resolve → Lower → TypeCheck
///
/// Returns the compiled program, any errors, and the GlobalContext
/// which contains the interner needed for formatting error messages
/// with resolved symbol names.
pub fn compile_document(uri: &str, source: &str) -> CompileResult {
    tracing::info!("compile_document called for: {}", uri);

    // Catch any panics during compilation
    let compilation_result = std::panic::catch_unwind(|| {
        let gcx = create_context();
        let compiler = Compiler::with_context(gcx);
        compiler.compile_with_diagnostics(source)
    });

    match compilation_result {
        Ok(result) => {
            tracing::info!(
                "Compilation finished: program={}, errors={}",
                result.program.is_some(),
                result.errors.len()
            );

            CompileResult {
                thir: result.program.map(Arc::new),
                errors: result.errors,
                gcx: result.gcx,
            }
        }
        Err(panic_info) => {
            tracing::error!("Compilation panicked: {:?}", panic_info);

            // Return an empty result with default context
            let gcx = GlobalContext::default();
            CompileResult {
                thir: None,
                errors: vec![fossil_lang::error::CompileError::new(
                    fossil_lang::error::CompileErrorKind::Runtime(
                        "Internal compiler error (panic)".to_string()
                    ),
                    fossil_lang::ast::Loc::generated(),
                )],
                gcx,
            }
        }
    }
}

