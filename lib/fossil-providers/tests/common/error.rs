use thiserror::Error;

#[derive(Error, Debug)]
pub enum TestSuiteError {
    #[error("The actual output does not match the expected output")]
    NotEquals,

    #[error("Failed to compile program")]
    MultipleCompilationFailures(#[from] fossil_lang::error::CompileErrors),

    #[error("Failed to compile program")]
    SingleCompilationFailure(#[from] fossil_lang::error::CompileError),

    #[error("Runtime evaluation failed")]
    Evaluation(#[from] fossil_lang::error::EvalError),

    #[error("Failed to read file")]
    IO(#[from] std::io::Error),

    #[error("Failed to parse RDF")]
    RDF(#[from] oxrdfio::RdfParseError),
}
