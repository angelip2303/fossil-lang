use thiserror::Error;

#[derive(Error, Debug)]
pub enum TestSuiteError {
    #[error("The actual output does not match the expected output")]
    NotEquals,

    #[error("Failed to compile program")]
    Compilation,

    #[error("Failed to read file")]
    IO(#[from] std::io::Error),

    #[error("Failed to parse RDF")]
    RDF(#[from] oxrdfio::RdfParseError),
}

impl From<fossil_lang::error::CompileError> for TestSuiteError {
    fn from(_: fossil_lang::error::CompileError) -> Self {
        TestSuiteError::Compilation
    }
}
