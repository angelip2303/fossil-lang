use thiserror::Error;

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Expected {} arguments, got {}", expected, actual)]
    InvalidArgumentCount { expected: usize, actual: usize },
}
