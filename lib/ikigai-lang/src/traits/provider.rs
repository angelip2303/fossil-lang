use thiserror::Error;

use crate::ast::Literal;
use crate::generated::Type;

type Result<T> = std::result::Result<T, ProviderError>;

#[derive(Error, Debug, Clone)]
pub enum ProviderError {
    #[error("Invalid argument count")]
    InvalidArgumentCount,
    #[error("Invalid argument type")]
    InvalidArgumentType,
}

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProviderImpl: Send + Sync {
    fn generate(&self, args: &[Literal]) -> Result<Type>;
}
