use crate::{
    ast::{Literal, Type},
    error::ProviderError,
};

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProviderImpl: Send + Sync {
    fn generate(&self, args: &[Literal]) -> Result<Type, ProviderError>;
}
