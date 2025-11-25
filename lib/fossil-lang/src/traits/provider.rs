use crate::ast::ast::{Literal, TypeKind};
use crate::context::Interner;
use crate::error::ProviderError;

/// The TypeProvider trait generates a type at compile-time
pub trait TypeProviderImpl: Send + Sync {
    fn generate(
        &self,
        args: &[Literal],
        interner: &mut Interner,
    ) -> Result<TypeKind, ProviderError>;
}
