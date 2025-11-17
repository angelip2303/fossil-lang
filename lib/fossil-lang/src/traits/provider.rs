use crate::{
    ast::{Ast, Literal, Type},
    context::Interner,
    error::ProviderError,
};

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProviderImpl: Send + Sync {
    fn generate(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        interner: &mut Interner,
    ) -> Result<Type, ProviderError>;
}
