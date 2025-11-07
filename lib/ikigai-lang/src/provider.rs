use crate::{ast::*, error::Result, module::Module, solver::Type};

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProvider: Send + Sync {
    fn param_types(&self) -> Vec<Type>; // TODO: I don't know if this is fine
    fn provide_type(&self, ast: &Ast, args: &[Arg]) -> Result<Type>; // TODO: I don't know if we should have several args as param, or just one
    fn generate_module(&self, name: &str, ty: Type) -> Result<Module>;
}
