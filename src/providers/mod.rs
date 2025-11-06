use crate::{ast::*, error::Result, module::Module, solver::Type};

pub mod csv;

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProvider: Send + Sync {
    fn param_types(&self) -> Vec<Type>;

    fn provide_type(&self, ast: &Ast, args: &[Arg]) -> Result<Type>;

    fn generate_module(&self, name: &str, ty: Type) -> Result<Module>;
}
