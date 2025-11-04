use crate::{ast::*, error::Result, module::Module, solver::Type};

pub mod csv;
pub mod registry;

/// The TypeProvider trait generates a whole module at compile-time
pub trait TypeProvider: Send + Sync {
    fn param_types(&self) -> Vec<Type>;

    fn provide_type(&self, ast: &Ast, args: &[Arg]) -> Result<Type>;

    fn generate_module(&self, type_name: &str, ast: &Ast, args: &[Arg]) -> Result<Module>;
}
