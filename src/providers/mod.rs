use crate::{ast::*, error::CompileError, solver::Type};
use polars::prelude::LazyFrame;

pub mod csv;
pub mod registry;

pub trait TypeProvider: Send + Sync {
    /// Tipos de parámetros esperados
    fn param_types(&self) -> Vec<Type>;

    /// Genera el tipo en compile-time
    fn provide(&self, ast: &Ast, args: &[Arg]) -> Result<Type, CompileError>;

    /// Lee datos en runtime (para .load)
    fn load(&self, path: &str) -> Result<LazyFrame, CompileError>;
}
