use crate::functions::Value;
use crate::{ast::*, error::Error, solver::Type};

pub mod csv;
pub mod registry;

pub trait TypeProvider {
    fn provide(&self, ast: &Ast, args: &[Arg]) -> Result<Type, Error>;
}

pub trait Read {
    type Output;
    fn read(&self, args: &[Value]) -> Result<Self::Output, Error>;
}

pub trait Write {
    type Input;
    fn write(&self, input: Self::Input, args: &[Value]) -> Result<(), Error>;
}
