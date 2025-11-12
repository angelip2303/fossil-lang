use std::collections::HashMap;

use crate::context::*;
use crate::generated::{ConcreteAst, ExprId};

pub type TypeId = NodeId<Type>;

/// AST representation after type checking (with inferred types)
#[derive(Debug)]
pub struct CheckedAst {
    pub ast: ConcreteAst,
    pub expr_types: HashMap<ExprId, Type>,
}

/// Type variable for polymorphism
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

/// Types in the type checker (includes type variables for inference)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    String,
    Bool,
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
    Var(TypeVar),
}

pub type TypeArena = Arena<Type>;

/// Polytypes (type schemes) for polymorphic functions
#[derive(Clone, Debug)]
pub struct Polytype {
    pub vars: Vec<TypeVar>,
    pub ty: TypeId,
}

impl Polytype {
    pub fn mono(ty: TypeId) -> Self {
        Polytype { vars: vec![], ty }
    }

    pub fn new(vars: Vec<TypeVar>, ty: TypeId) -> Self {
        Polytype { vars, ty }
    }
}
