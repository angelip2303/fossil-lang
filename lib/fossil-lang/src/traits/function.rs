use crate::typechecker::Polytype;

pub trait FunctionImpl: Send + Sync {
    fn signature(&self) -> Polytype;
}
