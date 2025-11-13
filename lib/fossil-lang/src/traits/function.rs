use crate::ast::Polytype;

pub trait FunctionImpl: Send + Sync {
    fn signature(&self) -> Polytype;
}
