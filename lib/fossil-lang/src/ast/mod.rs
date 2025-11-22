pub mod ast;
pub mod hir;
pub mod thir;
pub mod visitor;

pub type Span = (usize, usize);
