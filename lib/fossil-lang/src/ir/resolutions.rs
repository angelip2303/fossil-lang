use std::collections::HashMap;

use crate::context::DefId;
use crate::ir::{ExprId, StmtId, TypeId};

#[derive(Default)]
pub struct Resolutions {
    pub expr_defs: HashMap<ExprId, DefId>,
    pub stmt_defs: HashMap<StmtId, DefId>,
    pub type_defs: HashMap<TypeId, DefId>,
    pub expr_rewrites: HashMap<ExprId, ExprId>,
}
