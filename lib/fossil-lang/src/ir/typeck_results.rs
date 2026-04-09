use std::collections::HashMap;

use crate::context::DefId;
use crate::ir::{ExprId, TypeId};

#[derive(Default, Clone, PartialEq)]
pub struct TypeckResults {
    pub expr_types: HashMap<ExprId, TypeId>,
    pub binding_types: HashMap<DefId, TypeId>,
}
