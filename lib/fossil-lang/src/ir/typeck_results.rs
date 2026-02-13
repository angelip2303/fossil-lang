use std::collections::HashMap;

use crate::ir::{ExprId, TypeId};

#[derive(Default)]
pub struct TypeckResults {
    pub expr_types: HashMap<ExprId, TypeId>,
}
