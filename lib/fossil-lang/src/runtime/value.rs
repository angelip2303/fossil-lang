use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::ast::ExprId;
use crate::context::Symbol;
use crate::module::BindingId;

#[derive(Clone)]
pub enum Value {
    // internal values
    Int(i64),
    String(Arc<str>),
    Bool(bool),
    Series(Series),
    LazyFrame(LazyFrame),
    Unit,

    // functions as values allow, for example, them to be passed
    // as parameters to other functions (Higher-order functions)
    Closure {
        params: Vec<Symbol>,
        body: ExprId,
        env: Rc<Environment>,
    },

    Function(BindingId),
}

/// Environment for variable bindings during execution
#[derive(Clone, Default)]
pub struct Environment {
    bindings: HashMap<Symbol, Value>,
}

impl Environment {
    pub fn bind(&mut self, name: Symbol, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn lookup(&self, name: Symbol) -> Option<&Value> {
        self.bindings.get(&name)
    }
}
