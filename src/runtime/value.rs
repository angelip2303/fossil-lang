use std::collections::HashMap;
use std::sync::Arc;

use polars::prelude::LazyFrame;

use crate::runtime::RuntimeFunction;

#[derive(Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    List(Vec<Value>),
    Record(HashMap<String, Value>),
    DataFrame(LazyFrame), // TODO: a dataframe is a List of Records
    Function(Arc<dyn RuntimeFunction>),
}
