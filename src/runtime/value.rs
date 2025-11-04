use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

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
    Function(Arc<dyn RuntimeFunction>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", item)?;
                }
                write!(f, "]")
            }
            Value::Record(map) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for (key, value) in map {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", key, value)?;
                    first = false;
                }
                write!(f, " }}")
            }
            Value::Function(func) => write!(f, "<function: {}>", func.name()),
        }
    }
}
