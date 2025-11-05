use std::sync::Arc;

use indexmap::IndexMap;
use polars::prelude::*;

use crate::ast::NodeId;
use crate::error::{CompileError, Result};
use crate::runtime::RuntimeFunction;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    String(Arc<str>),
    List(ListRepr),
    Record(RecordRepr),
    Function(Function),
}

#[derive(Clone)]
pub enum ListRepr {
    Series(Series),
    DataFrame { lf: LazyFrame, schema: Arc<Schema> },
}

#[derive(Clone)]
pub enum RecordRepr {
    Owned(IndexMap<Arc<str>, Value>),
    Row { df: Arc<DataFrame>, idx: usize },
}

#[derive(Clone)]
pub enum Function {
    Native(Arc<dyn RuntimeFunction>),
    User(UserFunction),
}

#[derive(Clone)]
pub struct UserFunction {
    pub param: NodeId,
    pub body: NodeId,
    pub env: IndexMap<NodeId, Value>,
}

impl Value {
    pub fn from_vec(items: Vec<Value>) -> Result<Self> {
        if items.is_empty() {
            return Err(CompileError::EmptyList); // TODO: is this truly an error?
        }

        let first_type = items[0].discriminant();
        if !items.iter().all(|v| v.discriminant() == first_type) {
            return Err(CompileError::HeterogeneousTypes); // TODO: shouldn't this be checked at compile time?
        }

        match &items[0] {
            Value::Bool(_) => Self::from_bool_vec(&items),
            Value::Int(_) => Self::from_int_vec(&items),
            Value::String(_) => Self::from_string_vec(&items),
            Value::Record(_) => Self::from_record_vec(items),
            _ => Err(CompileError::UnsupportedType),
        }
    }

    fn from_bool_vec(items: &[Value]) -> Result<Self> {
        let values: Vec<bool> = items
            .iter()
            .map(|v| match v {
                Value::Bool(b) => Ok(*b),
                _ => Err(CompileError::HeterogeneousTypes), // TODO: shouldn't this be checked at compile time?
            })
            .collect::<Result<_>>()?;

        Ok(Value::List(ListRepr::Series(Series::from_iter(values))))
    }

    fn from_int_vec(items: &[Value]) -> Result<Self> {
        let values: Vec<i64> = items
            .iter()
            .map(|v| match v {
                Value::Int(i) => Ok(*i),
                _ => Err(CompileError::HeterogeneousTypes), // TODO: shouldn't this be checked at compile time?
            })
            .collect::<Result<_>>()?;

        Ok(Value::List(ListRepr::Series(Series::from_iter(values))))
    }

    fn from_string_vec(items: &[Value]) -> Result<Self> {
        let values: Vec<&str> = items
            .iter()
            .map(|v| match v {
                Value::String(s) => Ok(s.as_ref()),
                _ => Err(CompileError::HeterogeneousTypes), // TODO: shouldn't this be checked at compile time?
            })
            .collect::<Result<_>>()?;

        Ok(Value::List(ListRepr::Series(Series::from_iter(values))))
    }

    fn from_record_vec(items: Vec<Value>) -> Result<Self> {
        // TODO: shouldn't this be checked at compile time?
        let records: Vec<&IndexMap<Arc<str>, Value>> = items
            .iter()
            .map(|v| match v {
                Value::Record(RecordRepr::Owned(map)) => Ok(map),
                _ => Err(CompileError::HeterogeneousTypes),
            })
            .collect::<Result<_>>()?;

        // TODO: shouldn't this be checked at compile time?
        let first_keys: Vec<_> = records[0].keys().cloned().collect();
        for record in &records[1..] {
            let keys: Vec<_> = record.keys().cloned().collect();
            if keys != first_keys {
                return Err(CompileError::RecordSchemaMismatch);
            }
        }

        let mut columns = Vec::new();

        for key in &first_keys {
            let column_values = records
                .iter()
                .map(|r| r.get(key).unwrap().clone().try_into())
                .collect::<Result<Vec<AnyValue<'_>>>>()?;

            let name = PlSmallStr::from_str(key.as_ref());
            let series = Column::new(name, column_values);
            columns.push(series);
        }

        let df = DataFrame::new(columns)?;
        let schema = df.schema().clone();
        let lf = df.lazy();

        Ok(Value::List(ListRepr::DataFrame { lf, schema }))
    }

    fn discriminant(&self) -> std::mem::Discriminant<Self> {
        std::mem::discriminant(self)
    }
}

impl Value {
    pub fn as_lazyframe(&self) -> Result<LazyFrame> {
        match self {
            Value::List(ListRepr::DataFrame { lf, .. }) => Ok(lf.clone()),
            _ => Err(CompileError::ExpectedList(format!("{:?}", self))), // TODO: only List[Record] can be converted to LazyFrame
        }
    }

    pub fn as_series(&self) -> Result<Series> {
        match self {
            Value::List(ListRepr::Series(s)) => Ok(s.clone()),
            _ => Err(CompileError::ExpectedList(format!("{:?}", self))),
        }
    }

    pub fn get_field(&self, field: &str) -> Result<Value> {
        match self {
            Value::Record(RecordRepr::Owned(map)) => map
                .get(field)
                .cloned()
                .ok_or_else(|| CompileError::FieldNotFound(field.to_string())),
            Value::Record(RecordRepr::Row { df, idx }) => {
                let series = df
                    .column(field)
                    .map_err(|_| CompileError::FieldNotFound(field.to_string()))?;
                Ok(series.get(*idx)?.try_into()?)
            }
            _ => Err(CompileError::ExpectedRecord(format!("{:?}", self))),
        }
    }
}

impl<'a> TryFrom<Value> for AnyValue<'a> {
    type Error = CompileError;

    fn try_from(value: Value) -> std::result::Result<AnyValue<'a>, Self::Error> {
        match value {
            Value::Unit => Ok(AnyValue::Null), // TODO: may we should reconsider this
            Value::Bool(b) => Ok(AnyValue::Boolean(b)),
            Value::Int(i) => Ok(AnyValue::Int64(i)),
            Value::String(s) => Ok(AnyValue::StringOwned(PlSmallStr::from_str(&s))),
            Value::List(_) => Err(CompileError::UnsupportedType),
            Value::Record(_) => Err(CompileError::UnsupportedType),
            Value::Function(_) => Err(CompileError::UnsupportedType),
        }
    }
}

impl<'a> TryFrom<AnyValue<'a>> for Value {
    type Error = CompileError;

    fn try_from(value: AnyValue<'a>) -> std::result::Result<Value, Self::Error> {
        match value {
            AnyValue::Null => Ok(Value::Unit),
            AnyValue::Boolean(b) => Ok(Value::Bool(b)),
            AnyValue::Int8(i) => Ok(Value::Int(i as i64)),
            AnyValue::Int16(i) => Ok(Value::Int(i as i64)),
            AnyValue::Int32(i) => Ok(Value::Int(i as i64)),
            AnyValue::Int64(i) => Ok(Value::Int(i)),
            AnyValue::UInt8(u) => Ok(Value::Int(u as i64)),
            AnyValue::UInt16(u) => Ok(Value::Int(u as i64)),
            AnyValue::UInt32(u) => Ok(Value::Int(u as i64)),
            AnyValue::UInt64(u) => Ok(Value::Int(u as i64)),
            AnyValue::StringOwned(s) => Ok(Value::String(Arc::from(s.as_str()))),
            AnyValue::String(s) => Ok(Value::String(Arc::from(s))),
            _ => Err(CompileError::UnsupportedType),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(repr) => match repr {
                ListRepr::Series(s) => {
                    write!(f, "List::Series(len={}, dtype={})", s.len(), s.dtype())
                }
                ListRepr::DataFrame { schema, .. } => {
                    let cols: Vec<_> = schema.iter_names().collect();
                    write!(f, "List::DataFrame(cols={:?})", cols)
                }
            },
            Value::Record(repr) => match repr {
                RecordRepr::Owned(m) => {
                    let keys: Vec<_> = m.keys().map(|k| k.as_ref()).collect();
                    write!(f, "Record::Owned({:?})", keys)
                }
                RecordRepr::Row { idx, .. } => {
                    write!(f, "Record::Row(idx={})", idx)
                }
            },
            Value::Function(_) => write!(f, "<function>"),
        }
    }
}
