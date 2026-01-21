use std::any::Any;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::ast::thir::{ExprId, Param};
use crate::context::{DefId, Symbol};

// TODO: Fix BindingId when module system is implemented
pub type BindingId = u32;

/// Identificador único para tipos de extensión
///
/// Permite que stdlib y otros crates definan tipos custom sin modificar
/// el enum Value directamente. Cada tipo de extensión (Entity, Future, Option, etc.)
/// obtiene un ExtensionTypeId único.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExtensionTypeId(pub u64);

/// Trait para metadata específica de cada tipo de extensión
///
/// Permite que cada extensión almacene metadata arbitraria de compile-time
/// con el valor en runtime. El metadata puede ser downcasted usando as_any().
///
/// # Example
/// ```ignore
/// struct EntityMetadata {
///     id: Arc<str>,
///     type_def_id: DefId,
/// }
///
/// impl ExtensionMetadata for EntityMetadata {
///     fn type_name(&self) -> &str { "Entity" }
///     fn as_any(&self) -> &dyn Any { self }
/// }
/// ```
pub trait ExtensionMetadata: Send + Sync {
    /// Nombre del tipo de extensión (para debugging y error messages)
    fn type_name(&self) -> &str;

    /// Permite downcast a tipo concreto
    fn as_any(&self) -> &dyn Any;
}

#[derive(Clone)]
pub enum Value {
    // Primitive values
    Int(i64),
    String(Arc<str>),
    Bool(bool),
    Unit,

    // Collection values (backed by Polars internally)
    /// A single column of data
    Column(Series),
    /// A collection of records (rows with named fields)
    Records(LazyFrame),
    /// A single record (efficient row access into shared data)
    Record(Arc<DataFrame>, usize),

    // functions as values allow, for example, them to be passed
    // as parameters to other functions (Higher-order functions)
    Closure {
        params: Vec<Param>,
        body: ExprId,
        env: Rc<Environment>,
    },

    Function(BindingId),

    /// Builtin function from stdlib
    /// Contains DefId for identification and Arc to FunctionImpl for execution
    BuiltinFunction(
        DefId,
        std::sync::Arc<dyn crate::traits::function::FunctionImpl>,
    ),

    /// List of heterogeneous values (for collecting results, extensions, etc.)
    List(Vec<Value>),

    /// Field selector - represents a type-safe reference to a field name
    /// Evaluates to the field name as a string at runtime
    /// Used for functions like List::join(left, right, left_on: _.id, right_on: _.customer_id)
    FieldSelector(Arc<str>),

    /// Generic extension system for stdlib-defined types
    ///
    /// Permite que stdlib defina tipos wrapped (Entity, Future, Option, etc.)
    /// sin modificar este enum. Cada extensión tiene:
    /// - `type_id`: Identificador único del tipo de extensión
    /// - `value`: El valor wrapped
    /// - `metadata`: Metadata específica del tipo (compile-time info capturada)
    ///
    /// # Example
    /// ```ignore
    /// Value::Extension {
    ///     type_id: ENTITY_TYPE_ID,
    ///     value: Box::new(Value::Record(...)),
    ///     metadata: Arc::new(EntityMetadata { ... }),
    /// }
    /// ```
    Extension {
        type_id: ExtensionTypeId,
        value: Box<Value>,
        metadata: Arc<dyn ExtensionMetadata>,
    },
}

/// Environment for variable bindings during execution
#[derive(Clone, Default)]
pub struct Environment {
    bindings: HashMap<Symbol, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn bind(&mut self, name: Symbol, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn lookup(&self, name: Symbol) -> Option<&Value> {
        self.bindings.get(&name)
    }

    pub fn with_binding(mut self, name: Symbol, value: Value) -> Self {
        self.bind(name, value);
        self
    }
}
