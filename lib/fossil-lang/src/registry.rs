//! Registry — compile-time function and attribute definitions.
//!
//! Functions and attributes are DATA, not traits. Each is a const struct
//! registered via a builder pattern. The host (keasy) registers sources;
//! fossil-lang only defines the interface.
//!
//! Reference: F# Type Providers, Malloy Connection, DataFusion TableProvider.

/// Compile-time function definition (data, not a trait).
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: &'static str,
    pub namespace: &'static str,
    pub params: &'static [ParamDef],
    pub returns: ReturnType,
    pub impl_: OpImpl,
    /// Static schema known at definition time (F# erased type pattern).
    /// `Some` → compiler uses directly (text, pdf, etc.).
    /// `None` → host resolves via `HasSchemaResolver` at compile time.
    pub schema: Option<&'static [(&'static str, &'static str)]>,
}

/// How a function is implemented at execution time.
#[derive(Debug, Clone)]
pub enum OpImpl {
    /// External data source. Format is opaque to the compiler.
    /// The host's `SqlDialect` decides how to execute it.
    Source { format: &'static str },
    /// Output materialization (GraphAr, Turtle).
    Output { format: &'static str },
    /// Multi-step pipeline requiring host orchestration.
    Pipeline { handler: &'static str },
}

/// Attribute transform applied via `#[namespace(name)]`.
#[derive(Debug, Clone)]
pub struct AttributeOp {
    pub namespace: &'static str,
    pub name: &'static str,
    pub sql_template: &'static str,
}

/// Function parameter definition.
#[derive(Debug, Clone, Copy)]
pub struct ParamDef {
    pub name: &'static str,
    pub required: bool,
    pub default: &'static str,
}

/// What a function returns.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReturnType {
    Table,
    Emission,
    Unit,
}

// ── Constructors ─────────────────────────────────────────────────────

impl FunctionDef {
    /// Source with dynamic schema (host resolves at compile time).
    pub const fn source(name: &'static str, params: &'static [ParamDef]) -> Self {
        Self {
            name,
            namespace: "",
            params,
            returns: ReturnType::Table,
            impl_: OpImpl::Source { format: name },
            schema: None,
        }
    }

    /// Source with static schema known at definition time.
    pub const fn source_with_schema(
        name: &'static str,
        params: &'static [ParamDef],
        schema: &'static [(&'static str, &'static str)],
    ) -> Self {
        Self {
            name,
            namespace: "",
            params,
            returns: ReturnType::Table,
            impl_: OpImpl::Source { format: name },
            schema: Some(schema),
        }
    }

    pub const fn output(namespace: &'static str, name: &'static str, format: &'static str) -> Self {
        Self {
            name,
            namespace,
            params: &[],
            returns: ReturnType::Unit,
            impl_: OpImpl::Output { format },
            schema: None,
        }
    }

    pub const fn pipeline(namespace: &'static str, name: &'static str, handler: &'static str) -> Self {
        Self {
            name,
            namespace,
            params: &[],
            returns: ReturnType::Emission,
            impl_: OpImpl::Pipeline { handler },
            schema: None,
        }
    }
}

impl ParamDef {
    pub const fn required(name: &'static str) -> Self {
        Self { name, required: true, default: "" }
    }

    pub const fn with_default(name: &'static str, default: &'static str) -> Self {
        Self { name, required: false, default }
    }
}

impl AttributeOp {
    pub const fn new(namespace: &'static str, name: &'static str, sql_template: &'static str) -> Self {
        Self { namespace, name, sql_template }
    }
}

// ── Registry (builder pattern) ───────────────────────────────────────

/// Compile-time registry of all available functions and attributes.
///
/// The host registers sources; fossil-lang registers language features
/// (outputs, pipelines, attributes).
#[derive(Debug, Default)]
pub struct Registry {
    pub functions: Vec<FunctionDef>,
    pub attributes: Vec<AttributeOp>,
}

impl Registry {
    pub fn new() -> Self { Self::default() }

    pub fn add_function(&mut self, f: FunctionDef) -> &mut Self {
        self.functions.push(f); self
    }

    pub fn add_functions(&mut self, fns: &[FunctionDef]) -> &mut Self {
        self.functions.extend_from_slice(fns); self
    }

    pub fn add_attribute(&mut self, a: AttributeOp) -> &mut Self {
        self.attributes.push(a); self
    }

    pub fn add_attributes(&mut self, attrs: &[AttributeOp]) -> &mut Self {
        self.attributes.extend_from_slice(attrs); self
    }

    pub fn find_function(&self, namespace: &str, name: &str) -> Option<&FunctionDef> {
        self.functions.iter().find(|f| f.namespace == namespace && f.name == name)
    }

    pub fn find_source(&self, name: &str) -> Option<&FunctionDef> {
        self.find_function("", name)
    }

    pub fn find_attribute(&self, namespace: &str, name: &str) -> Option<&AttributeOp> {
        self.attributes.iter().find(|a| a.namespace == namespace && a.name == name)
    }
}
