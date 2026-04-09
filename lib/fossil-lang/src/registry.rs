//! Registry — compile-time function and attribute definitions.
//!
//! Functions and attributes are DATA, not traits. Each is a const struct
//! registered via a builder pattern in the composition root.
//!
//! Reference: Malloy blueprints, DataFusion Signature, Ibis SIMPLE_OPS.

/// Compile-time function definition (data, not a trait).
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: &'static str,
    pub namespace: &'static str,
    pub params: &'static [ParamDef],
    pub returns: ReturnType,
    pub impl_: OpImpl,
}

/// How a function is implemented at execution time.
#[derive(Debug, Clone)]
pub enum OpImpl {
    /// SQL expression that loads data: "SELECT * FROM read_csv('{path}')"
    SourceSql(&'static str),
    /// Host-side preprocessing (PDF, DOCX, IFC).
    Preprocess {
        handler: &'static str,
        /// Static schema for the preprocessed output.
        /// Defined by the plugin (e.g., fossil-doc defines PDF → {text: String, page: Int}).
        schema: &'static [(&'static str, &'static str)],
    },
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
    pub const fn source(name: &'static str, sql: &'static str, params: &'static [ParamDef]) -> Self {
        Self {
            name,
            namespace: "",
            params,
            returns: ReturnType::Table,
            impl_: OpImpl::SourceSql(sql),
        }
    }

    pub const fn output(namespace: &'static str, name: &'static str, format: &'static str) -> Self {
        Self {
            name,
            namespace,
            params: &[],
            returns: ReturnType::Unit,
            impl_: OpImpl::Output { format },
        }
    }

    pub const fn pipeline(namespace: &'static str, name: &'static str, handler: &'static str) -> Self {
        Self {
            name,
            namespace,
            params: &[],
            returns: ReturnType::Emission,
            impl_: OpImpl::Pipeline { handler },
        }
    }

    pub const fn preprocess(
        name: &'static str,
        handler: &'static str,
        params: &'static [ParamDef],
        schema: &'static [(&'static str, &'static str)],
    ) -> Self {
        Self {
            name,
            namespace: "",
            params,
            returns: ReturnType::Table,
            impl_: OpImpl::Preprocess { handler, schema },
        }
    }
}

impl ParamDef {
    pub const fn required(name: &'static str) -> Self {
        Self {
            name,
            required: true,
            default: "",
        }
    }

    pub const fn optional(name: &'static str) -> Self {
        Self {
            name,
            required: false,
            default: "",
        }
    }

    pub const fn with_default(name: &'static str, default: &'static str) -> Self {
        Self {
            name,
            required: false,
            default,
        }
    }
}

impl AttributeOp {
    pub const fn new(namespace: &'static str, name: &'static str, sql_template: &'static str) -> Self {
        Self {
            namespace,
            name,
            sql_template,
        }
    }
}

// ── Registry (builder pattern) ───────────────────────────────────────

/// Compile-time registry of all available functions and attributes.
///
/// Built via builder pattern in the composition root (keasy main.rs).
#[derive(Debug, Default)]
pub struct Registry {
    pub functions: Vec<FunctionDef>,
    pub attributes: Vec<AttributeOp>,
}

impl Registry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, f: FunctionDef) -> &mut Self {
        self.functions.push(f);
        self
    }

    pub fn add_functions(&mut self, fns: &[FunctionDef]) -> &mut Self {
        self.functions.extend_from_slice(fns);
        self
    }

    pub fn add_attribute(&mut self, a: AttributeOp) -> &mut Self {
        self.attributes.push(a);
        self
    }

    pub fn add_attributes(&mut self, attrs: &[AttributeOp]) -> &mut Self {
        self.attributes.extend_from_slice(attrs);
        self
    }

    /// Find a function by namespace + name.
    pub fn find_function(&self, namespace: &str, name: &str) -> Option<&FunctionDef> {
        self.functions
            .iter()
            .find(|f| f.namespace == namespace && f.name == name)
    }

    /// Find a source function by name (namespace = "").
    pub fn find_source(&self, name: &str) -> Option<&FunctionDef> {
        self.find_function("", name)
    }

    /// Find an attribute op by namespace + name.
    pub fn find_attribute(&self, namespace: &str, name: &str) -> Option<&AttributeOp> {
        self.attributes
            .iter()
            .find(|a| a.namespace == namespace && a.name == name)
    }
}
