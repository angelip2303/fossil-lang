use std::sync::Arc;

use crate::ast::ast::{Ast, Literal, ProviderArgument, TypeId};
use crate::context::{Interner, Symbol};
use crate::error::ProviderError;
use crate::traits::function::FunctionImpl;

/// Output from a type provider (F# style)
///
/// Providers generate both a type AND an optional module specification.
/// When a module is generated, it contains functions that operate on the type.
///
/// Example: `csv<"people.csv">` generates:
/// - Type: `{ id: int, name: string, age: int }`
/// - Module: `People` with `People::load()` function
pub struct ProviderOutput {
    /// The generated AST type
    pub generated_type: TypeId,

    /// Optional module specification
    /// When Some, a module with this name will be created containing functions
    pub module_spec: Option<ModuleSpec>,
}

/// Specification for a generated module
///
/// Describes the functions and submodules that should be created
/// for a provider-generated module.
pub struct ModuleSpec {
    /// Functions to register in the generated module
    pub functions: Vec<FunctionDef>,

    /// Nested submodules (for future extensibility)
    pub submodules: Vec<(String, ModuleSpec)>,
}

/// Definition of a function to be registered in a generated module
pub struct FunctionDef {
    /// Function name (e.g., "load", "save")
    pub name: String,

    /// Rust implementation of the function
    pub implementation: Arc<dyn FunctionImpl>,
}

/// Information about a provider parameter
#[derive(Debug, Clone)]
pub struct ProviderParamInfo {
    /// Parameter name
    pub name: &'static str,
    /// Whether this parameter is required
    pub required: bool,
    /// Default value if not provided (only for optional params)
    pub default: Option<Literal>,
}

/// Resolved provider arguments (name → value mapping)
#[derive(Debug, Default)]
pub struct ResolvedProviderArgs {
    args: std::collections::HashMap<Symbol, Literal>,
}

impl ResolvedProviderArgs {
    /// Get a required string argument
    pub fn get_string(&self, name: Symbol, interner: &Interner) -> Option<String> {
        self.args.get(&name).and_then(|lit| {
            if let Literal::String(sym) = lit {
                Some(interner.resolve(*sym).to_string())
            } else {
                None
            }
        })
    }

    /// Get an optional boolean argument
    pub fn get_bool(&self, name: Symbol) -> Option<bool> {
        self.args.get(&name).and_then(|lit| {
            if let Literal::Boolean(b) = lit {
                Some(*b)
            } else {
                None
            }
        })
    }

    /// Check if an argument was provided
    pub fn has(&self, name: Symbol) -> bool {
        self.args.contains_key(&name)
    }

    /// Insert an argument
    pub fn insert(&mut self, name: Symbol, value: Literal) {
        self.args.insert(name, value);
    }
}

/// The TypeProvider trait generates an AST type and optional module at compile-time
///
/// Providers have access to the AST type arena to allocate types,
/// and the interner for symbol management.
///
/// The generated AST type flows through the normal pipeline:
/// AST → Lower → HIR → TypeCheck → THIR
///
/// # F# Style Type Providers
///
/// Like F#, providers can generate both types and modules. For example,
/// a CSV provider generates a record type based on the CSV schema,
/// and a module containing a `load()` function to load the CSV at runtime.
///
/// Example:
/// ```ignore
/// type People = csv!("people.csv")
/// let data = People::load()  // Generated module function
/// ```
pub trait TypeProviderImpl: Send + Sync {
    /// Define the expected parameters for this provider
    ///
    /// Returns information about each parameter: name, required, default value.
    /// Used for validation and documentation.
    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![]
    }

    /// Generate an AST type and optional module from compile-time arguments
    ///
    /// # Arguments
    /// * `args` - Arguments passed to the provider (positional or named)
    /// * `ast` - Mutable access to the AST arena for type allocation
    /// * `interner` - Mutable access to the symbol interner
    /// * `type_name` - The name of the type being defined (e.g., "PersonData")
    ///
    /// # Returns
    /// A `ProviderOutput` containing:
    /// - The generated type
    /// - An optional module specification with functions
    fn provide(
        &self,
        args: &[ProviderArgument],
        ast: &mut Ast,
        interner: &mut Interner,
        type_name: &str,
    ) -> Result<ProviderOutput, ProviderError>;
}
