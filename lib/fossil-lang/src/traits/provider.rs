use std::sync::Arc;

use crate::ast::Loc;
use crate::ast::ast::{Ast, Literal, ProviderArgument, TypeId};
use crate::context::{Interner, Symbol};
use crate::error::{FossilError, FossilWarnings};
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
    pub generated_type: TypeId,
    pub module_spec: Option<ModuleSpec>,
    pub warnings: FossilWarnings,
}

impl ProviderOutput {
    pub fn new(generated_type: TypeId) -> Self {
        Self {
            generated_type,
            module_spec: None,
            warnings: FossilWarnings::new(),
        }
    }

    pub fn with_module(mut self, module_spec: ModuleSpec) -> Self {
        self.module_spec = Some(module_spec);
        self
    }

    pub fn with_warnings(mut self, warnings: FossilWarnings) -> Self {
        self.warnings = warnings;
        self
    }
}

pub struct ModuleSpec {
    pub functions: Vec<FunctionDef>,
}

pub struct FunctionDef {
    pub name: String,
    pub implementation: Arc<dyn FunctionImpl>,
}

#[derive(Debug, Clone)]
pub struct ProviderParamInfo {
    pub name: &'static str,
    pub required: bool,
    pub default: Option<Literal>,
}

#[derive(Debug, Default)]
pub struct ResolvedProviderArgs {
    /// Positional args stored as (param_index, literal)
    positional: Vec<(usize, Literal)>,
    /// Named args stored by symbol
    named: std::collections::HashMap<Symbol, Literal>,
}

impl ResolvedProviderArgs {
    /// Get the literal for a positional argument by index
    pub fn get_positional(&self, index: usize) -> Option<&Literal> {
        self.positional
            .iter()
            .find(|(i, _)| *i == index)
            .map(|(_, lit)| lit)
    }

    /// Get the literal for a named argument by symbol
    pub fn get_named(&self, name: Symbol) -> Option<&Literal> {
        self.named.get(&name)
    }

    /// Get string from a named argument
    pub fn get_named_string(&self, name: Symbol, interner: &Interner) -> Option<String> {
        self.named.get(&name).and_then(|lit| {
            if let Literal::String(sym) = lit {
                Some(interner.resolve(*sym).to_string())
            } else {
                None
            }
        })
    }

    /// Get bool from a named argument
    pub fn get_named_bool(&self, name: Symbol) -> Option<bool> {
        self.named.get(&name).and_then(|lit| {
            if let Literal::Boolean(b) = lit {
                Some(*b)
            } else {
                None
            }
        })
    }

    /// Get string from a positional argument
    pub fn get_positional_string(&self, index: usize, interner: &Interner) -> Option<String> {
        self.get_positional(index).and_then(|lit| {
            if let Literal::String(sym) = lit {
                Some(interner.resolve(*sym).to_string())
            } else {
                None
            }
        })
    }
}

/// Resolve provider arguments against parameter definitions
///
/// This function handles:
/// - Positional arguments mapped to parameters in order
/// - Named arguments matched by name
/// - Default values for optional parameters
/// - Validation of required parameters
///
/// # Arguments
/// * `args` - The provider arguments from the AST
/// * `param_info` - Parameter definitions from the provider
/// * `interner` - Symbol interner for name resolution
/// * `provider_name` - Provider name for error messages
/// * `loc` - Source location for error reporting
pub fn resolve_args(
    args: &[ProviderArgument],
    param_info: &[ProviderParamInfo],
    interner: &Interner,
    provider_name: &'static str,
    loc: Loc,
) -> Result<ResolvedProviderArgs, FossilError> {
    let mut resolved = ResolvedProviderArgs::default();

    // Build a map of param name -> index for quick lookup
    let param_name_to_index: std::collections::HashMap<&str, usize> = param_info
        .iter()
        .enumerate()
        .map(|(i, p)| (p.name, i))
        .collect();

    // Track which params have been filled (by index)
    let mut filled: Vec<bool> = vec![false; param_info.len()];
    let mut positional_idx = 0;

    // First pass: process all arguments
    for arg in args {
        match arg {
            ProviderArgument::Positional(lit) => {
                // Map positional arg to parameter by index
                if positional_idx < param_info.len() {
                    filled[positional_idx] = true;
                    // Store by index (we'll resolve names later)
                    resolved.positional.push((positional_idx, lit.clone()));
                    positional_idx += 1;
                }
            }
            ProviderArgument::Named { name, value } => {
                // Named arg - store the symbol directly
                let name_str = interner.resolve(*name);
                if let Some(&idx) = param_name_to_index.get(name_str) {
                    filled[idx] = true;
                }
                resolved.named.insert(*name, value.clone());
            }
        }
    }

    // Second pass: check required params and apply defaults
    for (idx, param) in param_info.iter().enumerate() {
        if !filled[idx] {
            if param.required {
                return Err(FossilError::missing_argument(param.name, provider_name, loc));
            }
            // Default values are applied by the caller if needed
        }
    }

    Ok(resolved)
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
    /// * `loc` - Source location of the provider invocation for error reporting
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
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError>;
}
