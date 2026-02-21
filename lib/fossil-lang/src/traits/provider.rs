use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::Loc;
use crate::ast::{Attribute, Literal, PrimitiveType, ProviderArgument};
use crate::context::{Interner, Symbol};
use crate::error::{FossilError, FossilWarnings};
use crate::runtime::storage::StorageConfig;
use crate::traits::function::FunctionImpl;

pub trait FileReader: Send + Sync + std::fmt::Debug {
    fn read_to_string(&self, url: &str) -> Result<String, String>;
}

#[derive(Debug)]
pub struct LocalFileReader;

impl FileReader for LocalFileReader {
    fn read_to_string(&self, path: &str) -> Result<String, String> {
        std::fs::read_to_string(path).map_err(|e| e.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderKind {
    /// Only generates a type definition (e.g. ShEx). Use with `type X = provider!(...)`.
    Schema,
    /// Generates a type + loads data (e.g. CSV). Use with `let x = provider!(...)`.
    Data,
    /// Works in both positions: `type X = provider!(...)` or `let x = provider!(...)`.
    Both,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct ProviderInfo {
    pub extensions: &'static [&'static str],
    pub kind: ProviderKind,
}

#[derive(Debug, Clone)]
pub enum FieldType {
    Primitive(PrimitiveType),
    Optional(Box<FieldType>),
    Named(String),
}

#[derive(Debug, Clone)]
pub struct FieldSpec {
    pub name: Symbol,
    pub ty: FieldType,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct ProviderSchema {
    pub fields: Vec<FieldSpec>,
}

pub struct ProviderOutput {
    pub schema: ProviderSchema,
    pub module_spec: Option<ModuleSpec>,
    pub warnings: FossilWarnings,
    /// Provider-generated type-level attributes (e.g., base IRI from ShEx).
    /// These are merged with user attributes during expansion; user attrs override.
    pub type_attributes: Vec<Attribute>,
}

impl ProviderOutput {
    pub fn new(schema: ProviderSchema) -> Self {
        Self {
            schema,
            module_spec: None,
            warnings: FossilWarnings::new(),
            type_attributes: Vec::new(),
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

    pub fn with_type_attributes(mut self, attrs: Vec<Attribute>) -> Self {
        self.type_attributes = attrs;
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

impl FunctionDef {
    pub fn new(name: impl Into<String>, implementation: impl FunctionImpl + 'static) -> Self {
        Self {
            name: name.into(),
            implementation: Arc::new(implementation),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProviderParamInfo {
    pub name: &'static str,
    pub required: bool,
    pub default: Option<Literal>,
}

#[derive(Debug, Clone)]
pub enum ProviderLiteral {
    String(String),
    Integer(i64),
    Boolean(bool),
}

#[derive(Debug, Default)]
pub struct ProviderArgs {
    args: HashMap<String, ProviderLiteral>,
}

impl ProviderArgs {
    pub fn get_string(&self, name: &str) -> Option<&str> {
        match self.args.get(name) {
            Some(ProviderLiteral::String(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn get_bool(&self, name: &str) -> Option<bool> {
        match self.args.get(name) {
            Some(ProviderLiteral::Boolean(b)) => Some(*b),
            _ => None,
        }
    }

    pub fn require_string(&self, name: &'static str, provider: &'static str, loc: Loc) -> Result<&str, FossilError> {
        self.get_string(name)
            .ok_or_else(|| FossilError::missing_argument(name, provider, loc))
    }
}

pub fn resolve_to_provider_args(
    args: &[ProviderArgument],
    param_info: &[ProviderParamInfo],
    interner: &Interner,
    provider_name: &'static str,
    loc: Loc,
) -> Result<ProviderArgs, FossilError> {
    let param_name_to_index: HashMap<&str, usize> = param_info
        .iter()
        .enumerate()
        .map(|(i, p)| (p.name, i))
        .collect();

    let mut filled: Vec<bool> = vec![false; param_info.len()];
    let mut result = HashMap::new();
    let mut positional_idx = 0;

    for arg in args {
        match arg {
            ProviderArgument::Positional(lit) => {
                if positional_idx < param_info.len() {
                    filled[positional_idx] = true;
                    let key = param_info[positional_idx].name.to_string();
                    result.insert(key, literal_to_provider_literal(lit, interner));
                    positional_idx += 1;
                }
            }
            ProviderArgument::Named { name, value } => {
                let name_str = interner.resolve(*name);
                if let Some(&idx) = param_name_to_index.get(name_str) {
                    filled[idx] = true;
                }
                result.insert(name_str.to_string(), literal_to_provider_literal(value, interner));
            }
        }
    }

    for (idx, param) in param_info.iter().enumerate() {
        if !filled[idx] && param.required {
            return Err(FossilError::missing_argument(param.name, provider_name, loc));
        }
    }

    Ok(ProviderArgs { args: result })
}

fn literal_to_provider_literal(lit: &Literal, interner: &Interner) -> ProviderLiteral {
    match lit {
        Literal::String(sym) => ProviderLiteral::String(interner.resolve(*sym).to_string()),
        Literal::Integer(n) => ProviderLiteral::Integer(*n),
        Literal::Boolean(b) => ProviderLiteral::Boolean(*b),
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeRegistry {
    identity_to_name: HashMap<String, String>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, identity: String, fossil_name: String) {
        self.identity_to_name.insert(identity, fossil_name);
    }

    pub fn resolve(&self, identity: &str) -> Option<&str> {
        self.identity_to_name.get(identity).map(|s| s.as_str())
    }

    pub fn resolve_name(&self, identity: &str) -> String {
        if let Some(name) = self.identity_to_name.get(identity) {
            return name.clone();
        }
        let s = identity.trim_start_matches('<').trim_end_matches('>');
        if let Some(pos) = s.rfind('#') {
            return s[pos + 1..].to_string();
        }
        if let Some(pos) = s.rfind('/') {
            return s[pos + 1..].to_string();
        }
        s.to_string()
    }
}

pub struct ProviderContext<'a> {
    pub interner: &'a mut Interner,
    pub storage: &'a StorageConfig,
    pub file_reader: &'a dyn FileReader,
}

pub trait TypeProviderImpl: Send + Sync {
    fn info(&self) -> ProviderInfo;

    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![]
    }

    /// Returns the external identity (e.g. full IRI) for the type this provider generates.
    /// Used during pre-scan to build the TypeRegistry for cross-reference resolution.
    fn type_identity(&self, _args: &ProviderArgs, _reader: &dyn FileReader) -> Option<String> {
        None
    }

    fn provide(
        &self,
        args: &ProviderArgs,
        ctx: &mut ProviderContext,
        type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError>;
}
