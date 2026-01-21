//! Attribute schema and validation infrastructure
//!
//! This module provides a generic, extensible attribute system with compile-time
//! validation. Modules can register attribute schemas that define:
//! - The attribute name (e.g., "rdf", "sql", "optional")
//! - Valid targets (Field, Type, Function, Any)
//! - Required and optional arguments with their types
//!
//! # Example
//!
//! ```rust,ignore
//! // Register an attribute schema
//! gcx.register_attribute(
//!     AttributeSchema::new("rdf", AttributeTarget::Field)
//!         .arg("uri", ArgSpec::required(ArgType::String))
//!         .arg("prefix", ArgSpec::optional(ArgType::String))
//!         .description("RDF predicate mapping for fields")
//! );
//! ```
//!
//! # Validation
//!
//! During the resolution phase, attributes are validated against their schemas:
//! - Unknown attributes produce errors
//! - Missing required arguments produce errors
//! - Type mismatches produce errors
//! - Unknown arguments produce errors with "Did you mean...?" suggestions

use std::collections::HashMap;

use crate::ast::ast::Literal;
use crate::context::Symbol;

/// Types that attribute arguments can have
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgType {
    /// A string literal (e.g., `uri = "http://..."`)
    String,
    /// An integer literal (e.g., `max_length = 100`)
    Int,
    /// A boolean literal (e.g., `required = true`)
    Bool,
    /// Any literal type is accepted
    Any,
}

impl ArgType {
    /// Check if a literal matches this argument type
    pub fn matches(&self, literal: &Literal) -> bool {
        match (self, literal) {
            (ArgType::String, Literal::String(_)) => true,
            (ArgType::Int, Literal::Integer(_)) => true,
            (ArgType::Bool, Literal::Boolean(_)) => true,
            (ArgType::Any, _) => true,
            _ => false,
        }
    }

    /// Get a human-readable name for this type
    pub fn name(&self) -> &'static str {
        match self {
            ArgType::String => "string",
            ArgType::Int => "int",
            ArgType::Bool => "bool",
            ArgType::Any => "any",
        }
    }
}

/// Specification for a single attribute argument
#[derive(Debug, Clone)]
pub struct ArgSpec {
    /// The expected type of this argument
    pub ty: ArgType,
    /// Whether this argument is required
    pub required: bool,
    /// Optional default value if not provided
    pub default: Option<Literal>,
    /// Optional description for documentation/LSP
    pub description: Option<&'static str>,
}

impl ArgSpec {
    /// Create a required argument specification
    pub fn required(ty: ArgType) -> Self {
        Self {
            ty,
            required: true,
            default: None,
            description: None,
        }
    }

    /// Create an optional argument specification
    pub fn optional(ty: ArgType) -> Self {
        Self {
            ty,
            required: false,
            default: None,
            description: None,
        }
    }

    /// Create an optional argument with a default value
    pub fn with_default(ty: ArgType, default: Literal) -> Self {
        Self {
            ty,
            required: false,
            default: Some(default),
            description: None,
        }
    }

    /// Add a description to this argument spec
    pub fn describe(mut self, description: &'static str) -> Self {
        self.description = Some(description);
        self
    }
}

/// Where an attribute can be applied
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeTarget {
    /// Can be applied to record fields
    Field,
    /// Can be applied to type definitions
    Type,
    /// Can be applied to function definitions
    Function,
    /// Can be applied anywhere
    Any,
}

impl AttributeTarget {
    /// Check if this target allows the given usage
    pub fn allows(&self, usage: AttributeTarget) -> bool {
        matches!(
            (self, usage),
            (AttributeTarget::Any, _)
                | (AttributeTarget::Field, AttributeTarget::Field)
                | (AttributeTarget::Type, AttributeTarget::Type)
                | (AttributeTarget::Function, AttributeTarget::Function)
        )
    }

    /// Get a human-readable name for this target
    pub fn name(&self) -> &'static str {
        match self {
            AttributeTarget::Field => "field",
            AttributeTarget::Type => "type",
            AttributeTarget::Function => "function",
            AttributeTarget::Any => "any",
        }
    }
}

/// Schema definition for an attribute
///
/// Defines the name, valid targets, and arguments for an attribute.
#[derive(Debug, Clone)]
pub struct AttributeSchema {
    /// The attribute name (e.g., "rdf", "sql", "optional")
    pub name: &'static str,
    /// Where this attribute can be applied
    pub target: AttributeTarget,
    /// Argument specifications by name
    pub args: HashMap<&'static str, ArgSpec>,
    /// Optional description for documentation
    pub description: Option<&'static str>,
}

impl AttributeSchema {
    /// Create a new attribute schema
    pub fn new(name: &'static str, target: AttributeTarget) -> Self {
        Self {
            name,
            target,
            args: HashMap::new(),
            description: None,
        }
    }

    /// Add an argument to this schema
    pub fn arg(mut self, name: &'static str, spec: ArgSpec) -> Self {
        self.args.insert(name, spec);
        self
    }

    /// Add a description to this schema
    pub fn description(mut self, description: &'static str) -> Self {
        self.description = Some(description);
        self
    }

    /// Get all argument names for "Did you mean...?" suggestions
    pub fn arg_names(&self) -> Vec<String> {
        self.args.keys().map(|s| s.to_string()).collect()
    }

    /// Check if an argument name exists
    pub fn has_arg(&self, name: &str) -> bool {
        self.args.contains_key(name)
    }

    /// Get the spec for an argument
    pub fn get_arg(&self, name: &str) -> Option<&ArgSpec> {
        self.args.get(name)
    }

    /// Get all required argument names
    pub fn required_args(&self) -> Vec<&'static str> {
        self.args
            .iter()
            .filter(|(_, spec)| spec.required)
            .map(|(name, _)| *name)
            .collect()
    }
}

/// Registry of all attribute schemas
///
/// Central registry for attribute schemas that modules can use to register
/// their custom attributes. Used during compilation to validate attributes.
#[derive(Debug, Clone, Default)]
pub struct AttributeRegistry {
    /// Schemas indexed by attribute name (as Symbol)
    schemas: HashMap<Symbol, AttributeSchema>,
    /// Schemas indexed by name string (for lookup before interning)
    schemas_by_name: HashMap<&'static str, AttributeSchema>,
}

impl AttributeRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            schemas: HashMap::new(),
            schemas_by_name: HashMap::new(),
        }
    }

    /// Register an attribute schema
    ///
    /// The schema is stored both by Symbol (for fast runtime lookup) and by
    /// name string (for registration before the name is interned).
    pub fn register(&mut self, schema: AttributeSchema, name_symbol: Symbol) {
        self.schemas_by_name.insert(schema.name, schema.clone());
        self.schemas.insert(name_symbol, schema);
    }

    /// Look up a schema by its Symbol
    pub fn get(&self, name: Symbol) -> Option<&AttributeSchema> {
        self.schemas.get(&name)
    }

    /// Look up a schema by its name string
    pub fn get_by_name(&self, name: &str) -> Option<&AttributeSchema> {
        self.schemas_by_name.get(name)
    }

    /// Check if an attribute is registered
    pub fn is_registered(&self, name: Symbol) -> bool {
        self.schemas.contains_key(&name)
    }

    /// Get all registered attribute names for "Did you mean...?" suggestions
    pub fn all_names(&self) -> Vec<String> {
        self.schemas_by_name.keys().map(|s| s.to_string()).collect()
    }

    /// Get all registered schemas
    pub fn all_schemas(&self) -> impl Iterator<Item = &AttributeSchema> {
        self.schemas_by_name.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Interner;

    #[test]
    fn test_arg_type_matches() {
        assert!(ArgType::String.matches(&Literal::String(Symbol::synthetic())));
        assert!(ArgType::Int.matches(&Literal::Integer(42)));
        assert!(ArgType::Bool.matches(&Literal::Boolean(true)));
        assert!(ArgType::Any.matches(&Literal::String(Symbol::synthetic())));
        assert!(ArgType::Any.matches(&Literal::Integer(42)));
        assert!(ArgType::Any.matches(&Literal::Boolean(true)));

        assert!(!ArgType::String.matches(&Literal::Integer(42)));
        assert!(!ArgType::Int.matches(&Literal::String(Symbol::synthetic())));
        assert!(!ArgType::Bool.matches(&Literal::String(Symbol::synthetic())));
    }

    #[test]
    fn test_attribute_target_allows() {
        assert!(AttributeTarget::Any.allows(AttributeTarget::Field));
        assert!(AttributeTarget::Any.allows(AttributeTarget::Type));
        assert!(AttributeTarget::Any.allows(AttributeTarget::Function));
        assert!(AttributeTarget::Field.allows(AttributeTarget::Field));
        assert!(!AttributeTarget::Field.allows(AttributeTarget::Type));
    }

    #[test]
    fn test_attribute_schema_builder() {
        let schema = AttributeSchema::new("rdf", AttributeTarget::Field)
            .arg("uri", ArgSpec::required(ArgType::String))
            .arg("prefix", ArgSpec::optional(ArgType::String))
            .description("RDF predicate mapping");

        assert_eq!(schema.name, "rdf");
        assert_eq!(schema.target, AttributeTarget::Field);
        assert!(schema.has_arg("uri"));
        assert!(schema.has_arg("prefix"));
        assert!(!schema.has_arg("unknown"));

        let required = schema.required_args();
        assert_eq!(required.len(), 1);
        assert!(required.contains(&"uri"));
    }

    #[test]
    fn test_attribute_registry() {
        let mut interner = Interner::default();
        let mut registry = AttributeRegistry::new();

        let schema = AttributeSchema::new("rdf", AttributeTarget::Field)
            .arg("uri", ArgSpec::required(ArgType::String));

        let rdf_sym = interner.intern("rdf");
        registry.register(schema, rdf_sym);

        assert!(registry.is_registered(rdf_sym));
        assert!(registry.get(rdf_sym).is_some());
        assert!(registry.get_by_name("rdf").is_some());

        let names = registry.all_names();
        assert!(names.contains(&"rdf".to_string()));
    }
}
