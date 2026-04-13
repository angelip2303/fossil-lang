//! Type metadata infrastructure
//!
//! This module provides infrastructure for capturing and storing compile-time
//! metadata about types, particularly attributes on record fields. This metadata
//! can then be accessed at runtime by builtin functions.
//!
//! # Architecture
//!
//! ```text
//! AST (RecordField.attrs)
//!   → Resolution (extract_type_metadata)
//!   → TypeMetadataMap (built during lowering)
//!   → Runtime (FunctionImpl accesses via RuntimeContext)
//! ```

use std::collections::HashMap;

use crate::ast::{Attribute, AttributeArg, Literal, StmtKind, TypeKind};
use crate::db::Symbol;

/// Type metadata (type-level + field-level attributes) extracted from AST
#[derive(Debug, Clone, PartialEq)]
pub struct TypeMetadata {
    pub type_attributes: Vec<AttributeData>,
    pub field_metadata: HashMap<Symbol, FieldMetadata>,
}

impl TypeMetadata {
    pub fn new() -> Self {
        Self {
            type_attributes: Vec::new(),
            field_metadata: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.field_metadata.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldMetadata {
    pub attributes: Vec<AttributeData>,
}

impl FieldMetadata {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    pub fn get_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.attributes.iter().find(|attr| attr.name == name)
    }
}

impl Default for FieldMetadata {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeData {
    pub name: Symbol,
    pub args: HashMap<Symbol, Literal>,
    pub positional: Vec<Literal>,
}

impl AttributeData {
    pub fn get(&self, key: Symbol) -> Option<&Literal> {
        self.args.get(&key)
    }

    fn from_attribute(attr: &Attribute) -> Self {
        let mut args = HashMap::new();
        let mut positional = Vec::new();
        for arg in &attr.args {
            match arg {
                AttributeArg::Named { key, value } => {
                    args.insert(*key, value.clone());
                }
                AttributeArg::Positional(value) => {
                    positional.push(value.clone());
                }
            }
        }
        AttributeData {
            name: attr.name,
            args,
            positional,
        }
    }
}

pub fn extract_type_metadata(ast: &crate::ast::Ast) -> HashMap<Symbol, TypeMetadata> {
    let mut result = HashMap::new();

    for &stmt_id in &ast.root {
        let stmt = &ast.stmts[stmt_id];

        if let StmtKind::Type {
            name, ty, attrs, ..
        } = &stmt.kind
        {
            let ty_node = &ast.types[*ty];

            let mut metadata = TypeMetadata::new();

            for attr in attrs {
                metadata
                    .type_attributes
                    .push(AttributeData::from_attribute(attr));
            }

            if let TypeKind::Record(fields) = &ty_node.kind {
                for field in fields {
                    if !field.attrs.is_empty() {
                        let mut field_meta = FieldMetadata::new();
                        for attr in &field.attrs {
                            field_meta
                                .attributes
                                .push(AttributeData::from_attribute(attr));
                        }
                        metadata.field_metadata.insert(field.name, field_meta);
                    }
                }
            }

            if !metadata.is_empty() || !metadata.type_attributes.is_empty() {
                result.insert(*name, metadata);
            }
        }
    }

    result
}

