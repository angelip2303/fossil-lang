use polars::prelude::*;

use fossil_lang::context::metadata::AttributeData;
use fossil_lang::context::{Interner, Symbol};
use fossil_lang::traits::provider::FunctionDef;

use super::{ColumnOp, TransformFunction};

#[derive(Debug)]
pub struct Trim;

#[derive(Debug)]
pub struct Lower;

#[derive(Debug)]
pub struct Upper;

#[derive(Debug)]
pub struct Slug;

#[derive(Debug)]
pub struct Default(pub String);

#[derive(Debug)]
pub struct ToNull(pub String);

#[derive(Debug)]
pub struct Min(pub i64);

#[derive(Debug)]
pub struct Max(pub i64);

#[derive(Debug)]
pub struct Replace {
    pub pattern: String,
    pub replacement: String,
}

impl ColumnOp for Trim {
    fn apply(&self, expr: Expr) -> Expr {
        crate::string::trim(expr)
    }
}

impl ColumnOp for Lower {
    fn apply(&self, expr: Expr) -> Expr {
        crate::string::lower(expr)
    }
}

impl ColumnOp for Upper {
    fn apply(&self, expr: Expr) -> Expr {
        crate::string::upper(expr)
    }
}

impl ColumnOp for Slug {
    fn apply(&self, expr: Expr) -> Expr {
        crate::string::slug(expr)
    }
}

impl ColumnOp for Default {
    fn apply(&self, expr: Expr) -> Expr {
        when(expr.clone().is_null())
            .then(lit(self.0.clone()))
            .otherwise(expr)
    }
}

impl ColumnOp for ToNull {
    fn apply(&self, expr: Expr) -> Expr {
        when(expr.clone().eq(lit(self.0.clone())))
            .then(lit(NULL))
            .otherwise(expr)
    }
}

impl ColumnOp for Min {
    fn apply(&self, expr: Expr) -> Expr {
        when(expr.clone().lt(lit(self.0)))
            .then(lit(NULL))
            .otherwise(expr)
    }
}

impl ColumnOp for Max {
    fn apply(&self, expr: Expr) -> Expr {
        when(expr.clone().gt(lit(self.0)))
            .then(lit(NULL))
            .otherwise(expr)
    }
}

impl ColumnOp for Replace {
    fn apply(&self, expr: Expr) -> Expr {
        expr.str()
            .replace_all(lit(self.pattern.clone()), lit(self.replacement.clone()), false)
    }
}

pub fn parse_clean_attr(attr_data: &AttributeData, interner: &Interner) -> Vec<Box<dyn ColumnOp>> {
    let mut ops: Vec<Box<dyn ColumnOp>> = Vec::new();
    let attr = fossil_lang::context::metadata::TypedAttribute::new(attr_data, interner);

    if let Some(keyword) = attr_data.first_positional_string(interner) {
        match keyword {
            "trim" => ops.push(Box::new(Trim)),
            "lower" => ops.push(Box::new(Lower)),
            "upper" => ops.push(Box::new(Upper)),
            "slug" => ops.push(Box::new(Slug)),
            _ => {}
        }
        return ops;
    }

    if let Some(v) = attr.string("default") {
        ops.push(Box::new(Default(v.to_string())));
    }
    if let Some(v) = attr.string("to_null") {
        ops.push(Box::new(ToNull(v.to_string())));
    }
    if let Some(n) = attr.int("min") {
        ops.push(Box::new(Min(n)));
    }
    if let Some(n) = attr.int("max") {
        ops.push(Box::new(Max(n)));
    }
    if let Some(pattern) = attr.string("replace") {
        let replacement = attr.string("with").unwrap_or("");
        ops.push(Box::new(Replace {
            pattern: pattern.to_string(),
            replacement: replacement.to_string(),
        }));
    }

    ops
}

pub fn clean_functions(type_name: Symbol) -> Vec<FunctionDef> {
    vec![FunctionDef::new(
        "clean",
        TransformFunction {
            type_name,
            namespace: "clean",
            label: "clean",
            parse: parse_clean_attr,
        },
    )]
}
