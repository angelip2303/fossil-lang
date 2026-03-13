use std::sync::Arc;

use polars::prelude::*;

use fossil_lang::context::{DefId, DefKind, Symbol};
use fossil_lang::context::global::TypeInfo;
use fossil_lang::context::metadata::TypedAttribute;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

/// Generates `Type.clean(data)` for record types with `#[clean]` attributes
pub fn clean_module_generator() -> Arc<dyn Fn(&TypeInfo) -> Option<ModuleSpec> + Send + Sync> {
    Arc::new(|info: &TypeInfo| {
        let clean_sym = info.interner.lookup("clean")?;
        let has_clean = info
            .fields
            .iter()
            .any(|f| f.attrs.iter().any(|a| a.name == clean_sym));

        if !has_clean {
            return None;
        }

        Some(ModuleSpec {
            functions: vec![FunctionDef::new("clean", CleanFunction {
                type_name: info.name,
            })],
        })
    })
}

#[derive(Debug, Clone)]
enum CleanOp {
    Trim,
    Lower,
    Upper,
    Slug,
    Default(String),
    ToNull(String),
    Min(i64),
    Max(i64),
    Replace { pattern: String, replacement: String },
}

fn resolve_type_def_id(ctx: &RuntimeContext, type_name: Symbol) -> Option<DefId> {
    ctx.gcx
        .definitions
        .find_by_symbol(type_name, |k| matches!(k, DefKind::Type))
        .map(|d| d.id())
}

fn extract_clean_ops(ctx: &RuntimeContext, type_name: Symbol) -> Vec<(String, Vec<CleanOp>)> {
    let Some(type_def_id) = resolve_type_def_id(ctx, type_name) else {
        return Vec::new();
    };

    let Some(metadata) = ctx.gcx.type_metadata.get(&type_def_id) else {
        return Vec::new();
    };

    let mut result = Vec::new();

    for (field_sym, field_meta) in &metadata.field_metadata {
        let field_name = ctx.gcx.interner.resolve(*field_sym);
        let mut ops = Vec::new();

        // Iterate ALL attributes on this field, collecting #[clean(...)] in order
        for attr_data in &field_meta.attributes {
            let attr = TypedAttribute::new(attr_data, &ctx.gcx.interner);
            if attr.name() != "clean" {
                continue;
            }

            // Check positional arg for keyword ops: trim, lower, upper, slug
            if let Some(keyword) = attr_data.first_positional_string(&ctx.gcx.interner) {
                match keyword {
                    "trim" => ops.push(CleanOp::Trim),
                    "lower" => ops.push(CleanOp::Lower),
                    "upper" => ops.push(CleanOp::Upper),
                    "slug" => ops.push(CleanOp::Slug),
                    _ => {} // unknown keyword, skip
                }
                continue;
            }

            // Named args — independent checks so one attribute can carry multiple ops
            if let Some(val) = attr.string("default") {
                ops.push(CleanOp::Default(val.to_string()));
            }
            if let Some(val) = attr.string("to_null") {
                ops.push(CleanOp::ToNull(val.to_string()));
            }
            if let Some(n) = attr.int("min") {
                ops.push(CleanOp::Min(n));
            }
            if let Some(n) = attr.int("max") {
                ops.push(CleanOp::Max(n));
            }
            if let Some(pattern) = attr.string("replace") {
                let replacement = attr.string("with").unwrap_or("");
                ops.push(CleanOp::Replace {
                    pattern: pattern.to_string(),
                    replacement: replacement.to_string(),
                });
            }
        }

        if !ops.is_empty() {
            result.push((field_name.to_string(), ops));
        }
    }

    result
}

fn apply_clean_ops(field_name: &str, ops: &[CleanOp]) -> Expr {
    let mut expr = col(field_name);
    for op in ops {
        expr = match op {
            CleanOp::Trim => crate::string::trim(expr),
            CleanOp::Lower => crate::string::lower(expr),
            CleanOp::Upper => crate::string::upper(expr),
            CleanOp::Slug => crate::string::slug(expr),
            CleanOp::Default(v) => {
                when(expr.clone().is_null())
                    .then(lit(v.clone()))
                    .otherwise(expr)
            }
            CleanOp::ToNull(s) => {
                when(expr.clone().eq(lit(s.clone())))
                    .then(lit(NULL))
                    .otherwise(expr)
            }
            CleanOp::Min(n) => {
                when(expr.clone().lt(lit(*n)))
                    .then(lit(NULL))
                    .otherwise(expr)
            }
            CleanOp::Max(n) => {
                when(expr.clone().gt(lit(*n)))
                    .then(lit(NULL))
                    .otherwise(expr)
            }
            CleanOp::Replace { pattern, replacement } => {
                expr.str().replace_all(lit(pattern.clone()), lit(replacement.clone()), false)
            }
        };
    }
    expr.alias(field_name)
}

fn take_frame(args: Vec<Value>, label: &str) -> Result<LazyFrame, FossilError> {
    let loc = fossil_lang::ast::Loc::generated();
    let input = args
        .into_iter()
        .next()
        .ok_or_else(|| FossilError::evaluation(format!("{label} requires a frame argument"), loc))?;
    input.into_frame()
        .ok_or_else(|| FossilError::evaluation(format!("{label} expects a Frame"), loc))
}

struct CleanFunction {
    type_name: Symbol,
}

impl FunctionImpl for CleanFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty], t_ty))
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut frame = take_frame(args, "clean")?;
        let field_ops = extract_clean_ops(ctx, self.type_name);

        let schema = frame.collect_schema()
            .map_err(|e| FossilError::evaluation(
                format!("Failed to collect schema for clean: {}", e),
                fossil_lang::ast::Loc::generated(),
            ))?;

        // Build a map of field_name → ops for quick lookup
        let ops_map: std::collections::HashMap<&str, &Vec<CleanOp>> =
            field_ops.iter().map(|(name, ops)| (name.as_str(), ops)).collect();

        let select_exprs: Vec<Expr> = schema
            .iter_names()
            .map(|name| {
                if let Some(ops) = ops_map.get(name.as_str()) {
                    apply_clean_ops(name, ops)
                } else {
                    col(name.clone()).alias(name.clone())
                }
            })
            .collect();

        Ok(Value::Frame(frame.select(select_exprs)))
    }
}
