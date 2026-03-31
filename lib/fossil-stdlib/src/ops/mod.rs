pub mod anon;
pub mod clean;

use std::sync::Arc;

use polars::prelude::*;

use fossil_lang::context::global::TypeInfo;
use fossil_lang::context::metadata::AttributeData;
use fossil_lang::context::{DefId, DefKind, Interner, Symbol};
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};

/// A column-level transformation applied via Polars expressions.
pub trait ColumnOp: Send + Sync + std::fmt::Debug {
    fn apply(&self, expr: Expr) -> Expr;
}

pub fn resolve_type_def_id(ctx: &RuntimeContext, type_name: Symbol) -> Option<DefId> {
    ctx.gcx
        .definitions
        .find_by_symbol(type_name, |k| matches!(k, DefKind::Type))
        .map(|d| d.id())
}

pub fn take_frame(args: Vec<Value>, label: &str) -> Result<LazyFrame, FossilError> {
    let loc = fossil_lang::ast::Loc::generated();
    let input = args
        .into_iter()
        .next()
        .ok_or_else(|| FossilError::evaluation(format!("{label} requires a frame argument"), loc))?;
    input
        .into_frame()
        .ok_or_else(|| FossilError::evaluation(format!("{label} expects a Frame"), loc))
}

pub type ParseAttr = fn(&AttributeData, &Interner) -> Vec<Box<dyn ColumnOp>>;

pub fn extract_ops(
    ctx: &RuntimeContext,
    type_name: Symbol,
    namespace: &str,
    parse: ParseAttr,
) -> Vec<(String, Vec<Box<dyn ColumnOp>>)> {
    let Some(type_def_id) = resolve_type_def_id(ctx, type_name) else {
        return Vec::new();
    };
    let Some(metadata) = ctx.gcx.type_metadata.get(&type_def_id) else {
        return Vec::new();
    };
    let Some(ns_sym) = ctx.gcx.interner.lookup(namespace) else {
        return Vec::new();
    };

    let mut result = Vec::new();

    for (field_sym, field_meta) in &metadata.field_metadata {
        let field_name = ctx.gcx.interner.resolve(*field_sym).to_string();
        let mut ops = Vec::new();

        for attr_data in &field_meta.attributes {
            if attr_data.name != ns_sym {
                continue;
            }
            ops.extend(parse(attr_data, &ctx.gcx.interner));
        }

        if !ops.is_empty() {
            result.push((field_name, ops));
        }
    }

    result
}

pub fn apply_ops(field_name: &str, ops: &[Box<dyn ColumnOp>]) -> Expr {
    let mut expr = col(field_name);
    for op in ops {
        expr = op.apply(expr);
    }
    expr.alias(field_name)
}

/// Generic function that applies [`ColumnOp`]s from attribute metadata.
/// Used by both `clean()` and `anonymize()`.
pub struct TransformFunction {
    pub type_name: Symbol,
    pub namespace: &'static str,
    pub label: &'static str,
    pub parse: ParseAttr,
}

impl FunctionImpl for TransformFunction {
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

    fn call(&self, args: Vec<Value>, _type_args: &[DefId], ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let frame = take_frame(args, self.label)?;
        let field_ops = extract_ops(ctx, self.type_name, self.namespace, self.parse);

        let exprs: Vec<Expr> = field_ops
            .iter()
            .map(|(name, ops)| apply_ops(name, ops))
            .collect();

        if exprs.is_empty() {
            return Ok(Value::Frame(frame));
        }

        Ok(Value::Frame(frame.with_columns(exprs)))
    }
}

pub fn make_module_generator(
    namespace: &'static str,
    build_functions: fn(Symbol) -> Vec<FunctionDef>,
) -> Arc<dyn Fn(&TypeInfo) -> Option<ModuleSpec> + Send + Sync> {
    Arc::new(move |info: &TypeInfo| {
        let ns_sym = info.interner.lookup(namespace)?;
        let has_ns = info
            .fields
            .iter()
            .any(|f| f.attrs.iter().any(|a| a.name == ns_sym));

        if !has_ns {
            return None;
        }

        Some(ModuleSpec {
            functions: build_functions(info.name),
        })
    })
}
