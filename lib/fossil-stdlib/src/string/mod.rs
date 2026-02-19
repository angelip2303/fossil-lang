use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

fn take_expr(value: Value, label: &str) -> Result<Expr, FossilError> {
    let loc = fossil_lang::ast::Loc::generated();
    match value {
        Value::Expr(e) => Ok(e),
        _ => Err(FossilError::evaluation(
            format!("{label} expects a string expression"),
            loc,
        )),
    }
}

/// `String.extract(input, pattern)` — extracts first capture group from a regex pattern
pub struct StringExtractFunction;

impl FunctionImpl for StringExtractFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s, s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.extract")?;
        let pattern = take_expr(args.next().unwrap(), "String.extract")?;
        Ok(Value::Expr(input.str().extract(pattern, 1)))
    }
}

/// `String.replace(input, pattern, replacement)` — replaces first match of a regex pattern
pub struct StringReplaceFunction;

impl FunctionImpl for StringReplaceFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s, s, s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.replace")?;
        let pattern = take_expr(args.next().unwrap(), "String.replace")?;
        let replacement = take_expr(args.next().unwrap(), "String.replace")?;
        Ok(Value::Expr(input.str().replace(pattern, replacement, false)))
    }
}

/// `String.replace_all(input, pattern, replacement)` — replaces all matches of a regex pattern
pub struct StringReplaceAllFunction;

impl FunctionImpl for StringReplaceAllFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s, s, s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.replace_all")?;
        let pattern = take_expr(args.next().unwrap(), "String.replace_all")?;
        let replacement = take_expr(args.next().unwrap(), "String.replace_all")?;
        Ok(Value::Expr(
            input.str().replace_all(pattern, replacement, false),
        ))
    }
}
