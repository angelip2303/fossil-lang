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

/// `String.trim(input)` — strips leading/trailing whitespace
pub struct StringTrimFunction;

impl FunctionImpl for StringTrimFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.trim")?;
        Ok(Value::Expr(input.str().strip_chars(lit(""))))
    }
}

/// `String.upper(input)` — converts to uppercase
pub struct StringUpperFunction;

impl FunctionImpl for StringUpperFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.upper")?;
        Ok(Value::Expr(input.str().to_uppercase()))
    }
}

/// `String.lower(input)` — converts to lowercase
pub struct StringLowerFunction;

impl FunctionImpl for StringLowerFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.lower")?;
        Ok(Value::Expr(input.str().to_lowercase()))
    }
}

/// `String.length(input)` — returns character count
pub struct StringLengthFunction;

impl FunctionImpl for StringLengthFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        let i = ir.int_type();
        Polytype::mono(ir.fn_type(vec![s], i))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.length")?;
        Ok(Value::Expr(input.str().len_chars()))
    }
}

/// `String.contains(input, pattern)` — checks if string contains literal pattern
pub struct StringContainsFunction;

impl FunctionImpl for StringContainsFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        let b = ir.bool_type();
        Polytype::mono(ir.fn_type(vec![s, s], b))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.contains")?;
        let pattern = take_expr(args.next().unwrap(), "String.contains")?;
        Ok(Value::Expr(input.str().contains_literal(pattern)))
    }
}

/// `String.starts_with(input, prefix)` — checks if string starts with prefix
pub struct StringStartsWithFunction;

impl FunctionImpl for StringStartsWithFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        let b = ir.bool_type();
        Polytype::mono(ir.fn_type(vec![s, s], b))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.starts_with")?;
        let prefix = take_expr(args.next().unwrap(), "String.starts_with")?;
        Ok(Value::Expr(input.str().starts_with(prefix)))
    }
}

/// `String.ends_with(input, suffix)` — checks if string ends with suffix
pub struct StringEndsWithFunction;

impl FunctionImpl for StringEndsWithFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        let b = ir.bool_type();
        Polytype::mono(ir.fn_type(vec![s, s], b))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.ends_with")?;
        let suffix = take_expr(args.next().unwrap(), "String.ends_with")?;
        Ok(Value::Expr(input.str().ends_with(suffix)))
    }
}

/// `String.slug(input)` — lowercase + remove non-alphanumeric characters
pub struct StringSlugFunction;

impl FunctionImpl for StringSlugFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let s = ir.string_type();
        Polytype::mono(ir.fn_type(vec![s], s))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args = args.into_iter();
        let input = take_expr(args.next().unwrap(), "String.slug")?;
        // lowercase then replace non-alphanumeric runs with ""
        let lowered = input.str().to_lowercase();
        Ok(Value::Expr(
            lowered.str().replace_all(lit(r"[^a-z0-9]+"), lit(""), false),
        ))
    }
}

/// `String.concat(a, b)` — concatenates two strings
pub struct StringConcatFunction;

impl FunctionImpl for StringConcatFunction {
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
        let a = take_expr(args.next().unwrap(), "String.concat")?;
        let b = take_expr(args.next().unwrap(), "String.concat")?;
        Ok(Value::Expr(concat_str([a, b], "", true)))
    }
}
