use fossil_lang::phases::typecheck::TypeVarGen;
use polars::prelude::*;

use fossil_lang::ast::{Ast, Polytype, PrimitiveType, Type, TypeVar};
use fossil_lang::error::RuntimeError;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::FunctionImpl;

pub struct RandomNextFunction;

impl FunctionImpl for RandomNextFunction {
    fn signature(&self, ast: &mut Ast, tvg: &mut TypeVarGen) -> Polytype {
        let input = vec![
            ast.types.alloc(Type::Primitive(PrimitiveType::Int)),
            ast.types.alloc(Type::Primitive(PrimitiveType::Int)),
        ];

        let output = ast.types.alloc(Type::Primitive(PrimitiveType::Int));

        let ty = ast.types.alloc(Type::Function(input, output));

        Polytype::mono(ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match (&args[0], &args[1]) {
            (Value::Int(min), Value::Int(max)) => {
                use rand::Rng;
                Ok(Value::Int(rand::rng().random_range(*min..=*max)))
            }
            _ => unreachable!("Type checker ensures correct types"), // TODO: I don't like this
        }
    }
}

pub struct CsvWriteFunction;

impl FunctionImpl for CsvWriteFunction {
    fn signature(&self, ast: &mut Ast, tvg: &mut TypeVarGen) -> Polytype {
        let var = tvg.next();

        let input = vec![
            ast.types.alloc(Type::Var(var)),
            ast.types.alloc(Type::Primitive(PrimitiveType::String)),
        ];

        let output = ast.types.alloc(Type::Primitive(PrimitiveType::Int));

        let ty = ast.types.alloc(Type::Function(input, output));

        Polytype::poly(vec![var], ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let lf = match &args[0] {
            Value::LazyFrame(lf) => lf.clone(),
            _ => unreachable!("Type checker ensures correct types"), // TODO: I don't like this
        };

        let path = match &args[1] {
            Value::String(s) => s.as_ref(),
            _ => unreachable!(), // TODO: I don't like this
        };

        let mut df = lf.collect()?;
        let file = std::fs::File::create(path)?;
        CsvWriter::new(file).finish(&mut df)?;

        Ok(Value::Bool(true)) // TODO: this is a placeholder
    }
}
