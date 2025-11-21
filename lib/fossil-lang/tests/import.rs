use std::sync::Arc;

use fossil_lang::ast::{Ast, Polytype, PrimitiveType, Type};
use fossil_lang::compiler::Compiler;
use fossil_lang::error::RuntimeError;
use fossil_lang::module::ModuleRegistry;
use fossil_lang::phases::typecheck::TypeVarGen;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::FunctionImpl;

struct IdentityFunction;

impl FunctionImpl for IdentityFunction {
    fn signature(&self, ast: &mut Ast, _: &mut TypeVarGen) -> Polytype {
        let input = vec![ast.types.alloc(Type::Primitive(PrimitiveType::Int))];
        let output = ast.types.alloc(Type::Primitive(PrimitiveType::Int));
        let ty = ast.types.alloc(Type::Function(input, output));
        Polytype::mono(ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match &args[0] {
            Value::Int(i) => Ok(Value::Int(*i)),
            _ => unreachable!(),
        }
    }
}

struct ToStringFunction;

impl FunctionImpl for ToStringFunction {
    fn signature(&self, ast: &mut Ast, _: &mut TypeVarGen) -> Polytype {
        let input = vec![ast.types.alloc(Type::Primitive(PrimitiveType::Int))];
        let output = ast.types.alloc(Type::Primitive(PrimitiveType::String));
        let ty = ast.types.alloc(Type::Function(input, output));
        Polytype::mono(ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match &args[0] {
            Value::Int(i) => Ok(Value::String(Arc::from(i.to_string()))),
            _ => unreachable!(),
        }
    }
}

#[test]
fn basic_import() {
    let src = r#"
        open math
        math::identity(42)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("identity", Arc::new(IdentityFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn basic_import_with_alias() {
    let src = r#"
        open math as m
        m::identity(42)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("identity", Arc::new(IdentityFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_multiple() {
    let src = r#"
        open math as m
        open string as s
        s::toString(m::identity(42))
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("identity", Arc::new(IdentityFunction))
        .done();

    registry
        .module("string")
        .function("toString", Arc::new(ToStringFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_undefined_module() {
    let src = r#"
        open undefined as u
        u::something(42) // this is an undefined module
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_err());
}

#[test]
fn import_use_undefined_function() {
    let src = r#"
        open math as m
        m::undefined(42) // this is an undefined function, but the module exists
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("identity", Arc::new(IdentityFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_err());
}

#[test]
fn import_local_same_function_name() {
    let src = r#"
        open math as m
        let identity = fn (x) -> x
        m::identity(42) // both should work, local doesn't shadow module
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("identity", Arc::new(IdentityFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}
