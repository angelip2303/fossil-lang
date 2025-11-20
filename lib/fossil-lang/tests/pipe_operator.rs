use fossil_lang::ast::{Ast, Polytype, PrimitiveType, Type};
use fossil_lang::compiler::Compiler;
use fossil_lang::error::RuntimeError;
use fossil_lang::module::ModuleRegistry;
use fossil_lang::phases::typecheck::TypeVarGen;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::FunctionImpl;
use std::sync::Arc;

// ============================================================================
// Mock Functions for Testing
// ============================================================================

struct AddOneFunction;

impl FunctionImpl for AddOneFunction {
    fn signature(&self, ast: &mut Ast, _: &mut TypeVarGen) -> Polytype {
        let input = vec![ast.types.alloc(Type::Primitive(PrimitiveType::Int))];
        let output = ast.types.alloc(Type::Primitive(PrimitiveType::Int));
        let ty = ast.types.alloc(Type::Function(input, output));
        Polytype::mono(ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match &args[0] {
            Value::Int(i) => Ok(Value::Int(i + 1)),
            _ => unreachable!(),
        }
    }
}

struct DoubleFunction;

impl FunctionImpl for DoubleFunction {
    fn signature(&self, ast: &mut Ast, _: &mut TypeVarGen) -> Polytype {
        let input = vec![ast.types.alloc(Type::Primitive(PrimitiveType::Int))];
        let output = ast.types.alloc(Type::Primitive(PrimitiveType::Int));
        let ty = ast.types.alloc(Type::Function(input, output));
        Polytype::mono(ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match &args[0] {
            Value::Int(i) => Ok(Value::Int(i * 2)),
            _ => unreachable!(),
        }
    }
}

struct ToStringFunction;

impl FunctionImpl for ToStringFunction {
    fn signature(&self, ast: &mut Ast, tvg: &mut TypeVarGen) -> Polytype {
        let var = tvg.next();
        let input = vec![ast.types.alloc(Type::Var(var))];
        let output = ast.types.alloc(Type::Primitive(PrimitiveType::String));
        let ty = ast.types.alloc(Type::Function(input, output));
        Polytype::poly(vec![var], ty)
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let s = match &args[0] {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::Bool(b) => b.to_string(),
            _ => "<value>".to_string(),
        };
        Ok(Value::String(Arc::from(s)))
    }
}

// ============================================================================
// Pipe Operator Tests
// ============================================================================

#[test]
fn pipe_simple() {
    let src = r#"
        let identity = fn (x) -> x
        42 |> identity
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_chain() {
    let src = r#"
        let identity = fn (x) -> x
        42 |> identity |> identity |> identity
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_with_module_function() {
    let src = r#"
        42 |> math::addOne
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_complex_chain() {
    let src = r#"
        42 |> math::addOne |> math::double |> util::toString
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .function("double", Arc::new(DoubleFunction))
        .done();

    registry
        .module("util")
        .function("toString", Arc::new(ToStringFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_with_lambda() {
    let src = r#"
        let addOne = fn (x) -> x
        42 |> addOne
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_with_nested_expressions() {
    let src = r#"
        let addOne = fn (x) -> x
        (21 |> addOne) |> addOne
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn pipe_type_error() {
    let src = r#"
        "hello" |> math::addOne
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_err());
}

// ============================================================================
// Import System Tests
// ============================================================================

#[test]
fn import_simple() {
    let src = r#"
        open math as m
        m::addOne(42)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_multiple() {
    let src = r#"
        open math as m
        open util as u
        u::toString(m::addOne(42))
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    registry
        .module("util")
        .function("toString", Arc::new(ToStringFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_with_pipe() {
    let src = r#"
        open math as m
        42 |> m::addOne |> m::double
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .function("double", Arc::new(DoubleFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_undefined_module() {
    let src = r#"
        open undefined as u
        u::something(42)
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_err());
}

#[test]
fn import_use_undefined_function() {
    let src = r#"
        open math as m
        m::undefined(42)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_err());
}

#[test]
fn import_with_local_shadowing() {
    let src = r#"
        open math as m
        let addOne = fn (x) -> x
        m::addOne(42)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    // Both should work - local doesn't shadow module
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn import_in_nested_scope() {
    let src = r#"
        let f = fn () -> {
            // Imports in nested scopes - design decision
            // This test assumes imports are only allowed at top level
            open math as m
            m::addOne(42)
        }
        f()
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .done();

    let compiler = Compiler::new(&registry);
    // This should fail if imports are only allowed at top level
    // Adjust based on your design decision
    assert!(compiler.compile(src).is_err());
}

// ============================================================================
// Combined Feature Tests
// ============================================================================

#[test]
fn import_and_pipe_complex() {
    let src = r#"
        open math as m
        open util as u

        let process = fn (x) ->
            x |> m::addOne |> m::double |> u::toString

        process(21)
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .function("double", Arc::new(DoubleFunction))
        .done();

    registry
        .module("util")
        .function("toString", Arc::new(ToStringFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}

#[test]
fn full_pipeline_example() {
    let src = r#"
        open math as m
        open util as u

        let numbers = [1, 2, 3]
        let process = fn (x) ->
            x |> m::addOne |> m::double

        // This would ideally use a map function
        [process(1), process(2), process(3)]
    "#;

    let mut registry = ModuleRegistry::default();
    registry
        .module("math")
        .function("addOne", Arc::new(AddOneFunction))
        .function("double", Arc::new(DoubleFunction))
        .done();

    let compiler = Compiler::new(&registry);
    assert!(compiler.compile(src).is_ok());
}
