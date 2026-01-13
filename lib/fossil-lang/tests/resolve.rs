use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::CompileError;

fn compile(src: &str) -> Result<(), CompileError> {
    let compiler = Compiler::new();
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    compiler.compile(input).map(|_| ())
}

#[test]
fn resolve_undefined_variable() {
    let src = r#"
        y
    "#;
    assert!(compile(src).is_err());
}

#[test]
fn resolve_simple_variable() {
    let src = r#"
        let x = 42
        x
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_simple_variable_defined_after_use() {
    let src = r#"
        x
        let x = 42
    "#;
    assert!(compile(src).is_err());
}

#[test]
fn resolve_function() {
    let src = r#"
        let f = fn () -> ()
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
#[ignore] // TODO: Forward references not yet supported
fn resolve_function_defined_after_use() {
    let src = r#"
        f()
        let f = fn () -> ()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_function_captures() {
    let src = r#"
        let x = 42
        let f = fn () -> x
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_multiple_variables() {
    let src = r#"
        let x = 1
        let y = 2
        let z = 3
        [x, y, z]
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_shadowing() {
    let src = r#"
        let x = 42
        let f = fn () -> {
            let x = "hello"
            x
        }
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_nested_scopes() {
    let src = r#"
        let x = 1
        let f = fn () -> {
            let y = 2
            let g = fn () -> {
                let z = 3
                [x, y, z]
            }
            g()
        }
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_parameter_shadows_outer() {
    let src = r#"
        let x = 1
        let f = fn (x) -> x
        f(42)
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_function_parameter() {
    let src = r#"
        let f = fn (x) -> x
        f(42)
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_multiple_parameters() {
    let src = r#"
        let f = fn (x, y, z) -> [x, y, z]
        f(1, 2, 3)
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_undefined_in_function_body() {
    let src = r#"
        let f = fn (x) -> y
        f(42)
    "#;
    assert!(compile(src).is_err());
}

#[test]
fn resolve_parameter_used_in_nested_function() {
    let src = r#"
        let f = fn (x) -> {
            let y = x
            y
        }
        f(42)
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_primitive_types() {
    let src = r#"
        type MyInt = int
        type MyString = string
        type MyBool = bool
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_type_alias() {
    let src = r#"
        type Age = int
        let age = 30
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_undefined_type() {
    let src = r#"
        type MyType = UndefinedType
    "#;
    assert!(compile(src).is_err());
}

#[test]
fn resolve_record_type() {
    let src = r#"
        type Person = {name: string, age: int}
    "#;
    assert!(compile(src).is_ok());
}

#[test]
#[ignore] // TODO: Type resolution issues
fn resolve_function_type() {
    let src = r#"
        type Mapper = (int) -> string
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_list_type() {
    let src = r#"
        type IntList = [int]
    "#;
    assert!(compile(src).is_ok());
}

#[test]
#[ignore] // TODO: Type resolution issues
fn resolve_complex_type() {
    let src = r#"
        type ComplexType = {
            name: string,
            scores: [int],
            formatter: (int) -> string
        }
    "#;
    assert!(compile(src).is_ok());
}

#[test]
#[ignore] // TODO: Forward references not yet supported
fn resolve_forward_reference() {
    let src = r#"
        let f = fn () -> g()
        let g = fn () -> 42
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_self_reference_in_let() {
    let src = r#"
        let x = x
    "#;
    assert!(compile(src).is_err());
}

#[test]
fn resolve_list_with_variables() {
    let src = r#"
        let a = 1
        let b = 2
        let c = 3
        [a, b, c]
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_record_with_variables() {
    let src = r#"
        let name = "John"
        let age = 30
        {name = name, age = age}
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_nested_function_calls() {
    let src = r#"
        let identity = fn (x) -> x
        identity(identity(identity(42)))
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_empty_function() {
    let src = r#"
        let f = fn () -> ()
        f()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_unused_variable() {
    let src = r#"
        let x = 42
        let y = 10
        x
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_complex_nesting() {
    let src = r#"
        let outer = 1
        let f1 = fn () -> {
            let middle = 2
            let f2 = fn () -> {
                let inner = 3
                let f3 = fn () -> {
                    [outer, middle, inner]
                }
                f3()
            }
            f2()
        }
        f1()
    "#;
    assert!(compile(src).is_ok());
}

#[test]
fn resolve_multiple_definitions_same_name() {
    let src = r#"
        let x = 1
        let x = 2
        x
    "#;
    // Shadowing at the same scope level
    // This depends on your hoisting strategy
    // Most languages either allow it (last definition wins) or error
    // Adjust this test based on your design decision
}
