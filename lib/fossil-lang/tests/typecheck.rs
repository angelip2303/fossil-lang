use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::CompileError;
use fossil_lang::passes::ThirProgram;

fn compile(src: &str) -> Result<ThirProgram, CompileError> {
    let compiler = Compiler::new();
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    compiler.compile(input)
}

#[test]
fn integer_variable() {
    let src = r#"
        let x = 42
        x
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn string_variable() {
    let src = r#"
        let x = "Hello World!"
        x
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn boolean_variable() {
    let src = r#"
        let x = true
        x
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn record_variable() {
    let src = r#"
        let x = {name = "John", age = 30}
        x
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn record_fields_from_variables() {
    let src = r#"
        let name = "John"
        let age = 30
        {name = name, age = age}
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn record_fields_from_variables_mixed() {
    let src = r#"
        let name = "John"
        let age = 30
        {name = name, age = age, is_student = false}
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn nested_records() {
    let src = r#"
        let person = {name = "John", age = 30}
        let address = {street = "123 Main St", city = "Anytown"}
        {person = person, address = address}
    "#;
    let result = compile(src);
    // Now valid - nested records are supported
    assert!(result.is_ok());
}

#[test]
fn record_with_unit_field() {
    let src = r#"
        {person = (), address = "Elm Street"}
    "#;
    let result = compile(src);
    // Now valid - records can have unit fields
    assert!(result.is_ok());
}

#[test]
fn record_with_function_field() {
    let src = r#"
        {person = (), address = "Elm Street", greet = fn() -> "Hello"}
    "#;
    let result = compile(src);
    // Now valid - records can have function fields
    assert!(result.is_ok());
}

#[test]
fn record_with_function_call_field() {
    let src = r#"
        let greet = fn () -> "Hello"
        {person = "John", address = "Elm Street", greet = greet()}
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn concat_records() {
    let src = r#"
        let x = {name = "John", age = 30}
        let y = {name = "Jane", age = 25}
        [x, y]
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn concat_records_mixed() {
    let src = r#"
        let x = {name = "John", age = 30}
        let y = {name = "Jane", age = 25}
        [x, y, {name = "Alice", age = 28}]
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn concat_records_with_mismatched_fields() {
    let src = r#"
        let x = {name = "John", age = 30}
        let y = {name = "Alice", age = 28, is_student = true}
        [x, y]
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn concat_records_with_mismatched_types() {
    let src = r#"
        let x = {name = "John", age = 30}
        let y = {name = "Alice", age = "28"}
        [x, y]
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn list_variable() {
    let src = r#"
        let x = [1, 2, 3]
        x
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn list_from_variables() {
    let src = r#"
        let x = 1
        let y = 2
        let z = 3
        [x, y, z]
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn list_from_variables_mixed() {
    let src = r#"
        let x = 1
        let y = 2
        let z = 3
        [x, y, z, 4, 5, 6]
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn list_from_mismatched_types() {
    let src = r#"
        let x = 1
        let y = "2"
        [x, y, z]
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn list_of_unit() {
    let src = r#"
        [(), (), ()]
    "#;
    let result = compile(src);
    // Now valid - lists of unit are allowed
    assert!(result.is_ok());
}

#[test]
fn empty_list() {
    let src = r#"
        []
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn list_of_functions() {
    let src = r#"
        [fn (x) -> x, fn (x) -> x, fn (x) -> x]
    "#;
    let result = compile(src);
    // Now valid - lists of functions are supported
    assert!(result.is_ok());
}

#[test]
fn list_of_function_call() {
    let src = r#"
        let greet = fn () -> "Hello"
        [greet()]
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn unit() {
    let src = r#"
        ()
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function() {
    let src = r#"
        let identity = fn (x) -> x
        identity(42)
        identity("Hello World!")
        identity(true)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_with_nested_call() {
    let src = r#"
        let identity = fn (x) -> x
        identity(identity(42))
        identity(identity("Hello World!"))
        identity(identity(true))
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_with_deeply_nested_call() {
    let src = r#"
        let identity = fn (x) -> x
        let id = identity(identity)
        id(42)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_with_multiple_arguments() {
    let src = r#"
        let record = fn (x, y) -> {x = x, y = y}
        record(1, 2)
        record("Hello ", "World!")
        record(true, false)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_with_no_arguments() {
    let src = r#"
        let unit = fn () -> ()
        unit()
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_that_returns_a_function() {
    let src = r#"
        let greeeeet = fn () -> fn () -> "Hello"
        greeeeet()
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_with_undefined_parameters() {
    let src = r#"
        let undefined = fn (x) -> y
        undefined(1)
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn function_call_with_less_arguments() {
    let src = r#"
        let greet = fn (x, y) -> "Hello"
        greet(1)
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn function_call_with_more_arguments() {
    let src = r#"
        let greet = fn (x, y) -> "Hello"
        greet(1, 2, 3)
    "#;
    let result = compile(src);
    assert!(result.is_err());
}

#[test]
fn function_captures_environment() {
    let src = r#"
        let x = 1
        let greet = fn (y) -> "Hello"
        greet(x)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn function_refers_to_environment() {
    let src = r#"
        let x = "Hello"
        let greet = fn () -> x
        greet()
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn to_list_function_returns_valid() {
    let src = r#"
        let to_list = fn (x) -> [x]
        to_list(42)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn to_list_function_returns_list_of_lists() {
    let src = r#"
        let to_list = fn (x) -> [x]
        to_list([1, 2, 3])
    "#;
    let result = compile(src);
    // Now valid - nested lists are supported
    assert!(result.is_ok());
}

#[test]
fn to_record_function_returns_valid() {
    let src = r#"
        let to_record = fn (x) -> {x = x}
        to_record(42)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
fn to_record_function_returns_nested_record() {
    let src = r#"
        let to_record = fn (x) -> {x = x}
        to_record({x = 42})
    "#;
    let result = compile(src);
    // Now valid - nested records are supported
    assert!(result.is_ok());
}
