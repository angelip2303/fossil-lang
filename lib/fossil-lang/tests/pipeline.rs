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
fn test_simple_let() {
    let src = "let x = 42";
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile simple let: {:?}", result.err());
}

#[test]
fn test_function_identity() {
    let src = "let identity = fn (x) -> x";
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile identity function: {:?}", result.err());
}

#[test]
fn test_record_basic() {
    let src = r#"let person = { name = "Alice", age = 30 }"#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile record: {:?}", result.err());
}

#[test]
fn test_field_access() {
    let src = r#"let person = { name = "Alice", age = 30 } let name = person.name"#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile field access: {:?}", result.err());
}

#[test]
fn test_pipe_operator() {
    let src = "let inc = fn (x) -> x let result = 5 |> inc";
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile pipe operator: {:?}", result.err());
}

#[test]
fn test_list() {
    let src = "let numbers = [1, 2, 3]";
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile list: {:?}", result.err());
}

#[test]
fn test_function_application() {
    let src = "let identity = fn (x) -> x let result = identity(42)";
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile function application: {:?}", result.err());
}

#[test]
fn test_nested_field_access() {
    let src = r#"let data = { person = { name = "Bob" } } let name = data.person"#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile nested field access: {:?}", result.err());
}

#[test]
fn test_lambda_with_field_access() {
    let src = r#"let get_name = fn (p) -> p.name let person = { name = "Charlie" } let result = get_name(person)"#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile lambda with field access: {:?}", result.err());
}
