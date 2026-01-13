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
fn test_polymorphic_field_access() {
    // A function that accesses .name field should work with any record that has .name
    let src = r#"
        let get_name = fn (p) -> p.name
        let person = { name = "Alice", age = 30 }
        let result = get_name(person)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_multiple_field_access() {
    // A function accessing multiple fields should work with records containing those fields
    let src = r#"
        let get_both = fn (p) -> { n = p.name, a = p.age }
        let person = { name = "Bob", age = 25, city = "NYC" }
        let result = get_both(person)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_partial_records() {
    // A function that only uses .x should accept records with more fields
    let src = r#"
        let extract = fn (r) -> r.x
        let full = { x = 1, y = 2, z = 3 }
        let result = extract(full)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_nested_field_access() {
    // Test accessing fields from records in sequence
    let src = r#"
        let get_x = fn (r) -> r.x
        let get_y = fn (r) -> r.y
        let point = { x = 10, y = 20 }
        let x_val = get_x(point)
        let y_val = get_y(point)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_identity_with_field_access() {
    // The identity function should work with field access
    let src = r#"
        let identity = fn (x) -> x
        let person = { name = "Charlie", age = 35 }
        let same = identity(person)
        let name = same.name
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_record_with_different_fields() {
    // Different records with different fields should work independently
    let src = r#"
        let person = { name = "Alice", age = 30 }
        let point = { x = 1, y = 2 }
        let name = person.name
        let x = point.x
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_function_composition_with_records() {
    // Test composing functions that access different fields
    let src = r#"
        let get_name = fn (p) -> p.name
        let get_age = fn (p) -> p.age
        let person = { name = "David", age = 40, city = "LA" }
        let name = get_name(person)
        let age = get_age(person)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_direct_field_access_on_concrete_record() {
    // Direct field access on concrete records should still work
    let src = r#"
        let person = { name = "Eve", age = 28 }
        let name = person.name
        let age = person.age
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}

#[test]
fn test_undefined_field_should_fail() {
    // Accessing a field that doesn't exist should fail
    let src = r#"
        let person = { name = "Frank", age = 50 }
        let missing = person.nonexistent
    "#;
    let result = compile(src);
    assert!(result.is_err(), "Should have failed with undefined field");
}

#[test]
fn test_field_access_through_function_parameter() {
    // Field access on a function parameter should work polymorphically
    let src = r#"
        let use_name = fn (obj) -> obj.name
        let thing1 = { name = "Thing1", kind = "A" }
        let thing2 = { name = "Thing2", value = 42 }
        let n1 = use_name(thing1)
        let n2 = use_name(thing2)
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());
}
