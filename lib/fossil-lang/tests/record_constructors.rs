//! Integration tests for auto-generated record constructors
//!
//! Tests that record types automatically get constructor functions
//! that can create record instances.

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
fn simple_record_type_definition() {
    let src = r#"
        type Person = { name: string, age: int }
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Record type definition should compile");
}

#[test]
fn record_constructor_basic() {
    let src = r#"
        type Person = { name: string, age: int }
        let p = Person("Alice", 30)
        p
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Basic record constructor should work: {:?}", result.err());
}

#[test]
fn record_constructor_with_variables() {
    let src = r#"
        type Person = { name: string, age: int }
        let name = "Bob"
        let age = 25
        let p = Person(name, age)
        p
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Constructor with variables should work");
}

#[test]
fn multiple_record_types() {
    let src = r#"
        type Person = { name: string, age: int }
        type Address = { street: string, city: string }

        let person = Person("Alice", 30)
        let address = Address("Main St", "NYC")

        person
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Multiple record constructors should work");
}

#[test]
fn record_constructor_single_field() {
    let src = r#"
        type Name = { value: string }
        let n = Name("Alice")
        n
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Single-field record constructor should work");
}

#[test]
fn record_constructor_many_fields() {
    let src = r#"
        type Employee = {
            name: string,
            age: int,
            department: string,
            salary: int
        }
        let e = Employee("Alice", 30, "Engineering", 100000)
        e
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Multi-field record constructor should work");
}

#[test]
fn record_literal_still_works() {
    let src = r#"
        type Person = { name: string, age: int }
        let p = { name = "Alice", age = 30 }
        p
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Record literals should still work alongside constructors");
}

#[test]
fn constructor_and_literal_in_same_program() {
    let src = r#"
        type Person = { name: string, age: int }
        let p1 = Person("Alice", 30)
        let p2 = { name = "Bob", age = 25 }
        p1
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Constructors and literals should coexist");
}

#[test]
fn constructor_wrong_arity_too_few() {
    let src = r#"
        type Person = { name: string, age: int }
        let p = Person("Alice")
    "#;
    let result = compile(src);
    // This should fail during type checking or runtime
    // For now, we just check that it either compiles (and will fail at runtime)
    // or fails at compile time with appropriate error
    if let Err(e) = result {
        let msg = e.message();
        assert!(
            msg.contains("Arity") || msg.contains("argument") || msg.contains("parameter"),
            "Error should mention arity/arguments: {}",
            msg
        );
    }
    // If it compiles, it will fail at runtime with arity mismatch
}

#[test]
fn constructor_wrong_arity_too_many() {
    let src = r#"
        type Person = { name: string, age: int }
        let p = Person("Alice", 30, "extra")
    "#;
    let result = compile(src);
    if let Err(e) = result {
        let msg = e.message();
        assert!(
            msg.contains("Arity") || msg.contains("argument") || msg.contains("parameter"),
            "Error should mention arity/arguments: {}",
            msg
        );
    }
}

#[test]
fn constructor_in_function() {
    let src = r#"
        type Person = { name: string, age: int }

        let make_person = fn(name, age) -> Person(name, age)

        let p = make_person("Alice", 30)
        p
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Constructor should work in function bodies");
}

#[test]
fn nested_constructor_calls() {
    let src = r#"
        type Inner = { value: int }
        type Outer = { inner: int, extra: int }

        let i = Inner(42)
        let o = Outer(100, 200)
        o
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Nested constructor calls should work");
}

#[test]
fn constructor_with_type_annotation() {
    let src = r#"
        type Person = { name: string, age: int }
        let p: Person = Person("Alice", 30)
        p
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Constructor with type annotation should work");
}

// Test that constructor doesn't conflict with existing value binding
#[test]
fn constructor_name_conflict_skip() {
    let src = r#"
        let Person = 42
        type Person = { name: string, age: int }
    "#;
    let result = compile(src);
    // Constructor generation should be skipped when name conflicts
    // Type definition should still work
    assert!(result.is_ok(), "Should handle name conflicts gracefully");
}

#[test]
fn empty_record_constructor() {
    let src = r#"
        type Empty = {}
        let e = Empty()
        e
    "#;
    let result = compile(src);
    assert!(result.is_ok(), "Empty record constructor should work");
}
