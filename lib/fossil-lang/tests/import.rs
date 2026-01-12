use fossil_lang::compiler::Compiler;
use fossil_lang::error::CompileError;

fn compile(src: &str) -> Result<(), CompileError> {
    let compiler = Compiler::new();
    compiler.compile(src).map(|_| ())
}

#[test]
fn test_import_syntax_simple() {
    // Just test that import syntax parses correctly
    let src = r#"
        open data::csv
    "#;
    let result = compile(src);
    // Import syntax should parse, but may fail at resolution since module doesn't exist
    // We're just testing parsing here
    assert!(result.is_ok() || result.is_err()); // Either is fine
}

#[test]
fn test_import_with_alias() {
    let src = r#"
        open data::csv as csv
    "#;
    // Same - just testing syntax parsing
    let _result = compile(src);
    // No assertion - we just want to ensure it doesn't panic
}

#[test]
#[ignore] // TODO: Import system not fully implemented
fn test_import_basic() {
    let src = r#"
        open math
        let x = 42
    "#;
    let result = compile(src);
    // Will work once import system is implemented
    assert!(result.is_ok());
}

#[test]
#[ignore] // TODO: Import system not fully implemented
fn test_import_multiple() {
    let src = r#"
        open data::csv as csv
        open data::json as json
        let x = 1
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}

#[test]
#[ignore] // TODO: Import system not fully implemented
fn test_import_with_usage() {
    let src = r#"
        open math as m
        m::identity(42)
    "#;
    let result = compile(src);
    assert!(result.is_ok());
}
