//! Multi-Module Integration Tests
//!
//! These tests verify that the module system works correctly with real files:
//! - File-based module loading
//! - Import resolution across modules
//! - Relative and absolute paths
//! - Topological ordering
//! - Circular dependency detection

use fossil_lang::compiler::{Compiler, CompilerInput};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_simple_two_file_project() {
    // Create temporary project directory
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil
    fs::write(
        root.join("main.fossil"),
        r#"
            open utils
            let result = 42
        "#,
    )
    .unwrap();

    // Create utils.fossil
    fs::write(
        root.join("utils.fossil"),
        r#"
            let helper = fn (x) -> x
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile two-file project: {:?}",
        result.err()
    );
}

#[test]
fn test_directory_compilation() {
    // Create temporary project directory
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil (entry point)
    fs::write(
        root.join("main.fossil"),
        r#"
            open helpers
            let x = 10
        "#,
    )
    .unwrap();

    // Create helpers.fossil
    fs::write(
        root.join("helpers.fossil"),
        r#"
            let add = fn (a, b) -> a
        "#,
    )
    .unwrap();

    // Compile the project directory
    let compiler = Compiler::new();
    let input = CompilerInput::Directory(root.to_path_buf());
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile project directory: {:?}",
        result.err()
    );
}

#[test]
fn test_nested_directory_structure() {
    // Create temporary project with nested structure
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create data directory
    fs::create_dir(root.join("data")).unwrap();

    // Create main.fossil
    fs::write(
        root.join("main.fossil"),
        r#"
            open data::csv
            let result = 42
        "#,
    )
    .unwrap();

    // Create data/csv.fossil
    fs::write(
        root.join("data/csv.fossil"),
        r#"
            let parse = fn (s) -> s
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile nested directory structure: {:?}",
        result.err()
    );
}

#[test]
#[ignore = "Relative imports (./utils) require parser support - not yet implemented"]
fn test_relative_imports_same_directory() {
    // Create temporary project
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil
    fs::write(
        root.join("main.fossil"),
        r#"
            open ./utils
            let x = 42
        "#,
    )
    .unwrap();

    // Create utils.fossil (same directory)
    fs::write(
        root.join("utils.fossil"),
        r#"
            let helper = fn () -> ()
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile with relative import (./): {:?}",
        result.err()
    );
}

#[test]
#[ignore = "Relative imports (../main) require parser support - not yet implemented"]
fn test_relative_imports_parent_directory() {
    // Create temporary project with nested structure
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create subdirectory
    fs::create_dir(root.join("sub")).unwrap();

    // Create main.fossil in root
    fs::write(
        root.join("main.fossil"),
        r#"
            let root_value = 42
        "#,
    )
    .unwrap();

    // Create sub/nested.fossil that imports from parent
    fs::write(
        root.join("sub/nested.fossil"),
        r#"
            open ../main
            let nested_value = 10
        "#,
    )
    .unwrap();

    // Compile from nested file
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("sub/nested.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile with parent import (../): {:?}",
        result.err()
    );
}

#[test]
fn test_chain_of_imports() {
    // Create temporary project with chain: main -> utils -> helpers
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil
    fs::write(
        root.join("main.fossil"),
        r#"
            open utils
            let main_value = 42
        "#,
    )
    .unwrap();

    // Create utils.fossil (imports helpers)
    fs::write(
        root.join("utils.fossil"),
        r#"
            open helpers
            let utils_value = 10
        "#,
    )
    .unwrap();

    // Create helpers.fossil (leaf)
    fs::write(
        root.join("helpers.fossil"),
        r#"
            let helper = fn () -> ()
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile chain of imports: {:?}",
        result.err()
    );
}

#[test]
fn test_diamond_dependency() {
    // Create diamond dependency: main -> {utils, helpers} -> common
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create common.fossil (shared dependency)
    fs::write(
        root.join("common.fossil"),
        r#"
            let shared = fn () -> 42
        "#,
    )
    .unwrap();

    // Create utils.fossil (imports common)
    fs::write(
        root.join("utils.fossil"),
        r#"
            open common
            let util_fn = fn () -> ()
        "#,
    )
    .unwrap();

    // Create helpers.fossil (imports common)
    fs::write(
        root.join("helpers.fossil"),
        r#"
            open common
            let helper_fn = fn () -> ()
        "#,
    )
    .unwrap();

    // Create main.fossil (imports both utils and helpers)
    fs::write(
        root.join("main.fossil"),
        r#"
            open utils
            open helpers
            let main_value = 10
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile diamond dependency: {:?}",
        result.err()
    );
}

#[test]
fn test_circular_import_detection() {
    // Create circular dependency: main -> utils -> main
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil (imports utils)
    fs::write(
        root.join("main.fossil"),
        r#"
            open utils
            let main_value = 42
        "#,
    )
    .unwrap();

    // Create utils.fossil (imports main - circular!)
    fs::write(
        root.join("utils.fossil"),
        r#"
            open main
            let utils_value = 10
        "#,
    )
    .unwrap();

    // Compile the project - should fail with circular import error
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_err(),
        "Should detect circular import but compilation succeeded"
    );

    // Verify it's specifically a circular import error
    if let Err(e) = result {
        let error_msg = format!("{:?}", e);
        assert!(
            error_msg.contains("Circular") || error_msg.contains("circular"),
            "Expected circular import error, got: {}",
            error_msg
        );
    }
}

#[test]
fn test_missing_module_error() {
    // Create project that imports non-existent module
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create main.fossil importing non-existent module
    fs::write(
        root.join("main.fossil"),
        r#"
            open nonexistent
            let x = 42
        "#,
    )
    .unwrap();

    // Compile the project - should fail with module not found error
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_err(),
        "Should fail with missing module error but compilation succeeded"
    );

    // Verify it's specifically a module not found error
    if let Err(e) = result {
        let error_msg = format!("{:?}", e);
        assert!(
            error_msg.contains("UndefinedModule") || error_msg.contains("not found"),
            "Expected module not found error, got: {}",
            error_msg
        );
    }
}

#[test]
fn test_module_with_actual_code() {
    // Test that imported modules' code compiles correctly
    // NOTE: Cross-module name resolution (using imported functions) is not yet implemented
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create math.fossil with actual functions
    fs::write(
        root.join("math.fossil"),
        r#"
            let add = fn (a, b) -> a
            let multiply = fn (a, b) -> a
        "#,
    )
    .unwrap();

    // Create main.fossil that imports math (but doesn't use its functions yet)
    fs::write(
        root.join("main.fossil"),
        r#"
            open math
            let result = 42
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile project with actual code: {:?}",
        result.err()
    );
}

#[test]
#[ignore = "Cross-module name resolution not yet implemented - imported functions not in scope"]
fn test_using_imported_functions() {
    // This test will pass once cross-module name resolution is implemented
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create math.fossil with actual functions
    fs::write(
        root.join("math.fossil"),
        r#"
            let add = fn (a, b) -> a
        "#,
    )
    .unwrap();

    // Create main.fossil that USES imported function
    fs::write(
        root.join("main.fossil"),
        r#"
            open math
            let result = add(5, 3)
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to use imported function: {:?}",
        result.err()
    );
}

#[test]
fn test_multiple_imports_in_same_file() {
    // Test importing multiple modules in one file
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create utils.fossil
    fs::write(
        root.join("utils.fossil"),
        r#"
            let util = fn () -> ()
        "#,
    )
    .unwrap();

    // Create helpers.fossil
    fs::write(
        root.join("helpers.fossil"),
        r#"
            let helper = fn () -> ()
        "#,
    )
    .unwrap();

    // Create main.fossil with multiple imports
    fs::write(
        root.join("main.fossil"),
        r#"
            open utils
            open helpers
            let result = 42
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile with multiple imports: {:?}",
        result.err()
    );
}

#[test]
fn test_deeply_nested_directory_structure() {
    // Test deep nesting: data/processing/csv/parser.fossil
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create nested directories
    fs::create_dir_all(root.join("data/processing/csv")).unwrap();

    // Create deeply nested module
    fs::write(
        root.join("data/processing/csv/parser.fossil"),
        r#"
            let parse = fn (s) -> s
        "#,
    )
    .unwrap();

    // Create main.fossil that imports nested module
    fs::write(
        root.join("main.fossil"),
        r#"
            open data::processing::csv::parser
            let result = 42
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile deeply nested structure: {:?}",
        result.err()
    );
}

#[test]
#[ignore = "Relative imports (./utils) require parser support - not yet implemented"]
fn test_mixed_absolute_and_relative_imports() {
    // Test mixing absolute (data::csv) and relative (./utils) imports
    let temp = TempDir::new().unwrap();
    let root = temp.path();

    // Create data directory
    fs::create_dir(root.join("data")).unwrap();

    // Create utils.fossil (root level)
    fs::write(
        root.join("utils.fossil"),
        r#"
            let util = fn () -> ()
        "#,
    )
    .unwrap();

    // Create data/csv.fossil
    fs::write(
        root.join("data/csv.fossil"),
        r#"
            let parse = fn () -> ()
        "#,
    )
    .unwrap();

    // Create main.fossil with mixed imports
    fs::write(
        root.join("main.fossil"),
        r#"
            open ./utils
            open data::csv
            let result = 42
        "#,
    )
    .unwrap();

    // Compile the project
    let compiler = Compiler::new();
    let input = CompilerInput::File(root.join("main.fossil"));
    let result = compiler.compile(input);

    assert!(
        result.is_ok(),
        "Failed to compile with mixed import styles: {:?}",
        result.err()
    );
}
