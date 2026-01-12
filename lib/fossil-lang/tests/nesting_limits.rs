//! Nesting Limit Tests
//!
//! These tests verify the practical limits of nesting depth in Fossil.
//!
//! ## Summary
//! - **Theoretical limit**: Infinite (no artificial restrictions)
//! - **Practical limit**: ~92 levels (Rust stack size limitation)
//! - **Cause**: Direct recursion in parser/resolver/lowering/typechecker
//!
//! ## Results
//! - ✅ 90 levels: Safe, works reliably
//! - ⚠️  92 levels: Near limit, works but slow
//! - ❌ 95+ levels: Stack overflow
//!
//! See docs/nesting_limits.md for detailed analysis.

use fossil_lang::compiler::Compiler;

fn compile(src: &str) -> Result<(), String> {
    let compiler = Compiler::new();
    compiler
        .compile(src)
        .map(|_| ())
        .map_err(|e| format!("{:?}", e))
}

/// Generate nested function definitions
fn generate_nested_functions(depth: usize) -> String {
    let mut src = String::new();

    // Generate nested lets
    for i in 0..depth {
        src.push_str(&format!("let f{} = fn (x) -> {{\n", i));
    }

    // Innermost expression
    src.push_str("x\n");

    // Close all blocks
    for _ in 0..depth {
        src.push_str("}\n");
    }

    src
}

/// Generate nested blocks
fn generate_nested_blocks(depth: usize) -> String {
    let mut src = String::new();
    src.push_str("let result = ");

    for i in 0..depth {
        src.push_str("{\n");
        src.push_str(&format!("let x{} = {}\n", i, i));
    }

    // Innermost expression
    src.push_str("42\n");

    // Close all blocks
    for _ in 0..depth {
        src.push_str("}\n");
    }

    src
}

/// Generate nested scopes with shadowing
fn generate_nested_scopes(depth: usize) -> String {
    let mut src = String::new();

    for i in 0..depth {
        src.push_str("{\n");
        src.push_str(&format!("let x = {}\n", i));
    }

    src.push_str("x\n");

    for _ in 0..depth {
        src.push_str("}\n");
    }

    src
}

#[test]
fn test_nesting_10_levels() {
    let src = generate_nested_functions(10);
    assert!(compile(&src).is_ok(), "Should handle 10 levels of nesting");
}

#[test]
fn test_nesting_50_levels() {
    let src = generate_nested_functions(50);
    assert!(compile(&src).is_ok(), "Should handle 50 levels of nesting");
}

#[test]
fn test_nesting_75_levels() {
    let src = generate_nested_functions(75);
    assert!(compile(&src).is_ok(), "Should handle 75 levels of nesting");
}

#[test]
fn test_nesting_85_levels() {
    let src = generate_nested_functions(85);
    assert!(compile(&src).is_ok(), "Should handle 85 levels of nesting");
}

#[test]
fn test_nesting_90_levels() {
    let src = generate_nested_functions(90);
    assert!(compile(&src).is_ok(), "Should handle 90 levels of nesting");
}

#[test]
#[ignore] // Near stack limit
fn test_nesting_92_levels() {
    let src = generate_nested_functions(92);
    assert!(compile(&src).is_ok(), "Should handle 92 levels of nesting");
}

#[test]
#[ignore] // Near stack limit
fn test_nesting_95_levels() {
    let src = generate_nested_functions(95);
    assert!(compile(&src).is_ok(), "Should handle 95 levels of nesting");
}

#[test]
#[ignore] // Near stack limit
fn test_nesting_100_levels() {
    let src = generate_nested_functions(100);
    assert!(compile(&src).is_ok(), "Should handle 100 levels of nesting");
}

#[test]
#[ignore] // Exceeds stack limit
fn test_nesting_500_levels() {
    let src = generate_nested_functions(500);
    assert!(compile(&src).is_ok(), "Should handle 500 levels of nesting");
}

#[test]
#[ignore] // Exceeds stack limit
fn test_nesting_1000_levels() {
    let src = generate_nested_functions(1000);
    assert!(
        compile(&src).is_ok(),
        "Should handle 1000 levels of nesting"
    );
}

#[test]
fn test_blocks_50_levels() {
    let src = generate_nested_blocks(50);
    assert!(
        compile(&src).is_ok(),
        "Should handle 50 levels of nested blocks"
    );
}

#[test]
fn test_scopes_50_levels() {
    let src = generate_nested_scopes(50);
    assert!(
        compile(&src).is_ok(),
        "Should handle 50 levels of nested scopes"
    );
}

#[test]
#[ignore] // Near stack limit
fn test_blocks_100_levels() {
    let src = generate_nested_blocks(100);
    assert!(
        compile(&src).is_ok(),
        "Should handle 100 levels of nested blocks"
    );
}

#[test]
#[ignore] // Near stack limit
fn test_scopes_100_levels() {
    let src = generate_nested_scopes(100);
    assert!(
        compile(&src).is_ok(),
        "Should handle 100 levels of nested scopes"
    );
}

#[test]
#[ignore] // Very deep nesting - may hit stack limits
fn test_nesting_10000_levels() {
    let src = generate_nested_functions(10000);
    match compile(&src) {
        Ok(_) => println!("✓ Successfully compiled 10000 levels!"),
        Err(e) => println!("✗ Failed at 10000 levels: {}", e),
    }
}
