//! Integration tests: error quality and diagnostics
//!
//! These tests verify that the compiler produces the *right kind* of error
//! with meaningful messages. They exercise the full pipeline (parse → … → typecheck).

use fossil_lang::context::extract_type_metadata;
use fossil_lang::error::{FossilError, FossilErrors};
use fossil_lang::passes::convert::ast_to_ir;
use fossil_lang::passes::expand::ProviderExpander;
use fossil_lang::passes::parse::Parser;
use fossil_lang::passes::resolve::IrResolver;
use fossil_lang::passes::typecheck::TypeChecker;
use fossil_lang::passes::IrProgram;

/// Full compilation: returns either a program or the collected errors.
fn compile(src: &str) -> Result<IrProgram, FossilErrors> {
    let parsed = Parser::parse(src, 0)?;
    let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx)).expand()?;
    let ty = extract_type_metadata(&expand_result.ast);
    let ir = ast_to_ir(expand_result.ast);
    let (ir, gcx) = IrResolver::new(ir, expand_result.gcx)
        .with_type_metadata(ty)
        .resolve()?;
    let program = TypeChecker::new(ir, gcx).check()?;
    Ok(program)
}

/// Extract the first error from a compilation failure.
fn first_error(src: &str) -> FossilError {
    match compile(src) {
        Ok(_) => panic!("expected compilation to fail for:\n{}", src),
        Err(errors) => errors.0.into_iter().next().unwrap(),
    }
}

// -------------------------------------------------------------------
// Syntax errors
// -------------------------------------------------------------------

#[test]
fn syntax_error_produces_syntax_variant() {
    let err = first_error("let x");
    assert!(
        matches!(err, FossilError::Syntax { .. }),
        "expected Syntax error, got {:?}",
        err
    );
}

#[test]
fn syntax_error_unclosed_brace() {
    let err = first_error("type T = { Name: string");
    assert!(
        matches!(err, FossilError::Syntax { .. }),
        "expected Syntax error, got {:?}",
        err
    );
}

// -------------------------------------------------------------------
// Resolution errors
// -------------------------------------------------------------------

#[test]
fn undefined_variable_message() {
    let err = first_error("let y = unknown_var");
    match &err {
        FossilError::UndefinedVariable { name, .. } => {
            assert!(
                name.contains("unknown_var"),
                "error should mention 'unknown_var', got: {}",
                name
            );
        }
        other => panic!("expected UndefinedVariable, got {:?}", other),
    }
}

#[test]
fn undefined_type_message() {
    let err = first_error("type T = { Name: NonExistent }");
    match &err {
        FossilError::UndefinedType { path, .. } => {
            assert!(
                path.contains("NonExistent"),
                "error should mention 'NonExistent', got: {}",
                path
            );
        }
        other => panic!("expected UndefinedType, got {:?}", other),
    }
}

#[test]
fn duplicate_definition_message() {
    let err = first_error(
        "type T = { A: int }\n\
         type T = { B: string }",
    );
    match &err {
        FossilError::AlreadyDefined { name, .. } => {
            assert!(
                name.contains("T"),
                "error should mention 'T', got: {}",
                name
            );
        }
        other => panic!("expected AlreadyDefined, got {:?}", other),
    }
}

// -------------------------------------------------------------------
// Type errors
// -------------------------------------------------------------------

#[test]
fn type_mismatch_heterogeneous_list() {
    let err = first_error("let x = [1, \"hello\"]");
    assert!(
        matches!(err, FossilError::TypeMismatch { .. }),
        "expected TypeMismatch, got {:?}",
        err
    );
}

#[test]
fn arity_mismatch_message() {
    let err = first_error(
        "let f = fn(x) -> x\n\
         let y = f(1, 2)",
    );
    match &err {
        FossilError::ArityMismatch {
            expected, actual, ..
        } => {
            assert_eq!(*expected, 1, "expected 1 param");
            assert_eq!(*actual, 2, "actual 2 args");
        }
        other => panic!("expected ArityMismatch, got {:?}", other),
    }
}

#[test]
fn field_not_found_message() {
    let err = first_error(
        "type T = { Name: string }\n\
         let t = T { Name = \"hi\" }\n\
         let z = t.Missing",
    );
    match &err {
        FossilError::FieldNotFound { field, .. } => {
            assert!(
                field.contains("Missing"),
                "error should mention 'Missing', got: {}",
                field
            );
        }
        other => panic!("expected FieldNotFound, got {:?}", other),
    }
}

// -------------------------------------------------------------------
// Multiple errors collected
// -------------------------------------------------------------------

#[test]
fn multiple_errors_collected() {
    // Two undefined variables should produce (at least) two errors
    let result = compile(
        "let a = unknown1\n\
         let b = unknown2",
    );
    match result {
        Ok(_) => panic!("expected compilation to fail"),
        Err(errors) => {
            assert!(
                errors.len() >= 2,
                "expected at least 2 errors, got {}",
                errors.len()
            );
        }
    }
}
