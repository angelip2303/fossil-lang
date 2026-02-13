//! Integration tests: error quality and diagnostics

mod common;
use common::compile;

use fossil_lang::error::FossilError;

fn first_error(src: &str) -> FossilError {
    match compile(src) {
        Ok(_) => panic!("expected compilation to fail for:\n{}", src),
        Err(errors) => errors.0.into_iter().next().unwrap(),
    }
}

#[test]
fn syntax_error_produces_syntax_variant() {
    let err = first_error("let x");
    assert!(matches!(err, FossilError::Syntax { .. }));
}

#[test]
fn syntax_error_missing_end() {
    let err = first_error("type T do Name: string");
    assert!(matches!(err, FossilError::Syntax { .. }));
}

#[test]
fn undefined_variable_message() {
    let err = first_error("let y = unknown_var");
    match &err {
        FossilError::Undefined { kind: "variable", name, .. } => {
            assert!(name.contains("unknown_var"));
        }
        other => panic!("expected Undefined variable, got {:?}", other),
    }
}

#[test]
fn undefined_type_message() {
    let err = first_error("type T do Name: NonExistent end");
    match &err {
        FossilError::Undefined { kind: "type", name, .. } => {
            assert!(name.contains("NonExistent"));
        }
        other => panic!("expected Undefined type, got {:?}", other),
    }
}

#[test]
fn duplicate_definition_message() {
    let err = first_error(
        "type T do A: int end\n\
         type T do B: string end",
    );
    match &err {
        FossilError::AlreadyDefined { name, .. } => {
            assert!(name.contains("T"));
        }
        other => panic!("expected AlreadyDefined, got {:?}", other),
    }
}

#[test]
fn field_not_found_message() {
    let err = first_error(
        "type T do Name: string end\n\
         let t = T { Name = \"hi\" }\n\
         let z = t.Missing",
    );
    match &err {
        FossilError::FieldNotFound { field, .. } => {
            assert!(field.contains("Missing"));
        }
        other => panic!("expected FieldNotFound, got {:?}", other),
    }
}

#[test]
fn field_not_found_in_projection() {
    let err = first_error(
        "type Data do Name: string end\n\
         let d = Data { Name = \"hi\" }\n\
         let result = d |> each row -> row.NonExistent",
    );
    match &err {
        FossilError::FieldNotFound { field, .. } => {
            assert!(field.contains("NonExistent"));
        }
        other => panic!("expected FieldNotFound, got {:?}", other),
    }
}

#[test]
fn multiple_errors_collected() {
    let result = compile(
        "let a = unknown1\n\
         let b = unknown2",
    );
    match result {
        Ok(_) => panic!("expected compilation to fail"),
        Err(errors) => {
            assert!(errors.len() >= 2);
        }
    }
}
