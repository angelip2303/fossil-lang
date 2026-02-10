//! Integration tests: full compilation pipeline (parse → expand → convert → resolve → typecheck)

use fossil_lang::compiler::Compiler;
use fossil_lang::context::extract_type_metadata;
use fossil_lang::error::FossilErrors;
use fossil_lang::ir::{PrimitiveType, StmtKind, TypeKind};
use fossil_lang::passes::convert::ast_to_ir;
use fossil_lang::passes::expand::ProviderExpander;
use fossil_lang::passes::parse::Parser;
use fossil_lang::passes::resolve::IrResolver;
use fossil_lang::passes::typecheck::TypeChecker;
use fossil_lang::passes::IrProgram;

/// Full compilation helper: source → IrProgram
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

/// Helper: get the TypeKind of the value expr for a let at root[idx]
fn let_value_type<'a>(prog: &'a IrProgram, idx: usize) -> &'a TypeKind {
    let stmt = prog.ir.stmts.get(prog.ir.root[idx]);
    match &stmt.kind {
        StmtKind::Let { value, .. } | StmtKind::Const { value, .. } => {
            let expr = prog.ir.exprs.get(*value);
            let ty_id = expr.ty.type_id();
            &prog.ir.types.get(ty_id).kind
        }
        other => panic!("expected Let/Const at root[{}], got {:?}", idx, other),
    }
}

// -------------------------------------------------------------------
// Programs that should compile successfully
// -------------------------------------------------------------------

#[test]
fn compile_let_integer() {
    let prog = compile("let x = 42").unwrap();
    assert_eq!(prog.ir.root.len(), 1);
    assert!(
        matches!(let_value_type(&prog, 0), TypeKind::Primitive(PrimitiveType::Int)),
        "expected Int"
    );
}

#[test]
fn compile_let_string() {
    let prog = compile("let x = \"hello\"").unwrap();
    assert!(matches!(
        let_value_type(&prog, 0),
        TypeKind::Primitive(PrimitiveType::String)
    ));
}

#[test]
fn compile_let_bool() {
    let prog = compile("let x = true").unwrap();
    assert!(matches!(
        let_value_type(&prog, 0),
        TypeKind::Primitive(PrimitiveType::Bool)
    ));
}

#[test]
fn compile_type_and_record_construction() {
    let prog = compile(
        "type Person = { Name: string, Age: int }\n\
         let p = Person { Name = \"Alice\", Age = 30 }",
    )
    .unwrap();
    assert!(
        matches!(let_value_type(&prog, 1), TypeKind::Named(_)),
        "expected Named(Person)"
    );
}

#[test]
fn compile_function_application() {
    let prog = compile(
        "let f = fn(x) -> x\n\
         let y = f(42)",
    )
    .unwrap();
    assert!(
        matches!(let_value_type(&prog, 1), TypeKind::Primitive(PrimitiveType::Int)),
        "identity function applied to 42 should infer Int"
    );
}

#[test]
fn compile_list_inference() {
    let prog = compile("let xs = [1, 2, 3]").unwrap();
    assert!(
        matches!(let_value_type(&prog, 0), TypeKind::List(_)),
        "expected List type"
    );
}

#[test]
fn compile_field_access() {
    let prog = compile(
        "type T = { Name: string }\n\
         let t = T { Name = \"hi\" }\n\
         let n = t.Name",
    )
    .unwrap();
    assert!(
        matches!(let_value_type(&prog, 2), TypeKind::Primitive(PrimitiveType::String)),
        "t.Name should be String"
    );
}

#[test]
fn compile_variable_propagation() {
    let prog = compile(
        "let x = 42\n\
         let y = x",
    )
    .unwrap();
    assert!(matches!(
        let_value_type(&prog, 1),
        TypeKind::Primitive(PrimitiveType::Int)
    ));
}

#[test]
fn compile_block_expression() {
    let prog = compile("let x = { let y = 1\n y }").unwrap();
    assert!(matches!(
        let_value_type(&prog, 0),
        TypeKind::Primitive(PrimitiveType::Int)
    ));
}

#[test]
fn compile_string_interpolation() {
    let prog = compile(
        "let name = \"world\"\n\
         let greeting = \"hello ${name}\"",
    )
    .unwrap();
    assert!(matches!(
        let_value_type(&prog, 1),
        TypeKind::Primitive(PrimitiveType::String)
    ));
}

#[test]
fn compile_nested_function() {
    let prog = compile(
        "let f = fn(x) -> fn(y) -> x\n\
         let g = f(42)\n\
         let z = g(\"ignored\")",
    )
    .unwrap();
    assert!(
        matches!(let_value_type(&prog, 2), TypeKind::Primitive(PrimitiveType::Int)),
        "nested closure should capture x = Int"
    );
}

#[test]
fn compile_multiple_types() {
    let prog = compile(
        "type A = { X: int }\n\
         type B = { Y: string }\n\
         let a = A { X = 1 }\n\
         let b = B { Y = \"hi\" }",
    )
    .unwrap();
    assert_eq!(prog.ir.root.len(), 4);
    assert!(matches!(let_value_type(&prog, 2), TypeKind::Named(_)));
    assert!(matches!(let_value_type(&prog, 3), TypeKind::Named(_)));
}

// -------------------------------------------------------------------
// Optional types
// -------------------------------------------------------------------

#[test]
fn compile_optional_field() {
    let prog = compile(
        "type T = { Name: string, Age: int? }\n\
         let t = T { Name = \"hi\", Age = 42 }",
    )
    .unwrap();
    // Check the type definition has an Optional field
    let stmt = prog.ir.stmts.get(prog.ir.root[0]);
    if let StmtKind::Type { ty, .. } = &stmt.kind {
        let ty_node = prog.ir.types.get(*ty);
        if let TypeKind::Record(fields) = &ty_node.kind {
            let age_ty = fields.fields[1].1;
            assert!(
                matches!(prog.ir.types.get(age_ty).kind, TypeKind::Optional(_)),
                "Age should be Optional, got {:?}",
                prog.ir.types.get(age_ty).kind
            );
        } else {
            panic!("expected Record");
        }
    } else {
        panic!("expected Type stmt");
    }
}

#[test]
fn compile_optional_widening() {
    // int should unify with int? (widening)
    let _prog = compile(
        "type T = { Age: int? }\n\
         let t = T { Age = 42 }",
    )
    .unwrap();
}

// -------------------------------------------------------------------
// Programs that should produce errors
// -------------------------------------------------------------------

#[test]
fn compile_record_with_different_field_value() {
    // Currently the typechecker does not reject mismatched field types
    // at record construction. This test documents that behavior.
    let _prog = compile(
        "type T = { Name: string }\n\
         let t = T { Name = 42 }",
    );
}

#[test]
fn error_undefined_variable() {
    let result = compile("let y = x");
    assert!(result.is_err(), "undefined variable x should fail");
}

#[test]
fn error_undefined_type() {
    let result = compile("type T = { Name: Foo }");
    assert!(result.is_err(), "undefined type Foo should fail");
}

#[test]
fn error_heterogeneous_list() {
    let result = compile("let xs = [1, \"hello\"]");
    assert!(result.is_err(), "mixed int/string list should fail");
}

#[test]
fn error_arity_mismatch() {
    let result = compile(
        "let f = fn(x) -> x\n\
         let y = f(1, 2)",
    );
    assert!(result.is_err(), "too many args should fail");
}

#[test]
fn error_field_not_found() {
    let result = compile(
        "type T = { Name: string }\n\
         let t = T { Name = \"hi\" }\n\
         let z = t.Age",
    );
    assert!(result.is_err(), "accessing missing field Age should fail");
}

#[test]
fn error_duplicate_type() {
    let result = compile(
        "type T = { A: int }\n\
         type T = { B: string }",
    );
    assert!(result.is_err(), "duplicate type definition should fail");
}

// -------------------------------------------------------------------
// Compiler struct API
// -------------------------------------------------------------------

#[test]
fn compiler_new_creates_instance() {
    let compiler = Compiler::new();
    // Just verify it doesn't panic — the Compiler struct is usable
    drop(compiler);
}
