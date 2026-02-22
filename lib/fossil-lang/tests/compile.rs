//! Integration tests: full compilation pipeline (parse → expand → convert → resolve → typecheck)

mod common;
use common::compile;

use fossil_lang::ir::{ExprKind, PrimitiveType, StmtKind, TypeKind};
use fossil_lang::passes::IrProgram;

fn let_value_type<'a>(prog: &'a IrProgram, idx: usize) -> &'a TypeKind {
    let stmt = prog.ir.stmts.get(prog.ir.root[idx]);
    match &stmt.kind {
        StmtKind::Let { value, .. } => {
            let ty_id = prog.typeck_results.expr_types.get(value)
                .expect("expected type for let value");
            &prog.ir.types.get(*ty_id).kind
        }
        other => panic!("expected Let at root[{}], got {:?}", idx, other),
    }
}

#[test]
fn compile_let_literals() {
    let cases: &[(&str, fn(&TypeKind) -> bool)] = &[
        ("let x = 42", |tk| matches!(tk, TypeKind::Primitive(PrimitiveType::Int))),
        ("let x = \"hello\"", |tk| matches!(tk, TypeKind::Primitive(PrimitiveType::String))),
        ("let x = true", |tk| matches!(tk, TypeKind::Primitive(PrimitiveType::Bool))),
    ];
    for (src, check) in cases {
        let prog = compile(src).unwrap();
        assert!(check(let_value_type(&prog, 0)), "failed for: {}", src);
    }
}

#[test]
fn compile_type_and_record_construction() {
    let prog = compile(
        "type Person do Name: string Age: int end\n\
         let p = Person { Name = \"Alice\", Age = 30 }",
    )
    .unwrap();
    assert!(matches!(let_value_type(&prog, 1), TypeKind::Named(_)));
}

#[test]
fn compile_field_access() {
    let prog = compile(
        "type T do Name: string end\n\
         let t = T { Name = \"hi\" }\n\
         let n = t.Name",
    )
    .unwrap();
    assert!(matches!(
        let_value_type(&prog, 2),
        TypeKind::Primitive(PrimitiveType::String)
    ));
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
fn compile_multiple_types() {
    let prog = compile(
        "type A do X: int end\n\
         type B do Y: string end\n\
         let a = A { X = 1 }\n\
         let b = B { Y = \"hi\" }",
    )
    .unwrap();
    assert_eq!(prog.ir.root.len(), 4);
    assert!(matches!(let_value_type(&prog, 2), TypeKind::Named(_)));
    assert!(matches!(let_value_type(&prog, 3), TypeKind::Named(_)));
}

#[test]
fn compile_optional_field() {
    let prog = compile(
        "type T do Name: string Age: int? end\n\
         let t = T { Name = \"hi\", Age = 42 }",
    )
    .unwrap();
    let stmt = prog.ir.stmts.get(prog.ir.root[0]);
    if let StmtKind::Type { ty, .. } = &stmt.kind {
        let ty_node = prog.ir.types.get(*ty);
        if let TypeKind::Record(fields) = &ty_node.kind {
            let age_ty = fields.fields[1].1;
            assert!(
                matches!(prog.ir.types.get(age_ty).kind, TypeKind::Optional(_)),
                "Age should be Optional"
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
    compile(
        "type T do Age: int? end\n\
         let t = T { Age = 42 }",
    )
    .unwrap();
}

#[test]
fn compile_projection() {
    let prog = compile(
        "type T do Name: string end\n\
         let x = 42\n\
         x |> each row -> T { Name = \"hi\" }",
    )
    .unwrap();
    // root[2] is the pipe expression statement
    let stmt = prog.ir.stmts.get(prog.ir.root[2]);
    if let StmtKind::Expr(expr_id) = &stmt.kind {
        let expr = prog.ir.exprs.get(*expr_id);
        assert!(
            matches!(&expr.kind, ExprKind::Projection { outputs, .. } if outputs.len() == 1),
            "expected Projection with 1 output, got {:?}", expr.kind
        );
    } else {
        panic!("expected Expr stmt at root[2]");
    }
}

#[test]
fn compile_record_with_ctor_args() {
    let prog = compile(
        "type T(id: string) do Name: string end\n\
         let t = T(\"abc\") { Name = \"hi\" }",
    )
    .unwrap();
    assert!(matches!(let_value_type(&prog, 1), TypeKind::Named(_)));
}

#[test]
fn error_undefined_variable() {
    assert!(compile("let y = x").is_err());
}

#[test]
fn error_undefined_type() {
    assert!(compile("type T do Name: Foo end").is_err());
}

#[test]
fn error_field_not_found() {
    assert!(compile(
        "type T do Name: string end\n\
         let t = T { Name = \"hi\" }\n\
         let z = t.Age",
    ).is_err());
}

#[test]
fn compile_add_output_operator() {
    let prog = compile(
        "type A do X: int end\n\
         type B do Y: string end\n\
         let x = 42\n\
         x |> each row -> A { X = 1 } +> each row -> B { Y = \"hi\" }",
    )
    .unwrap();
    // root[3] is the chained pipe expression
    // +> flattens into a single Projection with 2 outputs
    let stmt = prog.ir.stmts.get(prog.ir.root[3]);
    if let StmtKind::Expr(expr_id) = &stmt.kind {
        let expr = prog.ir.exprs.get(*expr_id);
        assert!(
            matches!(&expr.kind, ExprKind::Projection { outputs, .. } if outputs.len() == 2),
            "expected Projection with 2 outputs, got {:?}", expr.kind
        );
    } else {
        panic!("expected Expr stmt at root[3]");
    }
}

#[test]
fn compile_chained_pipe_and_add_output() {
    // |> then +> flattens into single Projection with 2 outputs
    let prog = compile(
        "type A do X: int end\n\
         type B do Y: string end\n\
         let x = 42\n\
         let result = x |> each r -> A { X = 1 } +> each r -> B { Y = \"hi\" }",
    )
    .unwrap();
    // result should typecheck (the last output determines type)
    assert!(matches!(let_value_type(&prog, 3), TypeKind::Named(_)));
}

#[test]
fn compile_nested_field_access() {
    let prog = compile(
        "type Inner do Value: int end\n\
         type Outer do Child: Inner end\n\
         let inner = Inner { Value = 42 }\n\
         let outer = Outer { Child = inner }\n\
         let v = outer.Child.Value",
    )
    .unwrap();
    assert!(matches!(
        let_value_type(&prog, 4),
        TypeKind::Primitive(PrimitiveType::Int)
    ));
}

#[test]
fn error_duplicate_type() {
    assert!(compile(
        "type T do A: int end\n\
         type T do B: string end",
    ).is_err());
}

#[test]
fn error_unknown_field_in_record_construction() {
    assert!(compile(
        "type T do Name: string end\n\
         let t = T { BadField = \"hi\" }",
    ).is_err());
}

#[test]
fn compile_record_ctor_args_no_body() {
    // T("abc") with ctor_args but no fields is parsed as Application to RecordConstructor.
    // This is how "references" work after removing `ref` — the constructor call resolves identity.
    let prog = compile(
        "type Inner(id: string) do Name: string end\n\
         type Outer do Ref: Inner end\n\
         let x = 42\n\
         x |> each row -> Outer { Ref = Inner(\"abc\") }",
    )
    .unwrap();
    let stmt = prog.ir.stmts.get(prog.ir.root[3]);
    if let StmtKind::Expr(expr_id) = &stmt.kind {
        let expr = prog.ir.exprs.get(*expr_id);
        assert!(
            matches!(&expr.kind, ExprKind::Projection { outputs, .. } if outputs.len() == 1),
            "expected Projection with 1 output, got {:?}", expr.kind
        );
    } else {
        panic!("expected Expr stmt at root[3]");
    }
}

#[test]
fn compile_inline_constructor_in_projection_field() {
    // A constructor call used as a field value inside a projection output.
    // This should compile without errors (auto-emit handles it at runtime).
    let prog = compile(
        "type Inner(id: string) do Name: string end\n\
         type Outer do Ref: Inner end\n\
         let x = 42\n\
         x |> each row -> Outer { Ref = Inner(\"abc\") }",
    )
    .unwrap();
    // The Projection has 1 explicit output; auto-emit happens at runtime
    let stmt = prog.ir.stmts.get(prog.ir.root[3]);
    if let StmtKind::Expr(expr_id) = &stmt.kind {
        let expr = prog.ir.exprs.get(*expr_id);
        assert!(
            matches!(&expr.kind, ExprKind::Projection { outputs, .. } if outputs.len() == 1),
            "expected Projection with 1 output (auto-emit is runtime), got {:?}", expr.kind
        );
    } else {
        panic!("expected Expr stmt at root[3]");
    }
}

#[test]
fn compile_ctor_params_equal_field_count() {
    // When ctor param count equals field count, Application inference
    // should use the ctor param types (not fall through to field types).
    let prog = compile(
        "type T(x: int) do x: int end\n\
         let t = T(42)",
    )
    .unwrap();
    // let_value_type asserts the let produced a typed value in typeck_results.
    let ty = let_value_type(&prog, 1);
    assert!(matches!(ty, TypeKind::Record(_)), "expected Record, got {:?}", ty);
}

#[test]
fn compile_ctor_param_type_differs_from_field() {
    // Ctor param type (int) differs from field type (string).
    // The Application path must use the ctor param type, so T(42) compiles.
    // Previously this failed: "Expected `String`, found `Int`".
    let prog = compile(
        "type T(id: int) do id: string end\n\
         let t = T(42)",
    )
    .unwrap();
    let ty = let_value_type(&prog, 1);
    assert!(matches!(ty, TypeKind::Record(_)), "expected Record, got {:?}", ty);

    // Passing a string (the field type) must fail — proves we use ctor param types.
    assert!(compile(
        "type T(id: int) do id: string end\n\
         let t = T(\"wrong\")",
    ).is_err());

    // RecordInstance syntax T(ctor_args) { fields } still works independently.
    let prog2 = compile(
        "type T(id: int) do Name: string end\n\
         let t = T(42) { Name = \"hi\" }",
    )
    .unwrap();
    assert!(matches!(let_value_type(&prog2, 1), TypeKind::Named(_)));
}

#[test]
fn error_optional_narrowing() {
    // T? → T narrowing is unsound and should be rejected.
    // Use constructor-call syntax (Application) which unifies arg types against param types.
    assert!(compile(
        "type A do Val: int? end\n\
         type B(x: int) do x: int end\n\
         let a = A { Val = 42 }\n\
         let b = B(a.Val)"
    ).is_err());
}

#[test]
fn error_ref_keyword_removed() {
    // `ref T("abc")` should produce a syntax error (no longer valid)
    assert!(compile(
        "type T(id: string) do Name: string end\n\
         let t = ref T(\"abc\")",
    ).is_err());
}
