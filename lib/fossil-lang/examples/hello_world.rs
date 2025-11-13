pub fn main() {
    let src = r#"
        let a = 5
        let b = 10
        let c = {
            a = a,
            b = 4,
        }
    "#;

    let compiler = fossil_lang::compiler::Compiler;
    let _ = compiler.compile(src);
}
