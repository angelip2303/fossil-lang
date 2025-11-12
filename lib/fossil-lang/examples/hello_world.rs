use ikigai_lang::module::ModuleRegistry;

pub fn main() {
    let src = r#"
        let a = 5
        let b = 10
        let c = {
            a = a,
            b = 4,
        }
    "#;

    ikigai_lang::compiler::Compiler::new(ModuleRegistry::default())
        .compile(src)
        .unwrap();
}
