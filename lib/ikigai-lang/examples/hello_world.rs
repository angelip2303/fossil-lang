pub fn main() {
    let src = r#"
        let a = 5
        let b = 10
    "#;

    ikigai_lang::compile(src).unwrap();
}
