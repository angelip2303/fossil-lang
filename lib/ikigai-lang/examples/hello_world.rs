pub fn main() {
    let src = r#"
        let a = 5
        let b = 10
        let c = {
            a = 3,
            b = 4,
        }
    "#;

    ikigai_lang::compile(src).unwrap();
}
