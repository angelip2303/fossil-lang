use fossil_lang::compiler::Compiler;
use fossil_lang::module::ModuleRegistry;

pub fn main() {
    let src = r#"
        let number = 42
        let text = "hello world"
        let flag = true

        let x = 100
        let y = 200

        let point = {
            x = x,
            y = y
        }

        let person = {
            name = "Alice",
            age = 30,
            active = true
        }

        let company = {
            name = "TechCorp",
            headquarters = {
                building = "Tower A",
                floor = 10
            }
        }

        let numbers = [1, 2, 3, 4, 5]

        let strings = ["hello", "world", "fossil"]

        let x = 10
        let y = 20
        let z = 30
        let mixed = [x, y, z, 40]

        let empty = []

        let identity = fun (x) -> x

        let result = identity(42)
        let result2 = identity("hello")
        let result3 = identity(result)
        let result4 = identity(identity)
        let result5 = result4(3)

        let record = {
            name = "John",
        }

        let compatible = {
            name = "Alice",
        }

        let other = {
            surname = "Doe",
        }

        let compatible_list = [record, compatible]
        let not_compatible = [record, other]

        let list = [1, 2, 3]
        let nested_list = [[list]]
    "#;

    let registry = ModuleRegistry::default();
    let compiler = Compiler::new(&registry);
    let program = compiler.compile(src).unwrap();
    println!("{}", program);
}
