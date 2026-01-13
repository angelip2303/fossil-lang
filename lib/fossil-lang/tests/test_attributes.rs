use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;

#[test]
fn test_parse_attributes_in_record() {
    let src = r#"
        type Person = {
            #[uri("http://xmlns.com/foaf/0.1/name")]
            name: string,

            #[uri("http://xmlns.com/foaf/0.1/age")]
            age: int
        }
    "#;

    let gcx = GlobalContext::new();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Attributes parsed successfully"),
        Err(e) => panic!("Failed to parse attributes: {:?}", e),
    }
}

#[test]
fn test_attribute_without_args() {
    let src = r#"
        type Config = {
            #[required]
            api_key: string,

            #[optional]
            debug: bool
        }
    "#;

    let gcx = GlobalContext::new();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Attributes without args parsed successfully"),
        Err(e) => panic!("Failed to parse attributes without args: {:?}", e),
    }
}

#[test]
fn test_multiple_attributes_on_field() {
    let src = r#"
        type User = {
            #[uri("http://example.com/name")]
            #[required]
            username: string
        }
    "#;

    let gcx = GlobalContext::new();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Multiple attributes on single field parsed successfully"),
        Err(e) => panic!("Failed to parse multiple attributes: {:?}", e),
    }
}

#[test]
fn test_record_without_attributes() {
    let src = r#"
        type Simple = {
            name: string,
            value: int
        }
    "#;

    let gcx = GlobalContext::new();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Record without attributes parsed successfully"),
        Err(e) => panic!("Failed to parse record without attributes: {:?}", e),
    }
}
