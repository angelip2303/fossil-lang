use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::context::{ArgSpec, ArgType, AttributeSchema, AttributeTarget};
use fossil_lang::passes::GlobalContext;

/// Create a GlobalContext with standard attribute schemas registered
fn create_context_with_attributes() -> GlobalContext {
    let mut gcx = GlobalContext::new();

    // Register #[rdf(uri = "...", prefix = "...", datatype = "...")]
    gcx.register_attribute(
        AttributeSchema::new("rdf", AttributeTarget::Field)
            .arg("uri", ArgSpec::required(ArgType::String))
            .arg("prefix", ArgSpec::optional(ArgType::String))
            .arg("datatype", ArgSpec::optional(ArgType::String)),
    );

    // Register #[optional]
    gcx.register_attribute(AttributeSchema::new("optional", AttributeTarget::Field));

    // Register #[required]
    gcx.register_attribute(AttributeSchema::new("required", AttributeTarget::Field));

    // Register #[sql(column = "...", primary_key = true/false)]
    gcx.register_attribute(
        AttributeSchema::new("sql", AttributeTarget::Field)
            .arg("column", ArgSpec::optional(ArgType::String))
            .arg("primary_key", ArgSpec::optional(ArgType::Bool)),
    );

    // Register #[deprecated(message = "...")]
    gcx.register_attribute(
        AttributeSchema::new("deprecated", AttributeTarget::Any)
            .arg("message", ArgSpec::optional(ArgType::String)),
    );

    gcx
}

#[test]
fn test_parse_attributes_in_record() {
    let src = r#"
        type Person = {
            #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
            name: string,

            #[rdf(uri = "http://xmlns.com/foaf/0.1/age")]
            age: int
        }
    "#;

    let gcx = create_context_with_attributes();
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

    let gcx = create_context_with_attributes();
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
            #[rdf(uri = "http://example.com/name")]
            #[required]
            username: string
        }
    "#;

    let gcx = create_context_with_attributes();
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

// =====================================================
// Attribute Validation Tests
// =====================================================

#[test]
fn test_unknown_attribute_error() {
    let src = r#"
        type Person = {
            #[unknown(foo = "bar")]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => panic!("Expected error for unknown attribute"),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.contains("unknown") || msg.contains("Unknown"),
                "Expected 'unknown attribute' error, got: {}",
                msg
            );
            println!("✓ Unknown attribute correctly rejected: {:?}", e);
        }
    }
}

#[test]
fn test_missing_required_arg_error() {
    let src = r#"
        type Person = {
            #[rdf()]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => panic!("Expected error for missing required argument"),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.contains("uri") || msg.contains("required"),
                "Expected 'missing required argument' error, got: {}",
                msg
            );
            println!("✓ Missing required arg correctly rejected: {:?}", e);
        }
    }
}

#[test]
fn test_type_mismatch_error() {
    let src = r#"
        type Person = {
            #[rdf(uri = 42)]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => panic!("Expected error for type mismatch"),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.contains("mismatch") || msg.contains("expected") || msg.contains("string"),
                "Expected 'type mismatch' error, got: {}",
                msg
            );
            println!("✓ Type mismatch correctly rejected: {:?}", e);
        }
    }
}

#[test]
fn test_unknown_arg_error() {
    let src = r#"
        type Person = {
            #[rdf(uri = "http://example.com", unknownarg = "value")]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => panic!("Expected error for unknown argument"),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.contains("unknownarg") || msg.contains("Unknown argument"),
                "Expected 'unknown argument' error, got: {}",
                msg
            );
            println!("✓ Unknown arg correctly rejected: {:?}", e);
        }
    }
}

#[test]
fn test_unknown_arg_with_suggestion() {
    let src = r#"
        type Person = {
            #[rdf(uri = "http://example.com", prfix = "foaf")]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => panic!("Expected error for unknown argument with suggestion"),
        Err(e) => {
            let msg = format!("{:?}", e);
            // Should have "Did you mean 'prefix'?" suggestion
            println!("✓ Error for misspelled arg: {:?}", e);
            // The error should contain the misspelled arg name
            assert!(
                msg.contains("prfix") || msg.contains("Unknown"),
                "Expected error mentioning 'prfix', got: {}",
                msg
            );
        }
    }
}

#[test]
fn test_sql_attribute_valid() {
    let src = r#"
        type User = {
            #[sql(column = "user_id", primary_key = true)]
            id: int,

            #[sql(column = "user_name")]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ SQL attributes parsed successfully"),
        Err(e) => panic!("Failed to parse SQL attributes: {:?}", e),
    }
}

#[test]
fn test_deprecated_attribute_valid() {
    let src = r#"
        type OldApi = {
            #[deprecated(message = "Use newField instead")]
            oldField: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Deprecated attribute parsed successfully"),
        Err(e) => panic!("Failed to parse deprecated attribute: {:?}", e),
    }
}

#[test]
fn test_attribute_optional_args() {
    // Test that optional args can be omitted
    let src = r#"
        type Person = {
            #[rdf(uri = "http://example.com")]
            name: string
        }
    "#;

    let gcx = create_context_with_attributes();
    let compiler = Compiler::with_context(gcx);
    let input = CompilerInput::String {
        src: src.to_string(),
        name: "test".to_string(),
    };
    let result = compiler.compile(input);

    match result {
        Ok(_) => println!("✓ Optional args correctly handled"),
        Err(e) => panic!("Failed with optional args: {:?}", e),
    }
}
