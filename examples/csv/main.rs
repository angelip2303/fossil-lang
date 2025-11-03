fn main() -> Result<(), ikigai_core::error::Error> {
    let source = r#"
        type Person = Csv<"examples/csv/people.csv">
    "#;

    ikigai_core::compile(source)?;

    Ok(())
}
