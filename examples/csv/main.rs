fn main() -> Result<(), ikigai_core::error::CompileError> {
    let source = r#"
        type Person = Data.Csv.CsvProvider<"examples/csv/people.csv">
        let people = Person.load "examples/csv/people.csv"
    "#;

    // Compile y ejecutar
    ikigai_core::compile_and_run(source)?;

    Ok(())
}
