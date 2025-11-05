fn main() -> Result<(), ikigai_core::error::CompileError> {
    let source = r#"
        type Person = Data.Csv.CsvProvider<"examples/csv/people.csv">
        let people = Person.load "examples/csv/people.csv"

        type PersonWithAge = {
            name: string,
            age: int
        }

        let peopleWithAge =
            people
                |> map (fun person -> {
                    name = person.name,
                    age = Random.next 0 100
                } :> PersonWithAge)

        Data.Csv.write peopleWithAge "examples/csv/people_with_age.csv"
    "#;

    ikigai_core::compile(source)?;

    Ok(())
}
