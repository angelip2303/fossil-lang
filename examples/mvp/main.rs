fn main() -> Result<(), ikigai_core::error::CompileError> {
    let source = r#"
        open Data.Csv as Csv
        open Random as Random

        type Person = Csv<"examples/csv/people.csv">
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

        Csv.write peopleWithAge "examples/csv/people_with_age.csv"
    "#;

    ikigai_core::compile(source)?;

    Ok(())
}
