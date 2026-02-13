# Data.Rdf

The `Data.Rdf` module provides functions for working with RDF knowledge graphs.
It includes functions for writing RDF data.

## Writing RDF knowledge graphs

```
let input = csv!("path/to/input.csv")

type Person(id: string) do
    @rdf(uri = "http://example.com/person/name")
    name: string

    @rdf(uri = "http://example.com/person/age")
    age: int

    @rdf(uri = "http://example.com/person/email")
    email: string
end

input
|> each person -> Person("http://example.com/person/${person.name}") {
    name = person.name,
    age = person.age,
    email = person.email
}
|> Rdf.serialize("path/to/output.ttl")
```
