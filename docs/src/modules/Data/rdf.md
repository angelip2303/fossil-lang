# Data.Rdf

The `Data.Rdf` module provides functions for working with RDF knowledge graphs.
It includes functions for writing RDF data.

## Writing RDF knowledge graphs

```
open Data.Csv as Csv
open Data.Rdf as Rdf

type Person = ShEx<"path/to/shape.shex">

Csv.read "path/to/input.csv"
  |> map (fn person -> {
        name = person.name;
        age = person.age;
        email = person.email;
      } :> Person)
  |> Rdf.write "path/to/output.ttl"
```
