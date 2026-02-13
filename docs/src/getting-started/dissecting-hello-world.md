# Dissecting Hello World!

```fossil
type Input = csv!("people.csv")

@rdf(type = "http://xmlns.com/foaf/0.1/Person")
type Output(id: string) do
    @rdf(uri = "http://example.com/person/name")
    name: string

    @rdf(uri = "http://example.com/person/age")
    age: int

    @rdf(uri = "http://example.com/person/email")
    email: string
end

input
|> each row -> Output("http://example.com/person/${row.name}") {
    name = row.name,
    age = row.age,
    email = row.email
}
|> Rdf.serialize("results.ttl")
```

There are a number of things going on in the basic pipeline example so lets break them down one step at a time.

```fossil
type
```

`fossil` uses the keyword `type` to declare a new type.

```fossil
csv!("people.csv")
```

`fossil` introduces a meta-programming feature that allow users declare types whose structure is inferred from source files at compile-time.
This mechanism generates modules that provide two core functionalities: _a type_ and the `load` function.
The type is a record type that represents the structure of the source, while the `load` function is used to load the data from the source file.
In this example, we define a type `Input` that represents the structure of a CSV file.

> Providers can be distinguished from functions as they are suffixed with the bang symbol `!`.

```fossil
do
    @rdf(uri = "http://example.com/person/name")
    name: string
    ...
end
```

A record is a composite data structure that can be used to represent collections of fixed (in number) fields with possibly different data types each.
The collection of fields is surrounded by `do...end` keywords.
Fields are defined using a name followed by a colon `:` and the type of the field.
Fields can also be annotated with attributes, such as `@rdf(uri = "...")`, which can be used to provide additional information about the field.

> Internally, type providers build the record type from the source file.

```fossil
let
```

`fossil` uses the `let` keyword to bind values for later use.

```fossil
each row -> ...
```

The `each` keyword introduces a per-row iteration block within a pipe.
It binds the current row to a name (here `row`) and produces an output expression for each row.

```fossil
Output("http://example.com/person/${row.name}") { name = row.name, ... }
```

Constructors are a way to create new instances of a record type.
Constructor arguments (in parentheses) provide metadata like the subject IRI.
Record fields (in braces) provide the data values.

```
|>
```

The pipe operator `|>` is used to chain operations together.
It takes the output of the previous expression and passes it as input to the next operation.
Hence, `var |> func` is equivalent to `func(var)`.
This allows for a more functional programming style.

> A pipeline is typically written as a sequence of operations, each in a separate line, with the preceding pipeline operator `|>`. Refer to the last three lines of the example on the top of this page.

```fossil
"http://example.com/person/${row.name}"
```

`fossil` provides a way to interpolate values into strings using the `${}` syntax.
The only requirement is that the value being interpolated is must implement the `ToString` trait.
