# Dissecting the basic pipeline

```
open Data.Csv as Csv
open Random as Random

type Person = Csv<"people.csv">

type PersonWithAge = {
    name: string,
    age: number
}

let people = Person.load "people.csv"

let peopleWithAge =
    people
        |> map (fun person -> {
            name = person.name,
            age = Random.int 0 100
        } :> PersonWithAge)

Csv.write peopleWithAge "people_with_age.csv"
```

There are a number of things going on in the basic pipeline example so lets break them down one step at a time.

```
open Data.Csv as Csv
open Random as Random
```

`fossil` uses the keyword `open` to load the content of another module.
In this example we use it to get access to the `Data.Csv` and `Random` modules, which are bundled in the `fossil` standard library.
The `Data` sub-modules provide a set of functions for working with different data formats.
In this case, we use the `Data.Csv` module to read and write a CSV file.
The `Random` module provides a set of functions for generating random values.
As it can be seen, the `as` keyword is used to give a module an alias, _e.g_ load all the content in `Data.Csv` and make it available with the `Csv` prefix.

> In case no alias is provided, the module is loaded globally.

```
let
```

`fossil` uses the `let` keyword to bind values for later use.

```
type Person = Csv<"people.csv">
let people = Person.load "people.csv"
```

Here we access the `read` function from  `Data.Csv` and `Data.Excel`, respectively.
As mentioned earlier, in the case of the `Data.Csv` module, the `read` function is loaded globally, thus available in the global scope, while in the case of latter, the `Excel` prefix is used to access the corresponding function.

```
|>
```

The pipe operator `|>` is used to chain functions together.
It takes the output of the previous function and passes it as the first argument to the next function call.
Hence, `var |> func` is equivalent to `func(var)`.

```
join(addresses, left_on = "name", right_on = "name")
```

Functions can also have named arguments, which can be used to make the code more readable.

```
{
  id  = col("name") :: string |> trim |> upper,
  age = col("age") :: int
}
```

Records `{ ... }` are expressions that define key → expression mappings.
They are not primitive values but context-dependent blocks of transformations.
For example, in the `select` function, a record specifies how to derive new columns: each key is the alias of the resulting column, and each value is an expression that produces its content.

```
::
```

`::` is the cast operator, and can be used to convert a value to a specific type.


> Implementation wise, the cast operator is a syntactic sugar for a function call, _e.g_ `var :: string` is equivalent to `string(var)`.
