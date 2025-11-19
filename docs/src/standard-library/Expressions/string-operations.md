# String operations

## Modifying strings

### Case conversion

Converting the casing of a string is a common operation and `fossil` supports it out of the box with the `upper` and `lower` functions.

```
open Data.Csv

read "path/to/file.csv"
    |> map (fn row -> {
        name    = row.name |> upper,
        surname = lower row.surname,
    })
```

> Note that both the pipe-oriented and call-oriented syntaxes are supported.

### Stripping characters from the ends

`fossil` provides a mechanism to strip characters from the beginning and end of a string.
This is the `trim` function.

```
open Data.Csv

read "path/to/file.csv"
    |> map (fn row -> {
        name    = row.name |> trim,
        surname = trim row.surname,
    })
```
