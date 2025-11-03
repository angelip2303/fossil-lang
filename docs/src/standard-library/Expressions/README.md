# Basic operations

To map a dataset, the `map` function is used.

```
open Data.Csv

read "path/to/file.csv"
    |> map (fun row -> {
        name = row.name,
        surname = row.surname
    })
```
