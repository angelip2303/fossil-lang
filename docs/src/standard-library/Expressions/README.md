# Basic operations

To iterate over and transform a dataset, the `each` block is used within a pipe.

```
let input = csv!("path/to/file.csv")

type Output do
    name: string
    surname: string
end

input
|> each row -> Output { name = row.name, surname = row.surname }
```
