# Syntax and Semantics

`fossil` is a functional language at heart, basing its syntax on languages such as F# and OCaml.
But it also incorporates elements from other languages like Rust or R.
At its foundation, `fossil` is built on top of `fossil`, a powerful data manipulation library.
Thus, the syntax may look strange at first, but it is designed to be expressive and concise.

## Identifiers and Literals

The simplest syntactical elements in `fossil` are identifiers and literals, and none of them should be especially surprising if you are experienced in programming.

Identifiers are a sequence of alphanumeric characters including underscore ("_"), and they are required to start with either a letter or an underscore.
Literals come in five different forms: string, integer, float, boolean and Dictionary.

```
// An identifier
abc123_

// A string literal
"Hello, world!"

// An integer literal
123

// A float literal
3.14

// A boolean literal
true
```

## Comments

As it could be seen in the snippet above, comments are denoted by the `//` symbol.
They can be used to explain code or to disable code temporarily.
Note that, only single-line comments are supported.

## Functions

Being a functional languages, functions are everywhere.
`fossil` implements all operators as just functions.
Calling functions is usually done in the form of a _pipeline_.

```
"asd" :: string // equivalent to string("asd")
"asd" |> string // equivalent to string("asd")
```

## Variable bindings

Any program more complex than a single expression needs to bind variables.
This is done using the `let` keyword.

```
let data = Excel.read("data.xlsx")
```

## Pipeline

Pipelines are a powerful feature of `fossil` that allow you to chain multiple functions together.
This is done using the `|>` operator.

```
let data = Excel.read("data.xlsx") |> select({})
```

## Casts

Casts serve as a way to convert data from one type to another.
As a statically-typed language, `fossil` requires arguments to match their types.
Thus, operations over strings cannot be performed over numbers, and vice versa.
As an example, in the case of the `read` function from the `Data.Csv` module, all the columns are loaded as strings.
Therefore, if you want to perform operations over numbers, you need to cast them to the appropriate type.

```
let data = Csv.read("data.csv") |> select({ age = col("age") :: int})
```

## Records

Records `{ ... }` are expressions that define a set of key → expression mappings.
The syntax is similar to a dictionary or object literal: each entry has a key (an identifier) on the left and an expression on the right, separated by `=`.
Multiple entries are separated by commas:

```ikigai
{
  id  = col("name") :: string |> trim |> upper,
  age = col("age") :: int
}
```

Semantically, records are not primitive values but **context-dependent transformation blocks**.
Their meaning depends on the function that consumes them. For example, in the `select` function, a record specifies how to construct the output dataset: each key becomes the alias of a resulting column, and each value is an expression that defines the content of that column.
In other contexts (_e.g._ `Rdf.map` or `GraphQL.map`), records can be interpreted as mappings to RDF triples or GraphQL fields, respectively.

Record fields cannot be of type Function or closure.

## Lists

Lists `[ ... ]` are expressions that define a sequence of values.
The syntax is similar to an array literal: each value is separated by commas.

```ikigai
[1, 2, 3]
```

Semantically, lists are not primitive values but **context-dependent transformation blocks**.
Their meaning depends on the function that consumes them. For example, in the `select` function, a list specifies how to construct the output dataset: each value becomes the content of a resulting column.
In other contexts (_e.g._ `Rdf.map` or `GraphQL.map`), lists can be interpreted as mappings to RDF triples or GraphQL fields, respectively.

Lists cannot be of type Function or closure.
