# Syntax and Semantics

`fossil` is a functional language at heart, basing its syntax on languages such as Elixir and Rust.
But it also incorporates elements from other languages like F# and OCaml.
At its foundation, `fossil` is built on top of `polars`, a powerful data manipulation library.
Thus, the syntax may look strange at first, but it is designed to be expressive and concise.

## Identifiers and Literals

The simplest syntactical elements in `fossil` are identifiers and literals, and none of them should be especially surprising if you are experienced in programming.

Identifiers are a sequence of alphanumeric characters including underscore ("_"), and they are required to start with either a letter or an underscore.
Literals come in severaldifferent forms: string, integer, float and boolean.

```
// An identifier
abc123_

// A string literal
"Hello, world!"

// An integer literal
42

// A float literal
3.14

// A boolean literal
true
```

## Comments

As it could be seen in the snippet above, comments are denoted by the `//` symbol.
They can be used to explain or disable code temporarily.

> Only single-line comments are supported.

## Functions

Being a functional language, functions are everywhere.
Because of that, function calls are a fundamental part of the language.
They are used to execute code and produce results.
As most programming languages, `fossil` supports function calls by wrapping the function arguments in parentheses `()` preceded by the function name.
Another way to call a function is to use the `|>` operator, which allows you to chain multiple functions together.
This is called a _pipeline_.

```
let identity = fn (x) -> x
identity(42)
42 |> identity
```

## Variable bindings

Any program more complex than a single expression is bound to requiere variable bindings which serve to bind some value to a name allowing it to be used later.

```
let x = 1
```

## Record expressions

Records `{ ... }` are expressions that define a set of key-value pairs.
The syntax is similar to a dictionary or object literal: each entry has a key (_i.e._ identifier) on the left and an expression on the right, separated by `=`.
Multiple entries are separated by commas:

```
let trilobite = { species = "Eldredgeops rana", size = 10 }
```

> Record fields cannot be of type Function, closure, Record or Unit.

> It is not possible to concatenate records with mismatched fields.

## List expressions

Lists `[ ... ]` are expressions that define a sequence of values.
The syntax is similar to an array literal in many programming languages: each value is separated by commas.

```
[1, 2, 3, 4]
```

> List elements cannot be of type Function, closure, List or Unit.
