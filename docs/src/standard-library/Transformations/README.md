# Basic transformations

## Joins

A join operation combines columns from two datasets into a new dataset.

```
let people = ...
let addresses = ...

join ~left:people ~right:addresses ~onleft:people.id ~onright:addresses.id
```

## Concatenation

Two datasets can be concatenated into a new dataset.

```
let people = ...
let addresses = ...

people @ addresses
```
