# Importing Modules

As is often the case, standard functions are not sufficient for all use cases. In such cases, it is convenient to load external modules.
Those can be seen as a collection of functions that extend the functionality of the language.
To do this, we can use the `open` keyword followed by the name of the module we want to import.
Optionally, we can also specify a prefix for the imported functions using the `as` keyword.

For example, we need the `read` function from the `Data.Csv` module, which can be imported as follows:

```ikigai-core
open Data.Csv as Csv
```

> Module names are written in PascalCase with each segment separated by a dot.
> This indicates hierarchical structure.
