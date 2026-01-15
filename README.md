# TODO

## Type annotations in type definitions

This should be rejected as an Error, i.e. type declarations cannot have a type annotation.

```
type MyType: Foo = ...
```

## Properly handle imports

Import statements should be properly handled. This should also be included in the LSP.

## Maybe the language server should be moved to lib?
