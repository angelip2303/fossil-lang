## Known-issues

### Record fields and let-variables are mutally overloaded

```
let a = 3

let b = {
    a = 4,
}
```

The `b.a` field shadows the outer `a` variable.
