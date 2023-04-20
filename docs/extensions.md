# Burlap Extensions

This is a list of extensions that burlap has.

## Flags

### `-use-auto-none`

Make `return;` expand to `return none;` and `let x;` to `let x = none;`

### `-use-all`

Enables all extension flags.

## Functions

### `__burlap_typed_eq`

Can be used to make a typed equality check between two values, for example:
```
# "true"
print(1 == 1.0);
# "false"
print(__burlap_typed_eq(1, 1.0));
```

The following code is the pure-sack equivalent:
```
functi typed_eq(a, b) {
    # Check value
    if a == b {
        # Check type
        return type(a) == type(b);
    }
    return false;
}
```
