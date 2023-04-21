# Burlap Extensions

This is a list of extensions that burlap has.

## Flags

### `--use-all`

Enables all extension flags.

### `--use-auto-none`

Make `return;` expand to `return none;` and `let x;` to `let x = none;`

### `--use-burlap-extensions`

Enables using internal burlap functions, see below.

## Functions

### `__burlap_typed_eq(a, b)`

Can be used to make a typed equality check between two values, for example:
```
# "true"
print(1 == 1.0);
# "false"
print(__burlap_typed_eq(1, 1.0));
```

It can also be used to compare internal types, which `==` cannot.

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

### `__burlap_range(start, end)`

A faster version of `range` made for for-loops, in burlap for loops convert a `List` to `__burlap_iter` (an internal burlap type).

This can be very wastful, as Burlap has to construct a `List` and then immediately turn it into a `__burlap_iter`, as well as storing *every* value of the loops. 

`__burlap_range` does better than just producing a `__burlap_iter`, it makes a `__burlap_rangetype`, which only stores `(at, max, step)` and so the range can be a lot bigger and use very little memory ()

### `__burlap_print_stack()`

Prints the stack, a debugging mechanism.

## Internal Types

These types are internal to burlap, and shouldn't be seen by the average user.

### `__burlap_iter`

Used for loops, stores the list and the position.

### `__burlap_rangetype`

Used for optimized ranges, stores the position, max value, and step, returned by `__burlap_range`.
