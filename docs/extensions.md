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

### `__burlap_range(start, end)`

A faster version of `range` made for for-loops, burlap automatically uses it. Because of this, it cannot be disabled.

Ranges can be very wastful, as Burlap has to construct a `List` and then immediately turn it into a `__burlap_iter`, as well as storing *every* value that will be looped over.

`__burlap_range` does better than just producing a `__burlap_iter`, it makes a `__burlap_rangetype`, which only stores `(at, max, step)` and so the range can be a lot bigger and use very little memory.

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

### `__burlap_print(val)`

Prints the burlap representation of `val`. For example:
```
// Prints 'Str("Test")'
__burlap_print("Test");
```

### `__burlap_throw(err)`

Converts `err` to a string a uses it as an error
```
if really_important_function() == false {
    // Causes a run time error
    __burlap_throw("failed to do important thing!");
}
// Carry on
print("It worked!");
```

### `__burlap_load_library(libname)`

Try to load the library `libname` (raises an error on failure) and returns a (`__burlap_ptr`) handle to it. Used for FFI with other programming languages. It internally uses `dlopen` to load libraries. 

### `__burlap_load_functi(libhandle, name)`

Trys to find the symbol called `name` in the library with handle `libhandle` (raises an error on failure). Internally uses `dlsym` to find symbols.

### `__burlap_ffi_call(func_ptr)`

Calls `func_ptr`, returns `none`.

### `__burlap_ptr(x)`

The casting function for the `__burlap_ptr` type, see below.

## Internal Types

These types are internal to burlap, and shouldn't be seen by the average user.

### `__burlap_ptr`

A pointer type, unlike other internal types, `__burlap_ptr` is meant to be interacted with. As such, it can be used with equivalence operators, has a cast (`__burlap_ptr(x)`), and is returned from (extended) functions.

### `__burlap_iter`

Used for loops, stores the list and the position.

### `__burlap_rangetype`

Used for optimized ranges, stores the position, max value, and step, returned by `__burlap_range`.
