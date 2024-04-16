# Burlap Extensions

This is a list of extensions that burlap has, "extensions" is any flag, function, or type not explicitly outlined by the specifications.

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

### `__burlap_load_library(libname)` (C-FFI Only)

Try to load the library `libname` (raises an error on failure) and returns a (`__burlap_ptr`) handle to it. Used for FFI with other programming languages. It internally uses `dlopen` to load libraries. 

### `__burlap_load_functi(libhandle, name)` (C-FFI Only)

Tries to find the symbol called `name` in the library with handle `libhandle` (raises an error on failure). Internally uses `dlsym` to find symbols.

### `__burlap_ffi_call(func_ptr, args, ret_type)` (C-FFI Only)

Calls `func_ptr` with the C equivalent of `args`, returns the return value as `ret_type`.

Currently the members of `args` and the value of `ret_type` can be any of the following:
- Number (`int32_t`)
- Decimal (`float`)
- Byte (`uint8_t`)
- Bool (`_Bool`)
- String (`char*`)
- `__burlap_ptr` (`void*`)
- None or an empty string (`void`, only valid for return types)

`ret_type` is intentionally set up so that it can use the return value of `type`

For example:
mylib.c
```c
#include <stdio.h>

int double_num(int n) {
    printf("N is: %i\n", n);
    return n * 2;
}
```
and compile with `<compiler> -shared -fPIC mylib.c -o mylib.so`

Sack:
```
let handle = __burlap_load_lib("mylib.so");
let print_n = __burlap_load_functi(handle, "double_num");
// Should print "N is: 47"
let mynum = __burlap_ffi_call(print_n, [47], "Number")
// Should print "Doubled num: 94"
print("Doubled num: " + mynum)
```

### `__burlap_ptr(x)` (C-FFI Only)

The casting function for the `__burlap_ptr` type, see below.

### `__burlap_load_var(ref)`

Loads the underlying value from `ref` (which must be a `__burlap_reftype`). For an example, see `tests/internals.sk`.

### `__burlap_set_var(ref, value)`

Sets the underlying value of `ref` (which must be a `__burlap_reftype`) to `value`. For an example, see `tests/internals.sk`.

## Internal Types

These types are internal to burlap, and shouldn't be seen by the average user.

### `__burlap_ptr` (C-FFI Only)

A pointer type, unlike other internal types, `__burlap_ptr` is meant to be interacted with. As such, it can be used with equivalence operators, has a cast (`__burlap_ptr(x)`), and is returned from buildin (extended) functions.

### `__burlap_iter`

Used for loops, stores the list and the position.

### `__burlap_rangetype`

Used for optimized ranges, stores the position, max value, and step, returned by `__burlap_range`.

### `__burlap_reftype`

Used for references, can be created by calling `__burlap_reftype` with a variable. For an example, see `tests/internals.sk`.
