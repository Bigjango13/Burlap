# Specification differences

This is a list of known differences between Burlap and the current Sack spec.

- Burlap does not warn about style issues.
- Burlap allows bytes less than 8 bits
- Burlap allows `int` and `float` casts on every type.
- Burlap allows setting list keys that don't exist.
- Burlap has `myList["2"]` equivalent to `myList[2]`.
- Burlap does not buffer file IO, so `flush` simply returns none.
- Burlap lets functions access variables declared after them.
- Burlap allows comments at the end of files.
- Burlap converts files to strings differently.
