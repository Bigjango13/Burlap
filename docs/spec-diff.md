# Specification differences

This is a list of known differences between Burlap and the current Sack spec.

- Burlap allows `int` and `float` casts on every type. (Will change)
- Burlap lets functions access variables declared after them. (May change)
- Burlap does not buffer file IO, so `flush` simply returns none.
- Burlap allows setting list keys that don't exist.
- Burlap allows comments at the end of files.
- Burlap does not warn about style issues.
- Burlap allows bytes less than 8 bits.
- Burlap allows printing none.
