# Burlap
> A fast, powerful, and user-friendly sack interpreter, v0.0.1

## What is burlap
Burlap is a interpreter for the [sack](https://github.com/RandomSoup/sack) programming language. It is written in C++ (but may be rewritten in Rust). It is designed for a high level of spec compliance, any sack program should be able to be run using burlap and if not then it's a bug.

## Features
- [x] Complete sack v0.0.5 support
  - [x] Varibles
    - [x] Strings
    - [x] Number
    - [x] Decimal
    - [x] Bool
    - [x] None
  - [x] Operators
    - [x] Logical operators (`&&`, `||`, `^^`, `!`)
    - [x] Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
    - [x] Mathmatical operators (`+`, `-`, `*`, `/`, `%`)
    - [x] Variable operators (`=`, `+=`, `-=`, `*=`, `/=`)
  - [x] Print
  - [x] If/if else/else
  - [x] Return
  - [x] Functions
  - [x] Loop and range
  - [x] Casting
  - [x] Import
- [x] REPL
- [x] Lexing errors
- [x] Parsing errors
- [x] Runtime errors

## Known bugs
- Imports aren't relative to file and are instead relative to the user.

## Goals/roadmap
- Sack formatter, so all code can be formatted to fit the current sack spec.
- Sack debugger, so you can see what your program is doing and why it sets x to `none` instead of `26`.
- Transpiling to Python, transpiling sack to any diffrent language will provide huge benefits such as calling python functions and using a wider range of tools (and not to forget performance! Generally huge languages with lots of funding have pretty fast interpreters).
- Transpiling to JavaScript, see above for reasons, but this will also allow for using sack on the web.
- Transpiling to C, see above for reasons, but this will also allow for using sack as a compiled langauge which opens lots of doors.
- Rewrite burlap in Rust, once this is done "blazingly" will be added in front of every "fast", other then that Rust will also allow for nicer abstractions such as using Rusts `enum`s instead of abusing C++ RTTI.

## Building
All the building is managed by the Makefile, just run `make` and burlap will be build in the new `build/` directory. If you want a debug build (useful for debugging burlap itself), use `make debug` instead.

## Running
If you don't use any arguments burlap will enter repl mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) a sack interpreteter in rust, most commonly used.
- [Sack](https://github.com/StealthHydra179/sack) an old sack interpreter in python, possibly home to the original sack spec.
