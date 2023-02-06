# Burlap
> A fast, powerful, and user-friendly sack interpreter, v0.0.1

## What is burlap
Burlap is a interpreter for the [sack](https://github.com/RandomSoup/sack) programming language. It is written in C++ (but may be rewritten in Rust). It is designed for a high level of spec compliance, any sack program should be able to be run using burlap and if not then it's a bug.

## Features
- [x] Complete sack v0.0.5 support (C++/Rust)
- [x] Complete sack v1.0.0 support (Rust)
  - [ ] Varibles
    - [x] Strings
    - [x] Number
    - [x] Decimal
    - [x] Bool
    - [x] None
    - [ ] Lists (New)
  - [x] Operators
    - [x] Logical operators (`&&`, `||`, `^^`, `!`)
    - [x] Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
    - [x] Mathmatical operators (`+`, `-`, `*`, `/`, `%`)
    - [x] Variable operators (`=`, `+=`, `-=`, `*=`, `/=`)
  - [x] Print
  - [ ] Input (New)
  - [x] If/if else/else
  - [x] Return
  - [x] Functions
  - [ ] Loops
    - [x] Range loops
    - [ ] While loops (New)
  - [x] Casting
  - [x] Import
- [x] REPL
- [x] Lexing errors
- [x] Parsing errors
- [x] Runtime errors

## Extentions
These are either sack ideas that aren't currently in the spec, or non-standard changes to the sack language. **They are not part of the sack language.** <br>
Only the WIP rust interpreter supports them. They can be enabled by passing a specific flag or all enabled by `-use-all`.

Here is the list of extentions:
- String escapes (`-use-escape`), allows for `\n`, `\r`, `\t`, `\e`, `\'`, `\"` and `\\` escape codes in strings.
- Automatic none (`-use-auto-none`), allows for using `return;` as syntactic sugar for `return none;`, and `let x;` for `let x = none;`.
- Varible argument print (`-use-va-print`), allows for passing any number of args to print instead of just one.

## Known bugs
- (C++) Imports aren't relative to file and are instead relative to the user.

## Goals/roadmap
- Sack formatter, so all code can be formatted to fit the current sack spec.
- Sack debugger, so you can see what your program is doing and why it sets x to `none` instead of `26`.
- Transpiling to Python, transpiling sack to any diffrent language will provide huge benefits such as calling python functions and using a wider range of tools (and not to forget performance! Generally huge languages with lots of funding have pretty fast interpreters).
- Transpiling to JavaScript, see above for reasons, but this will also allow for using sack on the web. Running on the web will make it easier to create web-based sack playgrounds.
- Transpiling to C, see above for reasons, but this will also allow for using sack as a compiled langauge which opens lots of doors.
- Rewrite burlap in Rust, once this is done "blazingly" will be added in front of every "fast", other then that Rust will also allow for nicer abstractions such as using Rusts `enum`s instead of abusing C++ RTTI.

## Building
All the building is managed by the Makefile, just run `make` and burlap will be build in the new `build/` directory. If you want a debug build (useful for debugging burlap itself), use `make debug` instead.

## Running
If you don't use any arguments burlap will enter repl mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) a sack interpreteter in rust, most commonly used.
