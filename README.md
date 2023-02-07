# Burlap
> A powerful, blazingly fast, and user-friendly sack interpreter, v1.0.0

## What is burlap
Burlap is a interpreter for the [sack](https://github.com/RandomSoup/sack) programming language. It is written in Rust. It is designed for a high level of spec compliance, any sack program should be able to be run using burlap and if not then it's a bug.

## Features
- [x] Complete sack v0.0.5 support
- [x] Complete sack v1.0.0 support
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
  - [x] Input (New)
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

## Where's the C++ interpreter?

It was removed, the last commit with it was `196539`, here are the benchmarks for both the Rust and C++ implmentation at that time (made using [hyperfine](https://github.com/sharkdp/hyperfine)):
| Interpreter | Mean | Min | Max |
|:---|---:|---:|---:|
| Rust cargo/--release `tests/speedtest.sk` | 5.582 | 5.415 | 5.779 |
| C++ gcc/-O3 `tests/speedtest.sk` |  9.832 | 9.648 | 10.117 |

As you can see, the Rust interpreter is a *lot* faster, it also uses nicer abstractions.

## Extentions
These are either sack ideas that aren't currently in the spec, or non-standard changes to the sack language. **They are not part of the sack language.** <br>
They can be enabled by passing a specific flag or all enabled by `-use-all`.

Here is the list of extentions:
- String escapes (`-use-escape`), allows for `\n`, `\r`, `\t`, `\e`, `\'`, `\"` and `\\` escape codes in strings.
- Automatic none (`-use-auto-none`), allows for using `return;` as syntactic sugar for `return none;`, and `let x;` for `let x = none;`.
- Varible argument print (`-use-va-print`), allows for passing any number of args to print instead of just one.

## Goals/roadmap
- Sack formatter, so all code can be formatted to fit the current sack spec.
- Sack debugger, so you can see what your program is doing and why it sets x to `none` instead of `26`.
- Transpiling to Python, transpiling sack to any diffrent language will provide huge benefits such as calling python functions and using a wider range of tools (and not to forget performance! Generally huge languages with lots of funding have pretty fast interpreters).
- Transpiling to JavaScript, see above for reasons, but this will also allow for using sack on the web. Running on the web will make it easier to create web-based sack playgrounds.
- Transpiling to C, see above for reasons, but this will also allow for using sack as a compiled langauge which opens lots of doors.

## Building
It's a normal rust project, so `cargo run` to run, `cargo build` to build, and `cargo build --release` to build in release mode.

## Running
If you don't use any arguments burlap will enter repl mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) a sack interpreteter (also in rust), most commonly used.
