# Burlap
> A powerful and user-friendly sack interpreter, v1.1.0

## What is burlap
Burlap is a interpreter for the [sack](https://github.com/RandomSoup/sack) programming language. It is written in Rust. It is designed for a high level of spec compliance, any sack program should be able to be run using burlap and if not then it's a bug.

## Features
- [x] Complete sack v0.0.5 support
- [ ] Complete sack v1.0.0-beta support
  - [x] Variables
    - [x] Strings
    - [x] Number
    - [x] Decimal
    - [x] Bool
    - [x] None
    - [x] Lists **(New)**
    - [x] Byte **(New)**
    - [x] Type function **(New)**
  - [x] Operators
    - [x] Logical operators (`&&`, `||`, `^^`, `!`)
    - [x] Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
    - [x] Mathematical operators (`+`, `-`, `*`, `/`, `%`)
    - [x] Variable operators (`=`, `+=`, `-=`, `*=`, `/=`)
  - [x] Print
  - [x] Input **(New)**
  - [x] Len **(New)**
  - [x] If/if else/else
  - [x] Return
  - [x] Functions
  - [x] Loops
    - [x] Range loops
    - [x] While loops **(New)**
  - [x] Casting
  - [x] Import
  - [x] Scope
  - [ ] File IO **(New)**
    - [ ] Read **(New)**
    - [ ] Write **(New)**
    - [ ] Close **(New)**
    - [ ] Flush **(New)**
- [x] REPL
- [x] Lexing errors
- [x] Parsing errors
- [x] Runtime errors

## Building
It's a normal rust project, so `cargo run` to run, `cargo build` to build, and `cargo build --release` to build in release mode (slower to build, a lot faster to run).

If you want to install it, run `cargo install --git https://github.com/Bigjango13/Burlap`, now you can use `burlap` as a command!.

## Running

If you don't use any arguments burlap will enter REPL mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.

To run code in argv use `-`, for example: `burlap - "print('Hello world!');"`.

To show help, run `burlap -h` or `burlap --help`.

## Where's the C++ interpreter?

It was removed, the last commit with it was `196539`, here are the benchmarks for both the Rust and C++ implementation at that time (made using [hyperfine](https://github.com/sharkdp/hyperfine)):
| Interpreter | Mean | Min | Max |
|:---|---:|---:|---:|
| Rust cargo/--release `tests/speedtest.sk` | 5.582 | 5.415 | 5.779 |
| C++ gcc/-O3 `tests/speedtest.sk` |  9.832 | 9.648 | 10.117 |

As you can see, the Rust interpreter is a *lot* faster, it's also more maintainable.

## Extensions

Burlap has some extra features that aren't part of the sack language, there is a list in them [docs](docs/extensions.md).

## Goals/roadmap
- Sack formatter, so all code can be formatted to fit the current sack spec.
- Sack debugger, so you can see what your program is doing and why it sets x to `none` instead of `26`.
- Transpiling to Python, transpiling sack to any different language will provide huge benefits such as calling python functions and using a wider range of tools.
- Transpiling to JavaScript, this will also allow for using sack on the web. Running on the web will make it easier to create web-based sack playgrounds.

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) an older sack interpreteter
