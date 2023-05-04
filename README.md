# Burlap
> A powerful and user-friendly sack interpreter, v1.1.2

## What is Burlap?
Burlap is a interpreter for the [Sack](https://github.com/RandomSoup/sack) programming language. It aims for a high level of spec compliance, any valid sack program should be able to be run using burlap (it does have a few minor variations from the spec, known differences are listed [here](docs/spec-diff.md))

## Features
- [x] Complete sack v0.0.5 support
- [x] Complete sack v1.0.0 support
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
  - [x] File IO **(New)**
    - [x] Open **(New)**
    - [x] Read **(New)**
    - [x] Write **(New)**
    - [x] Close **(New)**
    - [x] Flush **(New)**
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

## History

Burlap started because I wanted to learn how to create a programming language. I knew about Sack and thought it would be a perfect way to dive in (and it was!). Anyway, here's the change log:

- 1.1.2: Added C iterop and `__burlap_throw`
- 1.1.1: Added file IO
- 1.1.0: Rewrote tree walking interpeter to a stack based VM
- 1.0.0: Rewrote C++ to Rust. Lists, while loops, input, imports, and more have been added.
- 0.0.1: C++ version works for Sack v0.0.5
- Beta: I tried to make a Sack to Python transpiler using Python regexes

## Extensions

Burlap has some extra features that aren't part of the sack language, there is a list in them [docs](docs/extensions.md).

## Goals/roadmap
- Autoformatter
- Debugger
- Transpiling to another programming language

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) an older sack interpreteter
