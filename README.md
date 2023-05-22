# Burlap
> A powerful and user-friendly sack interpreter, v1.1.5

## What is Burlap?
Burlap is a interpreter for the [Sack](https://github.com/RandomSoup/sack) programming language. It aims for a high level of spec compliance, any valid sack program should be able to be run using burlap (it does have a few minor variations from the spec, known differences are listed [here](docs/spec-diff.md))

## Features
- [x] Complete sack v0.0.5 support
- [x] Complete sack v1.0.0 support
  - [x] Types
    - [x] Strings
    - [x] Number
    - [x] Decimal
    - [x] Bool
    - [x] None
    - [x] Lists **(New)**
    - [x] Byte **(New)**
    - [x] `type` function **(New)**
  - [x] Operators
    - [x] Logical operators (`&&`, `||`, `^^`, `!`)
    - [x] Comparison operators (`>`, `<`, `>=`, `<=`, `==`, `!=`)
    - [x] Mathematical operators (`+`, `-`, `*`, `/`, `%`)
    - [x] Variable operators (`=`, `+=`, `-=`, `*=`, `/=`)
  - [x] Print
  - [x] Input **(New)**
  - [x] Len **(New)**
  - [x] Args **(New)**
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
    - [x] Seek **(New)**
- [x] Lexing errors
- [x] Parsing errors
- [x] Runtime errors
- [x] REPL
  - [x] Tab-completion
  - [x] Colors
  - [x] History
  - [x] Multiline editing
- [x] Extra features
  - [x] C interop for all number types
  - [x] C interop for strings
  - [x] Custom error messages
  - [x] And [more](docs/extensions.md)!

## Building

It's a normal rust project, so `cargo run` to run, `cargo build` to build, and `cargo build --release` to build in release mode (slower to build, a lot faster to run).

If you want to install it, run `cargo install --git https://github.com/Bigjango13/Burlap`, now you can use `burlap` as a command!.

## Running

If you don't use any arguments burlap will enter REPL mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.
For example: `burlap tests/test.sk`

You can pass arguments by putting things after the file, for example: `burlap tests/greet.sk Bob`.

To run code from argv using `-`, for example: `burlap - "print('Hello world!');"`.

To pass arguments to code in argv or the REPL use `--`, for example: `burlap -- --some-flag`

To show help, run `burlap -h` or `burlap --help`.

## History

Burlap started because I wanted to learn how to create a programming language. I knew about Sack and thought it would be a perfect way to dive in (and it was!). Anyway, here's the change log:

- 1.1.5: Self host speedrun
    - Add `args()`
    - Fix major bug with jumping instructions
    - Fix empty import bug
    - Make `str` return none on failure
    - Remove boolean and none math
- 1.1.4: Byte and file update
    - Add string indexing
    - Add string iterating
    - Add `seek(file, pos)`
    - Add `byte(x)` function
    - Make `write` not truncate
    - Make `len` zero indexed
    - Allow string casts to fail
    - Fixed compiler bug with the `PUSH3` instruction
- 1.1.3: REPL Update
    - Make block errors less noisy
    - Add multiline editing, highlighting, and tab-completion to the REPL
- 1.1.2: C update
    - Added C iterop
    - `__burlap_throw`
    - Fixed global variables for imports
- 1.1.1: File IO
    - Read, write, open, and flush
- 1.1.0: VM update
    - Rewrite tree walking interpreter into a stack based VM
- 1.0.0: Rust Evangelism Strike Force update
    - Decide to RIIR
    - Lists, while loops, input, imports, and more!
- 0.0.1: C++ version works for Sack v0.0.5
- Beta: I tried to make a Sack to Python transpiler in Python using regexes

## Extensions

Burlap has some extra features that aren't part of the sack language, there is a list in them [docs](docs/extensions.md).

## Goals/roadmap
- Autoformatter
- Debugger
- Transpiling to another programming language

## Alternatives

- [Skcore](https://github.com/Luminoso-256/scriptinglang) an older sack interpreteter
