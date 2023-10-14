# Burlap
> A powerful and user-friendly Sack interpreter, v1.2.3

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
      - [ ] Invalid indexes **(New)**
      - [ ] Multiplication **(New)**
      - [ ] Deletion **(New)**
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
  - [ ] Rand **(New)**
  - [x] If/if else/else
  - [x] Return
  - [x] Functions
  - [x] Loops
    - [x] Range loops
    - [x] While loops **(New)**
    - [ ] Infinite loops **(New)**
    - [x] Break **(New)**
    - [x] Continue **(New)**
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
- [x] REPL
  - [x] Tab-completion
  - [x] Colors
  - [x] History
  - [x] Multiline editing
- [x] Extra features
  - [x] C FFI (see the [Building with C FFI](# Building with C FFI) section)
  - [x] Web Assembly support (see the [Building for WASM](# Building for WASM) section)
  - [x] And [more](docs/extensions.md)!

## Building

It's a normal rust project, so `cargo run` to run, `cargo build` to build, and `cargo build --release` to build in release mode (slower to build, a lot faster to run).

If you want to install it, run `cargo install --git https://github.com/Bigjango13/Burlap`, now you can use `burlap` as a command!

## Building with C FFI

C FFI support is disabled by default because it has not been tested on Windows and MacOS (if you successfully use it on either of these platforms, please tell me!). In order to enable it, compile with the `--feature=cffi` flag. When running you will need to pass the `--use-all` feature.

The docs for using C FFI are [here](docs/extensions.md) starting at `__burlap_load_library(libname)`.

Note that C FFI and WASM are incompatible.

## Building for WASM

Burlap supports running on the web! To build, run the following commands:
```
# Install the wasm-pack tool
cargo install wasm-pack
# Build
wasm-pack build --target web --no-default-features --features=wasm --release
```

The output should be in `pkg/`, to run the code is tricker, however an example and more documentation should be available shortly.

The WASM build disables file IO, makes `args()` at global scope return `[wasm]`, and is incompatible with C FFI.

## Running

If you don't use any arguments burlap will enter REPL mode, to exit use Ctrl-D.
If you want to run a file just pass the file name as the argument.
For example: `burlap tests/test.sk`

You can pass arguments by putting things after the file, for example: `burlap tests/greet.sk Bob`.

To run code from argv using `-`, for example: `burlap - "print('Hello world!');"`.

To pass arguments to code in argv or the REPL use `--`, for example: `burlap -- --some-flag`

To show help, run `burlap -h` or `burlap --help`.

## Tests

### Feature test

The feature test can be ran with `burlap tests/test.sk`, after every run a file called `tmp-filename-for-tests` will be made as a part of file io tests. Sack currently doesn't have the ability to deleate files, it will need to be removed manually or two tests will fail on the next run.

### Speed test

The speed test is currently only one test, it can be ran with `burlap tests/speedtest.sk` or you can have a benchmark made with [hyperfine](https://github.com/sharkdp/hyperfine) (or other related tool) like so: `hyperfine "burlap tests/speedtest.sk" "python3 tests/speedtest.py"`

For profiling, I use [flamegraph-rs](https://github.com/flamegraph-rs/flamegraph), after you've installed that the command is: `CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph -- tests/speedtest.sk`.

## History

Burlap started because I wanted to learn how to create a programming language. I knew about Sack and thought it would be a perfect way to dive in (and it was!). Anyway, here's the change log:

- 1.2.3
    - Make lists Copy-On-Write
    - Reduce `Value` size to 16 bytes
    - Fix REPL duplication bug
    - Fix `VCALL` for user-defined functions
    - Fix eval order for lists and calls
    - Clean up lots of little things
- 1.2.2: Backtrace update
    - Add line numbers to runtime errors
    - Add the `--backtrace` flag
    - Fixed incorrect line numbers after multiline strings
- 1.2.1: Register update
    - Change backend from a stack-based one to a register and stack-based one
    - Add break/continue
    - Add `[:x]` syntax for lists
- 1.2.0: WASM update
    - Add WASM support
    - Add first class functions
    - Add function overloading
    - Add better docs about C-FFI
    - Check variable access and function calls (but not variable calls) before runtime
- 1.1.5
    - Add `args()`
    - Add `in` as a binop
    - Add `%=` operator
    - Add guaranteed tail recursion
    - Remove boolean and none math
    - Make short circuiting operators short circuit
    - Make `str` return none on failure
    - Fix major bug with jumping instructions
    - Fix returning from a loop
    - Fix empty import bug
- 1.1.4: Byte and file update
    - Add string indexing
    - Add string iterating
    - Add `seek(file, pos)`
    - Add `byte(x)` function
    - Make `write` not truncate
    - Make `len` zero indexed
    - Allow string casts to fail
    - Fixed compiler bug with the `PUSH3` instruction
- 1.1.3: REPL update
    - Make block errors less noisy
    - Add multiline editing, highlighting, and tab-completion to the REPL
- 1.1.2: C ffi update
    - Added C iterop
    - Add `__burlap_throw`
    - Fixed global variables for imports
- 1.1.1: File IO
    - Read, write, open, and flush
- 1.1.0: VM update
    - Rewrite tree walking interpreter into a stack based VM
- 1.0.0: Rusty updaye
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
