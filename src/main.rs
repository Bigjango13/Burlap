#![allow(clippy::needless_return, clippy::print_literal)]

pub mod backend;
pub mod lexer;
pub mod parser;
pub mod folding;

#[macro_use]
extern crate impl_ops;

use std::path::PathBuf;

#[cfg(not(target_family = "wasm"))]
#[path = ""]
mod cfg_mod {
    pub mod common;
    #[cfg(feature = "cffi")]
    pub mod cffi;
    #[cfg(feature = "repl")]
    pub mod repl;

    pub use std::fs;
    pub use std::env;
    pub use std::process::exit;

    #[cfg(feature = "repl")]
    pub use crate::repl::repl;
    pub use crate::common::{print_err, ErrType};
}
#[cfg(not(target_family = "wasm"))]
use crate::backend::vm::dis::dis;

#[cfg(target_family = "wasm")]
#[path = ""]
mod cfg_mod {
    pub use wasm_bindgen::prelude::*;
    #[macro_use] pub mod common;
    pub static mut THE_SOURCE: Option<String> = None;
}

use cfg_mod::*;

use crate::lexer::lex;
use crate::parser::{parse, AST};
use crate::backend::vm::compiler::{compile, Compiler};
use crate::backend::vm::vm::{run, Vm};

#[derive(Clone, Default)]
pub struct Arguments {
    source: String,
    name: String,
    path: PathBuf,
    is_debug: bool,
    is_repl: bool,
    backtrace: bool,
    dis: bool,
    // Extensions
    extension_color: bool,
    extension_auto_none: bool,
    extension_functies: bool,
    extension_va_print: bool,
    extension_debugging_functies: bool,
    // Arguments to the program itself
    program_args: Vec<String>
}

impl Arguments {
    pub fn new() -> Arguments {
        Arguments {
            source: "".to_string(), is_debug: false, dis: false,
            is_repl: true, name: "<stdin>".to_string(),
            backtrace: false, program_args: vec![],
            path: PathBuf::from("."),
            // Extensions
            extension_color: true,
            extension_auto_none: false,
            extension_functies: false,
            extension_va_print: false,
            extension_debugging_functies: false,
        }
    }
}

pub fn to_ast(args: &mut Arguments) -> Option<AST> {
    // Lex
    let tokens = lex(
        &args.source, args.name.clone(), true,
        args.extension_color,
    )?;
    args.source = "".to_string();
    // Parse
    return parse(AST::new(), tokens, args);
}

#[cfg(not(target_family = "wasm"))]
fn get_args() -> Result<Arguments, bool> {
    let mut args = Arguments::new();
    let mut file: String = "".to_string();
    let mut cli_args = env::args()
        .collect::<Vec<String>>().into_iter();
    // Skip first arg
    cli_args.next();
    while let Some(arg) = cli_args.next() {
        if !arg.starts_with('-') && file.is_empty() {
            // Files
            file = arg.to_string();
            args.name = arg;
            args.is_repl = false;
            break;
        } else if let Some(extension) = arg.strip_prefix("--use-") {
            // Extensions
            let extension = extension.to_string();
            if extension == "all" {
                // Set all extensions
                args.extension_color = true;
                args.extension_auto_none = true;
                args.extension_functies = true;
                args.extension_va_print = true;
                args.extension_debugging_functies = true;
            } else if extension == "color" {
                args.extension_color = true;
            } else if extension == "auto-none" {
                args.extension_auto_none = true;
            } else if extension == "burlap-extensions" {
                args.extension_functies = true;
            } else if extension == "va-print" {
                args.extension_va_print = true;
            } else if extension == "more-debug" {
                args.extension_debugging_functies = true;
            } else {
                print_err(
                    format!("unknown extension: {}", arg).as_str(),
                    ErrType::Warn,
                    args.extension_color
                );
            }
        } else if arg == "--no-color" {
            // Color is always the first argument
            args.extension_color = true;
        } else if arg == "-d" || arg == "--debug" {
            // Debug
            args.is_debug = true;
            args.extension_debugging_functies = true;
        } else if arg == "-a" || arg == "--disassemble" {
            // Disassemble
            args.dis = true;
        } else if arg == "-b" || arg == "--backtrace" {
            // Backtrace
            args.backtrace = true;
        } else if arg == "-h" || arg == "--help" {
            // Print help
            println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
            println!();
            println!("Usage:");
            println!("burlap <args> <file> <args for file>");
            println!();
            println!("Args:");
            println!("    -h --help         prints this help");
            println!("    --no-color        disables color");
            println!("    --use-X           enables X feature");
            println!("    --use-all         enables all features");
            println!("    - [command]       runs [command]");
            println!("    -d --debug        runs in debug mode");
            println!("    -b --backtrace    prints backtrace on runtime errors");
            println!("    -a --disassemble  prints disassembly instead of running");
            println!();
            println!(
                "Thank you for using Burlap! {}{}",
                "If there are any issues please report them to: ",
                env!("CARGO_PKG_REPOSITORY")
            );
            return Err(true);
        } else if arg == "-" && file.is_empty() {
            // Read source from command line
            args.is_repl = false;
            args.name = "<cli>".to_string();
            let Some(src) = cli_args.next() else {
                print_err(
                    "'-' is missing option source", ErrType::Err,
                    args.extension_color
                );
                return Err(false);
            };
            args.source = src.to_string();
        } else if arg == "--" {
            break;
        } else {
            // Anything else
            print_err(
                format!("unknown argument: {}", arg).as_str(), ErrType::Warn,
                args.extension_color
            );
        }
    }
    // Get the args to the program
    args.program_args.push(args.name.clone());
    for arg in cli_args {
        args.program_args.push(arg.to_string());
    }
    // Don't open files if source is filled or REPL
    if args.is_repl || !args.source.is_empty() {
        return Ok(args);
    }
    // Open file
    match fs::read_to_string(file) {
        Ok(v) => {
            args.source = v;
        },
        Err(err) => {
            // Report error
            print_err(
                format!("failed to open file: {}", err).as_str(), ErrType::Err,
                args.extension_color
            );
            return Err(false);
        }
    }
    return Ok(args);
}

#[cfg(not(target_family = "wasm"))]
#[allow(dead_code)]
fn main() {
    // Parse args
    let mut args = match get_args() {
        Ok(x) => x,
        Err(failed) => exit(!failed as i32),
    };
    // Run
    if args.is_repl {
        #[cfg(feature = "repl")]
        {
            // Repl
            repl(&mut args);
        }
        #[cfg(not(feature = "repl"))]
        {
            println!("You don't have the REPL enabled!");
            exit(1);
        }
    } else {
        args.path = PathBuf::from(args.name.clone());
        // Execute file
        let Some(mut ast) = to_ast(&mut args) else {
            exit(1);
        };
        let mut compiler = Compiler::new();
        // Fix import path
        compiler.program.path = args.path.clone();
        compiler.program.path.pop();
        if !compile(&mut ast, &Some(args.name.clone()), &mut compiler, false) {
            exit(1);
        }
        if args.dis {
            // Disassemble
            dis(&compiler.program, 0);
            exit(0);
        }
        // Run
        let mut vm = Vm::new(args.clone(), compiler.program);
        if !run(&mut vm) {
            exit(2);
        }
    }
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen]
pub fn setup_panics() {
    extern crate console_error_panic_hook;
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen]
pub fn burlap_run(src: &str) -> bool {
    let mut args = Arguments::new();
    args.extensions.pop();
    args.name = "<playground>".to_string();
    args.is_repl = false;
    unsafe {
        THE_SOURCE = Some(src.to_string());
        args.source = THE_SOURCE.as_ref().unwrap().clone();
    }
    let Some(mut ast) = to_ast(&mut args) else {
        return false;
    };
    let mut vm = Vm::new(args.clone());
    let mut compiler = Compiler::new();
    if !compile(&mut ast, &mut args, &mut compiler) {
        return false;
    }
    // Run
    run(&mut vm);
    return true;
}
