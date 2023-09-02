#![allow(clippy::needless_return, clippy::print_literal)]

mod compiler;
mod lexer;
mod parser;
mod value;
mod vm;

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

#[cfg(target_family = "wasm")]
#[path = ""]
mod cfg_mod {
    pub use wasm_bindgen::prelude::*;
    #[macro_use] pub mod common;
    pub static mut THE_SOURCE: Option<String> = None;
}

use cfg_mod::*;

use crate::compiler::{compile, Compiler};
use crate::lexer::lex;
use crate::parser::{parse, StmtNode};
use crate::vm::{run, Vm};

#[derive(Clone)]
pub struct Arguments {
    source: String,
    name: String,
    path: PathBuf,
    is_debug: bool,
    is_repl: bool,
    format: bool,
    extensions: Vec<String>,
    program_args: Vec<String>
}

impl Arguments {
    pub fn new() -> Arguments {
        Arguments {
            source: "".to_string(), is_debug: false,
            is_repl: true, extensions: vec!["color".to_string()],
            name: "<stdin>".to_string(), format: false,
            program_args: vec![], path: PathBuf::from(".")
        }
    }
}

pub fn to_ast(args: &mut Arguments, functis: Option<&mut Vec<(String, i32)>>) -> Option<Vec<StmtNode>> {
    // Lex
    let Some(tokens) = lex(
        &args.source, args.name.clone(), true,
        args.extensions.contains(&"color".to_string()),
    ) else {
        return None;
    };
    args.source = "".to_string();
    // Parse
    let Some((ast, mut new_functis)) = parse(tokens, args) else {
        return None;
    };
    if functis != None {
        // Add to function list
        functis.unwrap().append(&mut new_functis);
    }
    if args.is_debug {
        // Debug print ast
        println!("Ast: {:?}", ast);
    }
    return Some(ast);
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
            args.name = arg.to_string();
            args.is_repl = false;
            break;
        } else if arg.starts_with("--use-") {
            // Extensions
            let extension = arg[6..].to_string();
            if extension == "all" {
                // Push all extensions
                args.extensions.push("auto-none".to_string());
                args.extensions.push("burlap-extensions".to_string());
            } else {
                args.extensions.push(extension);
            }
        } else if arg == "--no-color" {
            // Color is always the first argument
            args.extensions.remove(0);
        } else if arg == "-f" || arg == "--format" {
            // Format
            args.format = true;
        } else if arg == "-d" || arg == "--debug" {
            // Debug
            args.is_debug = true;
        } else if arg == "-h" || arg == "--help" {
            // Print help
            println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
            println!();
            println!("Usage:");
            println!("burlap <args> <file> <args for file>");
            println!();
            println!("Args:");
            println!("\t-h --help\tprints help");
            println!("\t--no-color\tdisables color");
            println!("\t--use-X\tenables X feature");
            println!("\t--use-all\tenables all features");
            println!("\t- [command]\truns [command]");
            //println!("\t-f --format\tformat the file instead of running");
            println!("\t-d --debug\truns in debug mode");
            println!();
            println!(
                "Thank you for using burlap! {}{}",
                "If there are any issues please report them at ",
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
                    args.extensions.contains(&"color".to_string())
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
                args.extensions.contains(&"color".to_string())
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
                args.extensions.contains(&"color".to_string())
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
            if args.format {
                print_err(
                    "formatting requires a file", ErrType::Err,
                    args.extensions.contains(&"color".to_string())
                );
                exit(1);
            }
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
        let Some(ast) = to_ast(&mut args, None) else {
            exit(1);
        };
        let mut compiler = Compiler::new();
        // Fix import path
        compiler.program.path = args.path.clone();
        compiler.program.path.pop();
        if !compile(ast, &mut args, &mut compiler) {
            exit(1);
        }
        // Run
        let mut vm = Vm::new(args.clone(), compiler.program);
        if !run(&mut vm) {
            exit(1);
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
    let Some(ast) = to_ast(&mut args, None) else {
        return false;
    };
    let mut vm = Vm::new(args.clone());
    let mut compiler = Compiler::new(vm.program);
    if !compile(ast, &mut args, &mut compiler) {
        return false;
    }
    // Run
    run(&mut vm);
    return true;
}
