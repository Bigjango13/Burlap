#![allow(clippy::needless_return, clippy::print_literal)]
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::exit;

mod common;
mod lexer;
mod parser;
mod value;
mod compiler;
mod vm;

use crate::lexer::lex;
use crate::parser::{parse, ASTNode};
use crate::compiler::compile;
use crate::common::{print_err, ErrType};
use crate::vm::{run, Vm};

#[macro_use] extern crate impl_ops;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use home::home_dir;

#[derive(Clone)]
pub struct Arguments {
    source: String,
    name: String,
    is_debug: bool,
    is_repl: bool,
    extensions: Vec<String>
}

impl Arguments {
    pub fn new() -> Arguments {
        Arguments {
            source: "".to_string(), is_debug: false,
            is_repl: true, extensions: vec!["color".to_string()],
            name: "<stdin>".to_string()
        }
    }
}

pub fn to_ast(args: &mut Arguments) -> Option<Vec<ASTNode>> {
    // Lex
    let tokens = lex(args);
    if tokens.is_empty() {
        return None;
    }
    // Parse
    let ast = parse(tokens, args);
    if ast.is_empty() {
        return None;
    }
    if args.is_debug {
        // Debug print ast
        println!("Ast: {:?}", ast);
    }
    return Some(ast);
}

fn repl(args: &mut Arguments) {
    // Print welcome msg
    println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
    let mut rl = Editor::<()>::new().unwrap();
    // Try to get the home dir
    let hist_file = match home_dir() {
        Some(path) =>
            path.into_os_string().into_string().unwrap() + "/.burlap_history",
        None => "".to_string(),
    };
    // Load history
    if hist_file != "" && rl.load_history(&hist_file).is_err() {
        let color = args.extensions.contains(&"color".to_string());
        print_err("failed to open history file", ErrType::Warn, color);
        print_err(
            "create `~/.burlap_history` if you want history to save.",
            ErrType::Hint, color
        );
    };
    // REPL loop
    let mut vm = Vm::new(args.clone());
    loop {
        // Get input
        let readline = rl.readline(">> ");
        // C-d to exit
        if let Err(ReadlineError::Eof) = readline {
            break;
        }
        // Input
        if let Ok(line) = readline {
            // Add to history
            rl.add_history_entry(line.clone());
            args.source = line + ";";
            // Gen ast
            let Some(ast) = to_ast(args) else {
                continue;
            };
            args.source = "".to_string();
            // Compile
            if !compile(ast, args, &mut vm.program) {
                continue;
            }
            // Run
            if vm.at != 0 {
                vm.at += 1;
            }
            run(&mut vm);
        }
    }
    // Save history
    if hist_file != "" && rl.save_history(&hist_file).is_err() {
        print_err(
            "failed to save history", ErrType::Warn,
            args.extensions.contains(&"color".to_string())
        );
    }
}

fn get_args() -> Result<Arguments, bool> {
    let mut args = Arguments::new();
    let mut file: String = "".to_string();
    let mut cmd_from_cli: bool = false;
    // [1..] to skip the first arg
    for arg in &env::args().collect::<Vec<String>>()[1..] {
        if cmd_from_cli {
            args.source = arg.to_string();
            cmd_from_cli = false;
        } else if !arg.starts_with('-') && file.is_empty() {
            // Files
            file = arg.to_string();
            args.name = arg.to_string();
            args.is_repl = false;
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
        } else if arg == "-d" || arg == "--debug" {
            // Debug
            args.is_debug = true;
        } else if arg == "-h" || arg == "--help" {
            // Print help
            println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
            println!();
            println!("Usage:");
            println!("burlap <args> <file>");
            println!();
            println!("Args:");
            println!("\t-h --help\tprints help");
            println!("\t--no-color\tdisables color");
            println!("\t--use-X\tenables X feature");
            println!("\t--use-all\tenables all features");
            println!("\t- [command]\truns [command]");
            println!("\t-d --debug\truns in debug mode");
            println!();
            println!(
                "Thank you for using burlap! {}{}",
                "If there are any issues please report them at ",
                env!("CARGO_PKG_REPOSITORY")
            );
            return Err(true);
        } else if arg == "-" {
            // Read source from command line
            args.name = "<cli>".to_string();
            cmd_from_cli = file.is_empty();
            args.is_repl = false;
        } else {
            // Anything else
            print_err(
                format!("unknown argument: {}", arg).as_str(), ErrType::Warn,
                args.extensions.contains(&"color".to_string())
            );
        }
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

fn main() {
    // Parse args
    let mut args = match get_args() {
        Ok(x) => x,
        Err(failed) => exit(!failed as i32),
    };
    // Run
    if args.is_repl {
        // Repl
        repl(&mut args);
    } else {
        // Execute file
        let Some(ast) = to_ast(&mut args) else {
            exit(1);
        };
        args.source = "".to_string();
        let mut vm = Vm::new(args.clone());
        // Fix import path
        vm.program.path = PathBuf::from(args.name.clone());
        vm.program.path.pop();
        if !compile(ast, &mut args, &mut vm.program) {
            exit(1);
        }
        // Run
        if !run(&mut vm) {
            exit(1);
        }
    }
}
