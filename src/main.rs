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
use crate::parser::parse;
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

fn execute(vm: &mut Vm, args: &mut Arguments) -> bool {
    // Lex
    let tokens = lex(args);
    if tokens.is_empty() {
        return false;
    }
    // Parse
    let ast = parse(tokens, args);
    if ast.is_empty() {
        return false;
    }
    if args.is_debug {
        // Debug print ast
        println!("Ast: {:?}", ast);
    }
    // Compile
    let Some(program) = compile(ast) else {
        return false;
    };
    // Run!
    return run(vm, program);
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
        print_err("failed to open history file", ErrType::Warn);
        print_err(
            "create `~/.burlap_history` if you want history to save.", ErrType::Hint
        );
    };
    // REPL loop
    let mut vm = Vm::new(args.clone(), PathBuf::from("."));
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
            execute(&mut vm, args);
        }
    }
    // Save history
    if hist_file != "" {
        if rl.save_history(&hist_file).is_err() {
            print_err("failed to save history", ErrType::Err);
        };
    }
}

fn get_args() -> Option<Arguments> {
    let mut args = Arguments{
        source: "".to_string(), is_debug: false,
        is_repl: true, extensions: vec![],
        name: "<stdin>".to_string()
    };
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
        } else if arg.starts_with("-use-") {
            // Extensions
            let extension = arg[5..].to_string();
            args.extensions.push(extension);
        } else if arg == "-d" {
            // Debug
            args.is_debug = true;
        } else if arg == "-" {
            // Read source from command line
            args.name = "<cli>".to_string();
            cmd_from_cli = file.is_empty();
            args.is_repl = false;
        } else {
            // Anything else
            print_err(
                format!("unknown argument: {}", arg).as_str(), ErrType::Warn
            );
        }
    }
    // Don't open files if source is filled or REPL
    if args.is_repl || !args.source.is_empty() {
        return Some(args);
    }
    // Open file
    match fs::read_to_string(file) {
        Ok(v) => {
            args.source = v;
        },
        Err(err) => {
            // Report error
            print_err(
                format!("failed to open file: {}", err).as_str(), ErrType::Warn
            );
            return None;
        }
    }
    return Some(args);
}

fn main() {
    // Parse args
    let Some(mut args) = get_args() else {
        exit(1);
    };
    // Run
    if args.is_repl {
        // Repl
        repl(&mut args);
    } else {
        // Execute file
        let mut vm = Vm::new(args.clone(), PathBuf::from("."));
        if !execute(&mut vm, &mut args) {
            exit(1);
        }
    }
}
