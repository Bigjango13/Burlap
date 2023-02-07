use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::exit;

mod common;
mod interpreter;
mod lexer;
mod parser;
mod value;

use crate::lexer::lex;
use crate::parser::parse;
use crate::interpreter::{run, Interpreter};

#[macro_use] extern crate impl_ops;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use home::home_dir;

fn repl(extentions: Vec<String>) {
    // Print welcome msg
    println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
    let mut rl = Editor::<()>::new().unwrap();
    // Try to get the home dir
    let hist_file = match home_dir() {
        Some(path) => path.into_os_string().into_string().unwrap() + "/.burlap_history",
        None => "".to_string(),
    };
    // Load history
    if hist_file != "" && rl.load_history(&hist_file).is_err() {
        println!("Failed to open history file, make `~/.burlap_history` if you want histroy to save.");
    };
    let mut interpreter = Interpreter::new(true, PathBuf::from("."), extentions.clone());
    loop {
        // Get input
        let readline = rl.readline(">> ");
        match readline {
            // Execute line
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = lex(&(line + ";"), "<stdin>".to_string(), &extentions);
                if tokens.is_empty() {
                    continue;
                }
                let ast = parse(tokens, extentions.clone());
                if ast.is_empty() {
                    continue;
                }
                run(&mut interpreter, ast);
            },
            // Exit on EOF
            Err(ReadlineError::Eof) => {
                break;
            },
            _ => {}
        }
    }
    // Save history
    if hist_file != "" {
        if rl.save_history(&hist_file).is_err() {};
    }
}

fn get_args() -> (String, Vec<String>) {
    let possible_extentions = vec!["escape", "auto-none", "va-print"];
    let mut extentions: Vec<String> = vec![];
    let mut file: String = "".to_string();
    let args: Vec<String> = env::args().collect();
    // &args[1..] is to skip the first arg
    for arg in &args[1..] {
        if !arg.starts_with("-use-") {
            if !arg.starts_with('-') && file.is_empty() {
                file = arg.to_string();
                continue;
            }
            println!("Unknown argument: {}", arg);
            continue;
        }
        let extention = arg[5..].to_string();
        if extention == "all" {
            possible_extentions.iter().for_each(|i| extentions.push(i.to_string()));
            continue;
        }
        if !possible_extentions.contains(&extention.as_str()) {
            println!("Invalid extention: {}", extention);
            continue;
        }
        extentions.push(extention);
    }
    return (file, extentions);
}

fn import_file(interpreter: &mut Interpreter, path: &mut PathBuf) -> bool {
    // Open file
    let file_name = path.to_str().unwrap().to_string();
    let file = fs::read_to_string(path.clone());
    if file.is_err() {
         return false;
    }
    let contents = file.unwrap();
    // Set interpreter import path
    path.pop();
    let cur_path = interpreter.import_path.clone();
    interpreter.import_path = path.to_path_buf();
    // Run
    let tokens = lex(&contents, file_name, &interpreter.extentions);
    if tokens.is_empty() {
        return false;
    }
    let ast = parse(tokens, interpreter.extentions.clone());
    if ast.is_empty() {
        return false;
    }
    if !run(interpreter, ast) {
        return false;
    }
    // Reset import path
    interpreter.import_path = cur_path;
    return true;
}

fn main() {
    let (file_name, extentions) = get_args();
    // Repl
    if file_name.is_empty() {
        repl(extentions);
        exit(0);
    }
    // Run the file
    let mut path = PathBuf::from(file_name.clone());
    let mut import_path = path.clone();
    import_path.pop();
    let mut interpreter = Interpreter::new(false, import_path, extentions);
    if !import_file(&mut interpreter, &mut path) {
        exit(1);
    }
}
