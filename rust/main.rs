mod lexer;
mod parser;
mod common;
mod interpreter;
mod value;

use std::env;
use std::fs;
use std::process::exit;

#[macro_use] extern crate impl_ops;
use crate::lexer::lex;
use crate::parser::parse;
use crate::interpreter::{run, Interpreter};

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn repl(extentions: Vec<String>) {
    println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
    let mut rl = Editor::<()>::new().unwrap();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = lex(&(line + ";"), "<stdin>".to_string(), &extentions);
                if tokens.is_empty() {
                    continue;
                }
                //println!("Tokens: {:?}", tokens);
                let ast = parse(tokens, extentions.clone());
                //println!("AST:    {:?}", ast);
                run(&mut Interpreter{
                    is_repl: true, has_err: false, in_func: false
                }, ast);
            },
            Err(ReadlineError::Eof) => {
                break;
            },
            _ => {}
        }
    }
}

fn get_args() -> (String, Vec<String>) {
    let possible_extentions = vec!["escape", "auto-none"];
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

fn main() {
    let (file_name, extentions) = get_args();
    // Repl
    if file_name.is_empty() {
        repl(extentions);
        exit(0);
    }
    // Open file
    let file = fs::read_to_string(file_name.clone());
    if file.is_err() {
        println!("Failed to open file '{}': {}", file_name.clone(), file.err().unwrap());
        exit(1);
    }
    let contents = file.unwrap();
    // Run
    let tokens = lex(&contents, file_name, &extentions);
    if tokens.is_empty() {
        exit(1);
    }
    println!("Tokens: {:?}", tokens);
    let ast = parse(tokens, extentions.clone());
    println!("AST:    {:?}", ast);
}
