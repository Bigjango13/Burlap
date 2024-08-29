#![allow(clippy::needless_return, clippy::print_literal)]

pub mod backend;
pub mod lexer;
pub mod parser;

#[macro_use]
extern crate impl_ops;

use std::path::PathBuf;

#[path = ""]
mod cfg_mod {
    pub mod common;

    pub use std::fs;
    pub use std::env;
    pub use std::process::exit;

    pub use crate::common::{print_err, ErrType};
}

use cfg_mod::*;

use crate::lexer::lex;
use crate::parser::{parse, AST};
use crate::backend::compiler::{compile, Compiler};
use crate::backend::vm::vm::{run, Vm};

#[derive(Clone, Default)]
pub struct Arguments {
    source: String,
    name: String,
    path: PathBuf,
    is_debug: bool,
    backtrace: bool,
    extensions: Vec<String>,
    program_args: Vec<String>
}

impl Arguments {
    pub fn new() -> Arguments {
        Arguments {
            source: "".to_string(), is_debug: false,
            extensions: vec!["color".to_string()],
            name: "<stdin>".to_string(), backtrace: false,
            program_args: vec![], path: PathBuf::from("."),
        }
    }
}

pub fn to_ast(args: &mut Arguments) -> Option<AST> {
    // Lex
    let tokens = lex(
        &args.source, args.name.clone(), true,
        args.extensions.contains(&"color".to_string()),
    )?;
    args.source = "".to_string();
    // Parse
    return parse(AST::new(), tokens, args);
}

fn get_args() -> Result<Arguments, bool> {
    let mut args = Arguments::new();
    let mut cli_args = env::args()
        .collect::<Vec<String>>().into_iter();
    let file: String = cli_args.nth(1).expect("No filename!!");
    args.name = file.clone();
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

#[allow(dead_code)]
fn main() {
    // Parse args
    let mut args = match get_args() {
        Ok(x) => x,
        Err(failed) => exit(!failed as i32),
    };
    // Run
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
    // Run
    let mut vm = Vm::new(args.clone(), compiler.program);
    if !run(&mut vm) {
        exit(2);
    }
}
