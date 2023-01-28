mod lexer;
mod common;

use crate::lexer::lex;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn repl() {
    let mut rl = Editor::<()>::new().unwrap();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = lex(&line, "<stdin>".to_string());
                println!("{:?}", tokens);
            },
            Err(ReadlineError::Eof) => {
                break;
            },
            _ => {}
        }
    }
}

fn main() {
    // TODO: Opening files
    repl();
}
