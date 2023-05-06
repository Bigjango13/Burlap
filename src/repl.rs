use crate::vm::{run, Vm};
use crate::lexer::{lex, TokenType};
use crate::compiler::compile;
use crate::common::{print_err, ErrType};
use crate::{Arguments, to_ast};

#[cfg(feature = "fancyrepl")]
use rustyline::validate::MatchingBracketValidator;
#[cfg(feature = "fancyrepl")]
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use home::home_dir;

#[cfg(feature = "fancyrepl")]
#[derive(Completer, Helper, Hinter, Validator)]
struct FancyRepl {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    color: bool,
    name: String,
}

#[cfg(feature = "fancyrepl")]
impl rustyline::highlight::Highlighter for FancyRepl {
    fn highlight<'a>(
        &self, line: &'a str, _pos: usize
    ) -> std::borrow::Cow<'a, str> {
        let tokens = lex(
            &line.to_string(), self.name.clone(), false, self.color
        );
        if tokens.len() == 0 {
            return std::borrow::Cow::Borrowed(line);
        }
        let mut ret = String::new();
        for token in tokens {
            // Get the color of the token
            let color = match token.token {
                // Misc keywords
                TokenType::Let | TokenType::Func(..)
                    | TokenType::Import | TokenType::Bool(..)
                    | TokenType::None => Some("\x1b[32m"),
                // Control flow
                TokenType::If | TokenType::Else | TokenType::In
                    | TokenType::Str(..) | TokenType::While
                    | TokenType::Loop => Some("\x1b[1;33m"),
                TokenType::Return => Some("\x1b[35m"),
                // Comments
                TokenType::Skipped => Some("\x1b[1;36m"),
                // Operators
                TokenType::EqualsEquals | TokenType::NotEquals
                | TokenType::Lt | TokenType::Gt | TokenType::LtEquals
                | TokenType::GtEquals | TokenType::PlusEquals
                | TokenType::MinusEquals | TokenType::TimesEquals
                | TokenType::DivEquals | TokenType::Plus | TokenType::Minus
                | TokenType::Times | TokenType::Div | TokenType::Modulo
                | TokenType::PlusPlus | TokenType::MinusMinus
                | TokenType::Not | TokenType::Equals | TokenType::And
                | TokenType::Or | TokenType::Xor
                | TokenType::Colon => Some("\x1b[31m"),
                // Numbers
                TokenType::Int(_) | TokenType::Float(_) => Some("\x1b[1;35m"),
                // Bytes
                TokenType::Byte(_) => Some("\x1b[1;34m"),
                // Builtin functions
                TokenType::Identifier(i) if vec![
                    "print", "input", "type", "len", "range",
                    "open", "close", "read", "write", "flush",
                    "int", "float", "string"
                ].contains(&i.as_str()) => Some("\x1b[1;36m"),
                // Lexer errors
                TokenType::Invalid => Some("\x1b[41;30m"),
                // Non-highlighted
                TokenType::Eof | TokenType::Newline | TokenType::Lparan
                | TokenType::Rparan | TokenType::Lbracket
                | TokenType::Lbrace | TokenType::Rbrace
                | TokenType::Rbracket | TokenType::Semicolon
                | TokenType::Comma | TokenType::Identifier(_) => None,
            };
            if let Some(c) = color {
                ret += c;
                ret += &token.str;
                ret += "\x1b[0m";
            } else {
                ret += &token.str;
            }
        }
        return std::borrow::Cow::Owned(ret);
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        return true;
    }
}

pub fn repl(args: &mut Arguments) {
    // Print welcome msg
    println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
    let mut rl = Editor::new().unwrap();
    #[cfg(feature = "fancyrepl")]
    rl.set_helper(Some(FancyRepl{
        brackets: MatchingBracketValidator::new(),
        name: args.name.clone(),
        color: args.extensions.contains(&"color".to_string()),
    }));
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
            rl.add_history_entry(line.clone())
                .expect("failed to add line to history");
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
