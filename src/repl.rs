use crate::vm::{run, Vm};
#[cfg(feature = "fancyrepl")]
use crate::lexer::{lex, TokenType};
use crate::compiler::compile;
use crate::common::{print_err, ErrType};
use crate::{Arguments, to_ast};

#[cfg(feature = "fancyrepl")]
use rustyline::validate::MatchingBracketValidator;
#[cfg(feature = "fancyrepl")]
use rustyline::{Helper, Hinter, Validator};
use rustyline::error::ReadlineError;
#[cfg(feature = "fancyrepl")]
use rustyline::Editor;
#[cfg(not(feature = "fancyrepl"))]
use rustyline::DefaultEditor;
use home::home_dir;

#[cfg(feature = "fancyrepl")]
#[derive(Helper, Hinter, Validator)]
struct FancyRepl {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    color: bool,
    name: String,
    symbols: Vec<String>
}

#[cfg(feature = "fancyrepl")]
impl rustyline::highlight::Highlighter for FancyRepl {
    fn highlight<'a>(
        &self, line: &'a str, _pos: usize
    ) -> std::borrow::Cow<'a, str> {
        // Don't use color if there isn't any
        if !self.color {
            return std::borrow::Cow::Borrowed(line);
        }
        // Lex
        let tokens = lex(
            &line.to_string(), self.name.clone(), false, self.color
        ).unwrap_or(vec![]);
        if tokens.is_empty() {
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
            // Add the color
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

// Completion
#[cfg(feature = "fancyrepl")]
// Wrapper around String because it implements traits)
struct Candidate {
    str: String
}
#[cfg(feature = "fancyrepl")]
impl rustyline::completion::Candidate for Candidate {
    fn display(&self) -> &str {
        self.str.as_str()
    }

    fn replacement(&self) -> &str {
        self.str.as_str()
    }
}

#[cfg(feature = "fancyrepl")]
impl rustyline::completion::Completer for FancyRepl {
    type Candidate = Candidate;
    fn complete(
        &self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>
    ) -> rustyline::Result<(usize, Vec<Candidate>)> {
        let tokens = lex(
            &line.to_string(), self.name.clone(), false, self.color
        ).unwrap_or(vec![]);
        let mut target: Option<String> = None;
        let mut start = 0;
        for token in tokens {
            if token.stream.rat + token.str.len() < pos {
                // Keep going
                continue;
            } else if token.stream.rat >= pos {
                // Went too far
                break;
            }
            // At the correct token
            start = token.stream.rat;
            if let TokenType::Identifier(mut str) = token.token {
                // Truncate so 'pr_int' is 'pr' and 'print_' is 'print', etc..
                str.truncate(pos - token.stream.rat);
                target = Some(str);
            } else {
                // Tab was pressed not on an identifier, so insert 4 spaces
                return Ok((
                    start + 1,
                    vec![Candidate{str: "    ".to_string()}]
                ));
            }
            break;
        }
        let mut matched: Vec<Candidate> = vec![];
        let Some(target) = target else {
            // No match
            return Ok((0, matched));
        };
        // Check symbols for matches
        for i in &self.symbols {
            if i.starts_with(&target) {
                matched.push(Candidate{str: i.clone()});
            }
        }
        return Ok((start, matched));
    }
}

pub fn repl(args: &mut Arguments) {
    // Print welcome msg
    println!("Burlap v{}", env!("CARGO_PKG_VERSION"));
    let mut vm = Vm::new(args.clone());
    #[cfg(feature = "fancyrepl")]
    let mut rl = Editor::new().unwrap();
    #[cfg(not(feature = "fancyrepl"))]
    let mut rl = DefaultEditor::new().unwrap();
    #[cfg(feature = "fancyrepl")]
    // Helpers
    rl.set_helper(Some(FancyRepl{
        brackets: MatchingBracketValidator::new(),
        name: args.name.clone(),
        color: args.extensions.contains(&"color".to_string()),
        symbols: vm.get_symbols(true)
    }));
    // Try to get the home dir
    let hist_file = match home_dir() {
        Some(path) =>
            path.into_os_string().into_string().unwrap() + "/.burlap_history",
        None => "".to_string(),
    };
    // Load history
    if !hist_file.is_empty() && rl.load_history(&hist_file).is_err() {
        let color = args.extensions.contains(&"color".to_string());
        print_err("failed to open history file", ErrType::Warn, color);
        print_err(
            "create `~/.burlap_history` if you want history to save.",
            ErrType::Hint, color
        );
    };
    // REPL loop
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
            // Reset file name (imports mess it up during compiling)
            args.name = "<stdin>".to_string();
            // Run
            if vm.at != 0 {
                vm.at += 1;
            }
            run(&mut vm);
            // Update symbols
            #[cfg(feature = "fancyrepl")]
            {
                rl.helper_mut().unwrap().symbols = vm.get_symbols(true);
            }
        }
    }
    // Save history
    if !hist_file.is_empty() && rl.save_history(&hist_file).is_err() {
        print_err(
            "failed to save history", ErrType::Warn,
            args.extensions.contains(&"color".to_string())
        );
    }
}
