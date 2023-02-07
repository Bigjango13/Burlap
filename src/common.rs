use crate::lexer::TokenType::*;
use crate::lexer::TokenType;

// Stream
#[derive(Debug, Clone)]
pub struct Stream {
    // File name
    pub name: String,
    // Line contents
    pub str: String,
    // Line #
    pub line: usize,
    // Char pos
    pub at: usize
}

pub const IMPOSSIBLE_STATE: &str =
    "we've reached an impossible state, anything is possible, \
    the limits were in our heads all along, follow your dreams";

// Errors
#[allow(dead_code)]
pub enum ErrType{Err, Warn, Hint}
pub fn err(stream: &Stream, msg: &str, size: u8, errtype: ErrType) {
    // Print file name and line/char info ("test.sk:1:3: ")
    print!("\x1b[1m{}:{}:{}:\x1b[0m ", stream.name, stream.line, stream.at);
    // Get the name and color code from errtype
    let (color, name) = match errtype {
        // Red
        ErrType::Err => ("\x1b[1;31m", "error"),
        // Yellow
        ErrType::Warn => ("\x1b[1;33m", "warning"),
        // Cyan
        ErrType::Hint => ("\x1b[1;36m", "hint"),
    };
    // Print the type ("error:")
    println!("{}{}:\x1b[0m {}", color, name, msg);
    // Print the line ("    1 | print("Hello World!");")
    let line = format!("    {} | ", stream.line);
    println!("{}{}", line, stream.str);
    // Print arrow ("      |   ^")
    println!(
        "{}| {}{}{}\x1b[0m",
        " ".repeat(line.len() - 2), " ".repeat(stream.at), color,
        "^".repeat(size.into())
    );
}

// Token size finder
pub fn get_len(token: &TokenType) -> u8 {
    return match token {
        Identifier(str) => str.len().try_into().unwrap_or(1),
        Str(str) => str.len().try_into().unwrap_or(0) + 2,
        Int(num) => num.to_string().len().try_into().unwrap_or(1),
        Float(num) => num.to_string().len().try_into().unwrap_or(1),
        Bool(val) => if *val { 4 } else { 5 },
        None => 4,
        // Keywords
        Func => 4,
        Let => 3,
        Return => 6,
        In => 2,
        If => 2,
        Else => 4,
        Loop => 4,
        Import => 6,
        // Size two ops
        PlusPlus | MinusMinus | PlusEquals | MinusEquals | TimesEquals |
        DivEquals | EqualsEquals | NotEquals | LtEquals | GtEquals => {
            2
        }
        // Anything else
        _ => 1
    }
}
