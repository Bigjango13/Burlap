// Stream
#[derive(Debug, Clone)]
pub struct Stream {
    // File name
    pub name: String,
    // Line contents
    pub str: String,
    // Line #
    pub line: usize,
    // Char pos in line
    pub at: usize,
    // Size of token
    pub size: usize,
}

pub const IMPOSSIBLE_STATE: &str =
    "we've reached an impossible state, anything is possible, \
    the limits were in our heads all along, follow your dreams";

// Errors
pub enum ErrType{Err, Warn, Hint}
// Prints and error and returns the color
pub fn print_err(msg: &str, errtype: ErrType, color: bool) -> String {
    // Get the name and color code from errtype
    let (color_code, name) = match errtype {
        // Red
        ErrType::Err => ("\x1b[1;31m", "error"),
        // Yellow
        ErrType::Warn => ("\x1b[1;33m", "warning"),
        // Cyan
        ErrType::Hint => ("\x1b[1;36m", "hint"),
    };
    if color {
        println!("{}{}:\x1b[0m {}", color_code.clone(), name, msg);
        return color_code.to_string();
    } else {
        println!("{}: {}", name, msg);
        return "".to_string();
    }
}

pub fn err(stream: &Stream, msg: &str, errtype: ErrType, color: bool) {
    // Print file name and line/char info ("test.sk:1:3: ")
    if color {
        print!("\x1b[1m{}:{}:{}:\x1b[0m ", stream.name, stream.line, stream.at);
    } else {
        print!("{}:{}:{}: ", stream.name, stream.line, stream.at);
    }
    // Print the type ("error:")
    let color_code = print_err(msg, errtype, color);
    // Print the line ("    1 | print("Hello World!");")
    let line = format!("    {} | ", stream.line);
    println!("{}{}", line, stream.str.replace("\t", "    "));
    // Print arrow ("      |   ^")
    // Adjust for tabs
    let at = stream.str[0..stream.at].matches('\t').count()*3 + stream.at;
    print!(
        "{}| {}",
        " ".repeat(line.len() - 2), " ".repeat(at)
    );
    if color {
        println!("{}{}\x1b[0m", color_code, "^".repeat(stream.size.into()));
    } else {
        println!("{}", "^".repeat(stream.size.into()));
    }
}
