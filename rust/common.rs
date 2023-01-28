// Stream
#[derive(Debug, Clone)]
pub struct Stream {
    pub str: String,
    pub name: String,
    pub line: usize,

    pub at: usize
}

// Errors
#[allow(dead_code)]
pub enum ErrType{Err, Warn, Note}
pub fn err(stream: &Stream, msg: &str, errtype: ErrType) {
    // Print file name and line/char info ("test.sk:1:3: ")
    print!("\x1b[1m{}:{}:{}:\x1b[0m ", stream.name, stream.line, stream.at);
    // Get the name and color code from errtype
    let (color, name) = match errtype {
        // Red
        ErrType::Err => ("\x1b[1;31m", "error"),
        // Yellow
        ErrType::Warn => ("\x1b[1;33m", "warning"),
        // Cyan
        ErrType::Note => ("\x1b[1;36m", "note"),
    };
    // Print the type ("error:")
    println!("{}{}:\x1b[0m {}", color, name, msg);
    // Print the line ("    1 | print("Hello World!");")
    let line = format!("    {} | ", stream.line);
    println!("{}{}", line, stream.str);
    // Print arrow ("      |   ^")
    println!(
        "{}| {}{}^\x1b[0m",
        " ".repeat(line.len() - 2), " ".repeat(stream.at - 1), color
    );
}
