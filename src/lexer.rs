use crate::common::{err, ErrType, Stream};

// The token enum
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Special tokens
    Error(String), Eof, Blank,
    // Literals/data types
    Identifier(String), Str(String), Int(i32),
    Float(f32), Bool(bool), None,
    // Misc
    Lparan, Rparan, Lbracket, Rbracket,
    Lbrace, Rbrace, Semicolon, Comma, Equals,
    PlusPlus, MinusMinus,
    // Keywords
    Func, Let, Return, In, If, Else, Loop, While, Import,
    // Basic operators
    Plus, Minus, Times, Div, Modulo,
    // Basic Operators =
    PlusEquals, MinusEquals, TimesEquals, DivEquals,
    // Logical operators
    And, Or, Xor, Not,
    // Comparison operators
    EqualsEquals, NotEquals, Lt, Gt, LtEquals, GtEquals
}
// Token, has a token plus everything needed for errors
#[derive(Clone)]
pub struct Token {
    pub stream: Stream,
    pub token: TokenType
}
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Just pass through to token
        write!(f, "{:?}", self.token)
    }
}

// Get a char
fn get_chr(stream: &Stream) -> char {
    if stream.str.chars().count() <= stream.at {
        return '\0';
    }
    return stream.str.chars().nth(stream.at).unwrap();
}
// Get next char
fn next_chr(stream: &mut Stream) -> char {
    stream.at += 1;
    return get_chr(stream);
}
// is_next helper
fn is_next(stream: &mut Stream, c: char) -> bool {
    if get_chr(stream) == c {
        next_chr(stream);
        return true;
    }
    return false;
}

// Identifier/keywords
fn lex_identifier(stream: &mut Stream, _extentions: &[String]) -> TokenType {
    let mut chr = get_chr(stream);
    let mut name = "".to_string();
    // Invalid id
    if !chr.is_alphabetic() && chr != '_' {
        next_chr(stream);
        return TokenType::Error("invalid character while lexing".to_string());
    }
    // Read the id
    while chr.is_alphanumeric() || chr == '_' {
        name.push(chr);
        chr = next_chr(stream);
    }
    // Keywords
    let ret = match name.as_str() {
        "functi" | "func" => TokenType::Func,
        "let" => TokenType::Let,
        "return" => TokenType::Return,
        "in" => TokenType::In,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "loop" => TokenType::Loop,
        "while" => TokenType::While,
        "import" => TokenType::Import,
        "none" => TokenType::None,
        "true" => TokenType::Bool(true),
        "false" => TokenType::Bool(false),
        _ => TokenType::Identifier(name)
    };
    return ret;
}

// Strings
fn lex_str(stream: &mut Stream, end: char, extentions: &[String]) -> TokenType {
    let mut chr = get_chr(stream);
    let mut ret = "".to_string();
    // Read the string until the end
    while chr != end {
        if chr == '\0' {
            // End of line before end of string
            return TokenType::Error(
                format!("unterminated string, missing {}", end)
            );
        }
        if chr == '\\' && extentions.contains(&"escape".to_string()) {
            ret.push(match next_chr(stream) {
                'e' => '\x1b',
                'r' => '\r',
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                other => other
            })
        } else {
            ret.push(chr);
        }
        chr = next_chr(stream);
    }
    // Advance and return
    next_chr(stream);
    return TokenType::Str(ret);
}

// Numbers
fn lex_number(stream: &mut Stream, _extentions: &[String]) -> TokenType {
    let mut chr = get_chr(stream);
    let mut num = "".to_string();
    // Get the digits as a string
    while chr.is_ascii_digit() {
        num.push(chr);
        chr = next_chr(stream);
    }
    if chr != '.' {
        // It's an int, return it.
        return TokenType::Int(num.parse::<i32>().unwrap());
    }
    // It's a float
    num.push(chr);
    chr = next_chr(stream);
    if !chr.is_ascii_digit() {
        // Trailing period
        return TokenType::Error(
            "expected a number, Sack doesn't allow trailing periods on floats"
            .to_string()
        );
    }
    // Get the rest of the float
    while chr.is_ascii_digit() {
        num.push(chr);
        chr = next_chr(stream);
    }
    return TokenType::Float(num.parse::<f32>().unwrap());
}

// Get a single TokenType
fn get_tok(stream: &mut Stream, extentions: &[String]) -> TokenType {
    let chr = get_chr(stream);
    next_chr(stream);
    return match chr {
        // Misc single chars toks, (, ), [, ], {, }, ;, ,, %
        '(' => TokenType::Lparan,
        ')' => TokenType::Rparan,
        '[' => TokenType::Lbracket,
        ']' => TokenType::Rbracket,
        '{' => TokenType::Lbrace,
        '}' => TokenType::Rbrace,
        ';' => TokenType::Semicolon,
        ',' => TokenType::Comma,
        '%' => TokenType::Modulo,
        // Math, mathmath, and math= ops
        // +=, ++, +
        '+' => if is_next(stream, '=') {TokenType::PlusEquals}
            else if is_next(stream, '+') {TokenType::PlusPlus}
            else {TokenType::Plus},
        // -=, --, -
        '-' => if is_next(stream, '=') {TokenType::MinusEquals}
            else if is_next(stream, '-') {TokenType::MinusMinus}
            else {TokenType::Minus},
        // *=, *
        '*' => if is_next(stream, '=') {TokenType::TimesEquals}
            else {TokenType::Times},
        // /=, /
        '/' => if is_next(stream, '=') {TokenType::DivEquals}
            else {TokenType::Div},
        // Comparison ops
        // =, ==
        '=' => if is_next(stream, '=') {TokenType::EqualsEquals}
            else {TokenType::Equals},
        // !, !=
        '!' => if is_next(stream, '=') {TokenType::NotEquals}
            else {TokenType::Not},
        // <, <=
        '>' => if is_next(stream, '=') {TokenType::GtEquals}
            else {TokenType::Gt},
        // <, <=
        '<' => if is_next(stream, '=') {TokenType::LtEquals}
            else {TokenType::Lt},
        // Logical ops
        // &&
        '&' => if is_next(stream, '&') {TokenType::And}
            else {TokenType::Error(
                "invalid TokenType '&', did you mean '&&'?".to_string())
            },
        // ||
        '|' => if is_next(stream, '|') {TokenType::Or}
            else {TokenType::Error(
                "invalid token '|', did you mean '||'?".to_string())
            },
        // ^^
        '^' => if is_next(stream, '^') {TokenType::Xor}
            else {TokenType::Error(
                "invalid token '^', did you mean '^^'?".to_string())
            },
        // Strings
        '\'' | '"' => lex_str(stream, chr, extentions),
        // Numbers
        '0'..='9' => {stream.at -= 1; lex_number(stream, extentions)},
        // Whitespace/comments
        ' ' | '\t' | '\n' | '\r' | '\0' => TokenType::Blank,
        '#' => {
            while next_chr(stream) != '\0' {}
            TokenType::Blank
        }
        // Identifier
        _ => {
            stream.at -= 1;
            lex_identifier(stream, extentions)
        }
    };
}

pub fn lex(str: &str, name: String, extentions: &[String]) -> Vec<Token> {
    let mut stream = Stream{
        str: "".to_string(), name,
        line: 0, at: 0
    };
    let mut ret: Vec<Token> = Vec::new();
    let mut errors = false;
    for line in str.lines() {
        // Prep stream for new line
        stream.at = 0;
        stream.line += 1;
        stream.str = line.to_string();
        // Skip blank lines
        if stream.str.trim() == "" {
            continue;
        }
        // Tokenize the line
        while get_chr(&stream) != '\0' {
            let old_stream = stream.clone();
            let token = get_tok(&mut stream, extentions);
            if let TokenType::Error(msg) = token.clone() {
                errors = true;
                err(&old_stream, &msg, (stream.at - old_stream.at) as u8, ErrType::Err);
            }
            if let TokenType::Blank = token.clone() {
                continue;
            }
            if !errors {
                ret.push(Token{stream: old_stream, token});
            }
        }
    }
    // Return
    if errors {
        return Vec::new();
    }
    ret.push(Token{stream, token: TokenType::Eof});
    return ret;
}
