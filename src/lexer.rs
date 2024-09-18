use logos::Logos;

use crate::common::{err, ErrType, Stream};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(subpattern numbers = r"[0-9]((_?[0-9])*)?")]
// The token enum
pub enum TokenType {
    // Special tokens
    Invalid,
    Eof,
    #[regex(r"(#[^\n]*)|([ \t\f\r]+)")]
    Skipped,
    #[token("\n")]
    Newline,
    // Literals/data types
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(
        "'[^']*'", |lex| lex.slice()[1..lex.slice().len()-1].to_string())
    ]
    #[regex(
        "\"[^\"]*\"", |lex| lex.slice()[1..lex.slice().len()-1].to_string()
    )]
    Str(String),
    #[regex(r"(?&numbers)", |lex| lex.slice().parse().ok())]
    Int(i32),
    #[regex(r"(?&numbers)\.(?&numbers)", |lex| lex.slice().parse().ok())]
    Float(f32),
    #[regex("(true)|(false)", |lex| lex.slice() == "true")]
    Bool(bool),
    #[token("none")]
    None,
    // Valid byte
    #[regex(
        "0b[01]([01]?){7}",
        |lex| u8::from_str_radix(&lex.slice()[2..], 2).ok()
    )]
    // Invalid byte
    #[regex(
        "0b[01]{8}[01]+", |_| Err(())
    )]
    Byte(u8),
    // Misc
    #[token("(")]
    Lparan,
    #[token(")")]
    Rparan,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,
    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,
    // Keywords
    #[regex("func(ti)?", |lex| lex.slice() == "functi")]
    Func(bool),
    #[token("let")]
    Let,
    #[token("return")]
    Return,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("loop")]
    Loop,
    #[token("while")]
    While,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("import")]
    Import,
    // Basic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Modulo,
    // Basic Operators =
    #[token("+=")]
    PlusEquals,
    #[token("-=")]
    MinusEquals,
    #[token("*=")]
    TimesEquals,
    #[token("/=")]
    DivEquals,
    #[token("%=")]
    ModEquals,
    // Logical operators
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("^^")]
    Xor,
    #[token("!")]
    Not,
    // Comparison operators
    #[token("==")]
    EqualsEquals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEquals,
    #[token(">=")]
    GtEquals
}

// Token, has a token plus everything needed for errors
#[derive(Clone)]
pub struct Token {
    pub stream: Stream,
    pub token: TokenType,
    pub str: String,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.token)
    }
}

pub fn lex(
    src: &str, name: String, print_err: bool, color: bool
) -> Option<Vec<Token>> {
    let mut lex = TokenType::lexer(src);
    let mut ret: Vec<Token> = vec![];
    // Lines
    let lines = src.lines().collect::<Vec<&str>>();
    if lines.is_empty() {
        return Some(vec![]);
    }
    // Stream (for errors)
    let mut stream = Stream{
        name, rat: 0, at: 0, line: 1, size: 0,
    };
    let mut lastat = 0;
    let mut tok = lex.next();
    while tok.is_some() {
        stream.size = lex.span().end - lex.span().start;
        stream.rat = lex.span().start;
        stream.at = lex.span().start - lastat;
        if tok.clone().unwrap().is_err() {
            if !print_err {
                // The REPL lexes for highlighting, not syntax
                ret.push(Token{
                    token: TokenType::Invalid,
                    stream: stream.clone(),
                    str: lex.slice().to_string()
                });
                tok = lex.next();
                continue;
            }
            err(&stream, "failure to lex", ErrType::Err, color);
            return None;
        } else {
            let token = tok.unwrap().unwrap();
            if token == TokenType::Newline {
                // Bump line
                stream.line += 1;
                lastat = lex.span().start + 1;
            }
            if let TokenType::Str(ref str) = token {
                // Lines can have multiple lines in them
                let newlines = str.matches("\n").count();
                stream.line += newlines;
                lastat = lex.span().start + newlines;
            }
            if let TokenType::Newline | TokenType::Skipped = token {
                if print_err {
                    tok = lex.next();
                    continue;
                }
            }
            ret.push(Token{
                token, stream: stream.clone(),
                str: lex.slice().to_string()
            });
        }
        tok = lex.next();
    }
    ret.push(Token{
        token: TokenType::Eof,
        stream,
        str: "".to_string(),
    });
    return Some(ret);
}
