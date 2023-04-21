use logos::Logos;

use crate::Arguments;
use crate::common::{err, ErrType, Stream};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(subpattern numbers = r"[0-9]((_?[0-9])*)?")]
// Skips (whitespace/comments)
#[logos(skip r"#[^\n]*")]
#[logos(skip r"[ \t\f]+")]
// The token enum
pub enum TokenType {
    // Special tokens
    Eof,
    #[token("\n")]
    Newline,
    // Literals/data types
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("'[^']*'", |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
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
    #[token(">=")]
    LtEquals,
    #[token("<=")]
    GtEquals
}

// Token, has a token plus everything needed for errors
#[derive(Clone)]
pub struct Token {
    pub stream: Stream,
    pub token: TokenType
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.token)
    }
}

pub fn lex(args: &Arguments) -> Vec<Token> {
    let mut lex = TokenType::lexer(args.source.as_str());
    let mut ret: Vec<Token> = vec![];
    // Empty file
    let lines = args.source.lines().collect::<Vec<&str>>();
    if lines.is_empty() {
        return vec![];
    }
    // Stream (for errors)
    let mut stream = Stream{
        name: args.name.clone(), at: 0, line: 1, str: lines[0].to_string()
    };
    let mut lastat = 0;

    let mut tok = lex.next();
    while tok != None {
        stream.at = lex.span().start - lastat;
        if let Err(_) = tok.clone().unwrap() {
            let size = lex.span().end - lex.span().start;
            err(
                &stream, "failure to lex", size as u8, ErrType::Err,
                args.extensions.contains(&"color".to_string())
            );
            return vec![];
        } else {
            let token = tok.unwrap().unwrap();
            if token == TokenType::Newline {
                // Bump line
                stream.str = match lines.get(stream.line) {
                    Some(s) => *s,
                    None => ""
                }.to_string();
                stream.line += 1;
                lastat = lex.span().start;
            }
            ret.push(Token{token, stream: stream.clone()});
        }
        tok = lex.next();
    }
    ret.push(Token{
        token: TokenType::Eof,
        stream: stream
    });
    return ret;
}
