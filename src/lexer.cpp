#include <cmath>
#include <string>
#include <vector>

#include "lexer.h"

// Current char
static char getChar(stream_t &stream) {
    if (stream.str.size() <= stream.at) {
        return EOF;
    }
    return stream.str[stream.at];
}

// Advance char
static char getNextChar(stream_t &stream) {
    if (getChar(stream) == '\n') {
        stream.lastLinePos = stream.at + 1;
        stream.line++;
    }
    stream.at++;
    char ret = getChar(stream);
    return ret;
}

// Advance only if char matches
static bool isNext(stream_t &stream, char c) {
    if (getChar(stream) == c) {
        getNextChar(stream);
        return true;
    }
    return false;
}

static void push_back(stream_t &stream) {
    stream.at -= stream.at > 0;
}

// Read string
static Token lexStr(stream_t &stream, char end) {
    Token ret(TokenType::String, stream);
    char c = getChar(stream);
    while (
        c != end &&
        c != '\n' &&
        c != EOF
    ) {
        // Append
        ret.str += std::string(1, c);
        c = getNextChar(stream);
    }
    // Errors
    if (c == EOF || c == '\n') {
        err(
            ErrType::error,
            (c == EOF ?
                "unexpected EOF while parsing string (expected '"
                : "unexpected newline while parsing string (expected '\\"
            ) + std::string(1, end) + "')",
            stream
        );
        ret.errHandled = true;
        ret.tok = TokenType::Error;
    }
    // Clean up and return
    getNextChar(stream);
    return ret;
}

// Read number
static Token lexNum(stream_t &stream) {
    Token ret(TokenType::Int, stream);
    char c = getChar(stream);
    while (isdigit(c)) {
        // Add
        ret.numVal *= 10;
        ret.numVal += c - '0';
        ret.str += std::string(1, c);
        c = getNextChar(stream);
    }
    // Decimals
    if (c == '.') {
        ret.tok = TokenType::Float;
        ret.str += ".";
        c = getNextChar(stream);
        // Trailing period error
        if (!isdigit(c)) {
            err(ErrType::error, "expected a number", stream);
            err(
                ErrType::hint,
                "sack doesn't allow trailing periods, try adding a zero after",
                stream
            );
            ret.errHandled = true;
            ret.tok = TokenType::Error;
            return ret;
        }
        // Add
        ret.floatVal = ret.numVal;
        int divisor = 1;
        while (isdigit(c)) {
            ret.floatVal += (c - '0') / (float)pow(10, divisor);
            ret.str += std::string(1, c);
            divisor++;
            c = getNextChar(stream);
        }
    }
    return ret;
}

// Read identifier
static Token lexIdentifier(stream_t &stream) {
    char c = getChar(stream);
    if (!isalpha(c) && c != '_') return Token(TokenType::Error, stream);
    Token ret(TokenType::Identifier, stream);
    while (isalnum(c) || c == '_') {
        ret.str += std::string(1, c);
        c = getNextChar(stream);
    }
    return ret;
}

// Get the next token from the stream
static Token getTok(stream_t &stream) {
    char c = getChar(stream);
    getNextChar(stream);
    switch (c) {
        // Misc single chars toks, (, ), [, ], {, }, ;, ,, %
        case '(': return Token(TokenType::Lparan, stream, "(");
        case ')': return Token(TokenType::Rparan, stream, ")");
        case '[': return Token(TokenType::Lbracket, stream, "[");
        case ']': return Token(TokenType::Rbracket, stream, "]");
        case '{': return Token(TokenType::Lbrace, stream, "{");
        case '}': return Token(TokenType::Rbrace, stream, "}");
        case ';': return Token(TokenType::Semicolon, stream, ";");
        case ',': return Token(TokenType::Comma, stream, ",");
        case '%': return Token(TokenType::Modulo, stream, "%");

        // Math, mathmath, and math= ops, +, ++, +=, -, --, -=, *, *=, /, /=
        // +=, ++, +
        case '+': return isNext(stream, '=') ?
            Token(TokenType::Plus_equals, stream, "+=")
                : isNext(stream, '+') ?
                Token(TokenType::Plus_plus, stream, "++") :
                Token(TokenType::Plus, stream, "+");
        // -=, --, -
        case '-': return isNext(stream, '=') ?
            Token(TokenType::Minus_equals, stream, "-=")
                : isNext(stream, '-') ?
                Token(TokenType::Minus_minus, stream, "--") :
                Token(TokenType::Minus, stream, "-");
        // *=, *
        case '*': return isNext(stream, '=') ?
            Token(TokenType::Times_equals, stream, "*=") :
            Token(TokenType::Times, stream, "*");
        // *=, *
        case '/': return isNext(stream, '=') ?
            Token(TokenType::Div_equals, stream, "/=") :
            Token(TokenType::Div, stream, "/");

        // Comparison ops, =, ==, !, !=, >, >=, <, <=
        // ==, =
        case '=': return isNext(stream, '=') ?
            Token(TokenType::Equals_equals, stream, "==") :
            Token(TokenType::Equals, stream, "=");
        // !=, !
        case '!': return isNext(stream, '=') ?
            Token(TokenType::Not_equals, stream, "!=") :
            Token(TokenType::Not, stream, "!");
        // >=, >
        case '>': return isNext(stream, '=') ?
            Token(TokenType::Gt_equals, stream, ">=") :
            Token(TokenType::Gt, stream, ">");
        // <=, <
        case '<': return isNext(stream, '=') ?
            Token(TokenType::Lt_equals, stream, "<=") :
            Token(TokenType::Lt, stream, "<");

        // Logical ops, &&, ||, ^^
        // &&
        case '&': return isNext(stream, '&') ?
            Token(TokenType::And, stream, "&&") :
            Token(TokenType::Error, stream, "<lexing error>");
        // ||
        case '|': return isNext(stream, '|') ?
            Token(TokenType::Or, stream, "||") :
            Token(TokenType::Error, stream, "<lexing error>");
        // ^^
        case '^': return isNext(stream, '^') ?
            Token(TokenType::Xor, stream, "^^") :
            Token(TokenType::Error, stream, "<lexing error>");

        // Comments
        case '#':
            while (!isNext(stream, '\n') && getChar(stream) != EOF) {
                getNextChar(stream);
            }
            return getTok(stream);
        // Strings
        case '"':
            return lexStr(stream, '"');
        case '\'':
            return lexStr(stream, '\'');
        // Numbers
        case '0' ... '9':
            push_back(stream);
            return lexNum(stream);

        // Whitespace
        case '\n':
        case ' ':
        case '\r':
        case '\t':
            return getTok(stream);

        // EOF
        case EOF:
            return Token(TokenType::Eof, stream);

        // Identifiers and invalid tokens
        default:
            push_back(stream);
            Token ret = lexIdentifier(stream);
            #ifdef FUNCTI
            if (ret.str == "functi") {
            #else
            if (ret.str == "func") {
            #endif
                ret.tok = TokenType::Func;
            } else if (ret.str == "let") {
                ret.tok = TokenType::Let;
            } else if (ret.str == "return") {
                ret.tok = TokenType::Return;
            } else if (ret.str == "in") {
                ret.tok = TokenType::In;
            } else if (ret.str == "if") {
                ret.tok = TokenType::If;
            } else if (ret.str == "else") {
                ret.tok = TokenType::Else;
            } else if (ret.str == "loop") {
                ret.tok = TokenType::Loop;
            } else if (ret.str == "import") {
                ret.tok = TokenType::Import;
            } else if (ret.str == "none") {
                ret.tok = TokenType::None;
            } else if (ret.str == "true" || ret.str == "false") {
                ret.tok = TokenType::Bool;
            }
            return ret;
    }
}

// Tokenize an entire file
std::vector<Token> tokenize(stream_t &stream) {
    std::vector<Token> ret = {};
    while (getChar(stream) != EOF) {
        Token token = getTok(stream);
        // Syntax errors
        if (token.tok == TokenType::Error) {
            if (token.errHandled == false)
                err(ErrType::error, "illegal symbol while parsing", stream);
            return {};
        }
        ret.push_back(token);
    }
    ret.push_back(Token(TokenType::Eof, stream));
    return ret;
}
