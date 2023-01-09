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
    stream.at++;
    char ret = getChar(stream);
    if (ret == '\n') {
        stream.lastLinePos = stream.at;
        stream.line++;
    }
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
        c = getNextChar(stream);
    }
    // Decimals
    if (c == '.') {
        ret.tok = TokenType::Float;
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
            ret.floatVal += (c - '0') / (divisor * 10);
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
        case '(': return Token(TokenType::Lparan, stream);
        case ')': return Token(TokenType::Rparan, stream);
        case '[': return Token(TokenType::Lbracket, stream);
        case ']': return Token(TokenType::Rbracket, stream);
        case '{': return Token(TokenType::Lbrance, stream);
        case '}': return Token(TokenType::Rbrace, stream);
        case ';': return Token(TokenType::Semicolon, stream);
        case ',': return Token(TokenType::Comma, stream);
        case '%': return Token(TokenType::Modulo, stream);

        // Math, mathmath, and math= ops, +, ++, +=, -, --, -=, *, *=, /, /=
        case '+': return Token((isNext(stream, '=') ?
            TokenType::Plus_equals : isNext(stream, '+') ?
                TokenType::Plus :
                TokenType::Plus_plus), stream);
        case '-': return Token((isNext(stream, '=') ?
            TokenType::Minus_equals : isNext(stream, '-') ?
                TokenType::Minus :
                TokenType::Minus_minus), stream);
        case '*': return Token((isNext(stream, '=') ?
            TokenType::Times_equals :
            TokenType::Times), stream);
        case '/': return Token((isNext(stream, '=') ?
            TokenType::Div_equals :
            TokenType::Div), stream);

        // Comparison ops, =, ==, !, !=, >, >=, <, <=
        case '=': return Token((isNext(stream, '=') ?
            TokenType::Equals_equals :
            TokenType::Equals), stream);
        case '!': return Token((isNext(stream, '=') ?
            TokenType::Not_equals :
            TokenType::Not), stream);
        case '>': return Token((isNext(stream, '=') ?
            TokenType::Gt_equals :
            TokenType::Gt), stream);
        case '<': return Token((isNext(stream, '=') ?
            TokenType::Lt_equals :
            TokenType::Lt), stream);

        // Logical ops, &&, ||, ^^
        case '&': return Token((isNext(stream, '&') ?
            TokenType::And :
            TokenType::Error), stream);
        case '|': return Token((isNext(stream, '|') ?
            TokenType::Or :
            TokenType::Error), stream);
        case '^': return Token((isNext(stream, '^') ?
            TokenType::Xor :
            TokenType::Error), stream);

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
            return lexIdentifier(stream);
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
    }
    return ret;
}
