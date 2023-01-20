#pragma once

#include <vector>

#include "common.h"

enum TokenType {
    // Special tokens
    Error, Eof,

    // Literal data types
    Identifier, String, Int, Float, Bool, None,

    // Misc
    Lparan, Rparan, Lbracket, Rbracket,
    Lbrance, Rbrace, Semicolon, Comma, Equals,
    Plus_plus, Minus_minus,

    // Keywords (print is temporary)
    Func, Let, Return, Print, If, Elif, Else, Loop, Import,

    // Basic operators
    Plus, Minus, Times, Div, Modulo,

    // Basic Operators =
    Plus_equals, Minus_equals, Times_equals, Div_equals,

    // Logical operators
    And, Or, Xor, Not,

    // Comparison operators
    Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals
};

struct Token {
    TokenType tok = TokenType::Error;

    std::string str = "";
    int numVal = 0;
    float floatVal = 0.F;

    bool errHandled = false;
    ErrInfo err;

    Token(TokenType token, stream_t &stream, std::string strVal = "") {
        tok = token;
        str = strVal;
        err.name = stream.name;
        err.line = getLine(stream);
        err.lineNo = stream.line + 1;
        err.offset = stream.at - stream.lastLinePos;
        err.offset += stream.line == 0;
        err.size = 0;
    }
};

std::vector<Token> tokenize(stream_t &stream);
