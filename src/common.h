#pragma once

#include <string>

#define IMPOSSIBLE "We've reached an unreachable state. Anything is possible. The limits were in our heads all along. Follow your dreams"
enum class ErrType {
    note,
    hint,
    warn,
    error,
    comp_bug
};

struct stream_t {
    std::string str;

    size_t at = 0;
    size_t line = 0;

    size_t lastLinePos = 0;

    std::string name = "";
};

struct ErrInfo {
    std::string name;
    std::string line;
    size_t lineNo;
    size_t offset;
    short size = 0;
};

std::string getLine(stream_t &stream, size_t start);
std::string getLine(stream_t &stream);
void err(ErrType type, std::string msg, ErrInfo info);
void err(ErrType type, std::string msg, stream_t &stream, short size = 0);

