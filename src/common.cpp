#include <string>

#include "common.h"

std::string getLine(stream_t &stream, size_t start) {
    // Gets a line from the starting pos.
    std::string ret = "";
    // Advance if at end of line
    if (stream.str[start] == '\n')
        ++start;
    // Read line
    while (stream.str.size() > start) {
        char i = stream.str[start];
        if (i == '\n') break;
        ret += std::string(1, i);
        ++start;
    }
    return ret;
}

// Small wrapper
std::string getLine(stream_t &stream) {
    return getLine(stream, stream.lastLinePos);
}

// Logging
void err(ErrType type, std::string msg, ErrInfo info) {
    // First part ("<stdin>:1:0: ")
    printf(
        "\e[1m%s:%li:%li: \e[0m",
        info.name.c_str(), info.lineNo,
        info.offset
    );

    // Get color and log level based on type.
    std::string start = "";
    std::string color = "";
    switch (type) {
    case ErrType::note:
        start = "note";
        color = "\e[1;36m";
        break;
    case ErrType::hint:
        start = "hint";
        color = "\e[32m";
        break;
    case ErrType::warn:
        start = "warning";
        color = "\e[1;35m";
        break;
    case ErrType::error:
        start = "error";
        color = "\e[1;31m";
        break;
    case ErrType::comp_bug:
        start = "\e[1;43mburlap bug\e[0m";
        color = "\e[1;31m";
        break;
    }
    printf("%s%s:\e[0m %s\n", color.c_str(), start.c_str(), msg.c_str());
    printf("    %li | %s\n", info.lineNo, info.line.c_str());
    // This part looks scary, but all it does is print enough
    // spaces to equal the line number size
    std::string line = std::string(std::to_string(info.lineNo).size(), ' ');
    printf("    %s |", line.c_str());
    size_t size = info.size <= 0 ? 1 : info.size;
    line = std::string(info.offset, ' ');
    // Underline
    line += color + "^";
    if (size - 1 > 0) {
        line += std::string(size - 1, '~');
    }
    printf("%s\e[0m\n", line.c_str());
}

// Small wrapper
void err(ErrType type, std::string msg, stream_t &stream, short size /*=0*/) {
    err(type, msg, {
        stream.name,
        getLine(stream),
        stream.line + 1,
        stream.at - stream.lastLinePos,
        size
    });
}
