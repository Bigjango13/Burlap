#include <cstdio>
#include <fstream>
#include <filesystem>
#include <iostream>

#include <editline/readline.h>

#include "lexer.h"
#include "parser.h"

namespace fs = std::filesystem;
using namespace std::string_literals;

std::string read_input(char *name) {
    // Read the input file
    std::ifstream file(name, std::ios::in | std::ios::binary);
    if (!file.good()) {
        printf("Failed to open input file.\n");
        exit(1);
    }
    // Get buffer
    size_t size = fs::file_size(name);
    std::string ret(size, '\0');
    // Read and make stream
    file.read(ret.data(), size);
    return ret;
}

bool run(stream_t &stream) {
    // Reset stream just in case
    stream.at = 0;
    stream.line = 0;
    stream.lastLinePos = 0;
    // The pipeline begins
    // Lexing
    std::vector<Token> tokens = tokenize(stream);
    if (tokens.size() == 0)
        return false;
    // Parsing
    std::vector<std::unique_ptr<StmtAST>> ast = parse(tokens);
    if (ast.size() == 0)
        return false;
    return true;
}

static void repl_init(stream_t &stream) {
    stream.name = "<stdin>";
    stifle_history(100);
}

static void strip(std::string &str) {
    size_t first = str.find_first_not_of(' ');
    if (first == std::string::npos) return;
    size_t last = str.find_last_not_of(' ');
    str = str.substr(first, (last - first + 1));
}

static std::string repl_get_input(int nestedNum = 0) {
    char *line = readline(nestedNum == 0 ? ">>> " : "... ");
    if (!line) {
        puts("");
        return "exit";
    }
    add_history(line);
    std::string input = line;
    strip(input);
    // Empty line
    if (input.c_str()[0] == '\0') return repl_get_input(nestedNum);
    // Ends with '{'
    if (input.at(input.size() - 1) == '{') {
        input += "\n";
        std::string str = "";
        // Loop until }
        while (1) {
            str = repl_get_input(nestedNum + 1);
            input += str + "\n";
            // Ends with '}'
            if (str.size() != 0)
                if (str.at(str.size() - 1) == '}')
                    break;
        }
    }
    return input;
}

int main(int argc, char *argv[]) {
    // Args
    stream_t stream = {""};
    if (argc < 2) {
        // REPL
        repl_init(stream);
        while (1) {
            stream.str = repl_get_input();
            if (stream.str == "exit") return 0;
            run(stream);
        }
        return 0;
    }
    if (argc >= 3) {
        if (argv[1] == "-c"s) {
            stream.str = argv[2];
            stream.name = "<commandline>";
        } else {
            printf("Usage: %s input.sk\n", argv[0]);
            return 1;
        }
    } else {
        stream.str = read_input(argv[1]);
        stream.name = argv[1];
    }
    return !run(stream);
}
