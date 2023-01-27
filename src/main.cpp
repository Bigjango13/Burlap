#include <cstdio>
#include <fstream>
#include <filesystem>
#include <iostream>

#include <editline/readline.h>

#include "main.h"
#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

namespace fs = std::filesystem;
using namespace std::string_literals;

std::string read_input(std::string name) {
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

std::vector<std::shared_ptr<StmtAST>> genAST(stream_t &stream) {
    // Reset stream just in case
    stream.at = 0;
    stream.line = 0;
    stream.lastLinePos = 0;
    // The pipeline begins
    // Lexing
    std::vector<Token> tokens = tokenize(stream);
    if (tokens.size() == 0)
        return {};
    // Parsing
    std::vector<std::shared_ptr<StmtAST>> ast = parse(tokens);
    return ast;
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
    std::string input = line;
    strip(input);
    if (input == "") return repl_get_input(nestedNum);
    add_history(input.c_str());
    // Empty line
    if (input.c_str()[0] == '\0') return repl_get_input(nestedNum);
    // Ends with '{'
    if (input.at(input.size() - 1) == '{') {
        input += "\n";
        std::string str = "";
        // Loop until }
        while (1) {
            str = repl_get_input(nestedNum + 1);
            input += str;
            // Ends with '}'
            if (str.size() != 0)
                if (str.at(str.size() - 1) == '}')
                    break;
            input += "\n";
        }
    }
    return input;
}

bool runFile(std::string name) {
    stream_t stream = {""};
    stream.str = read_input(name);
    stream.name = name;
    // Lexing and parsing.
    auto ast = genAST(stream);
    // Executing
    Runner runner;
    return runner.exec(ast);
}

int main(int argc, char *argv[]) {
    // Args
    if (argc < 2) {
        // REPL
        state_t state = {};
        stream_t stream = {""};
        repl_init(stream);
        Runner runner;
        runner.isRepl = true;
        while (1) {
            stream.str = repl_get_input();
            if (stream.str == "exit") return 0;
            char last = stream.str.at(stream.str.size() - 1);
            if (last != ';' && last != '{' && last != '}') {
                // Implicit semicolons for REPL oneliners
                stream.str += ";";
            }
            auto ast = genAST(stream);
            runner.exec(ast);
        }
        return 0;
    }
    if (argc >= 3) {
        stream_t stream = {""};
        if (argv[1] == "-c"s) {
            stream.str = argv[2];
            stream.name = "<commandline>";
        } else {
            printf("Usage: %s input.sk\n", argv[0]);
            return 1;
        }
        // Lexing and parsing.
        auto ast = genAST(stream);
        // Executing
        Runner runner;
        return !runner.exec(ast);
    } else {
        return !runFile(argv[1]);
    }
}
