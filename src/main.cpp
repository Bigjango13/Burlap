#include <cstdio>
#include <fstream>
#include <filesystem>

#include "lexer.h"

namespace fs = std::filesystem;
using namespace std::string_literals;

std::string read_input(char *name) {
    // Read the input file
    std::ifstream file(
        name,
        std::ios::in | std::ios::binary
    );
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

void print_help(char *name) {
    printf("Usage: %s input.sk\n", name);
}

int main(int argc, char *argv[]) {
    // Args
    stream_t stream = {""};
    if (argc < 2) {
        print_help(argv[0]);
        return 0;
    }
    if (argc >= 3) {
        if (argv[1] == "-c"s) {
            stream.str = argv[2];
            stream.name = "<commandline>";
        } else {
            print_help(argv[0]);
            return 1;
        }
    } else {
        stream.str = read_input(argv[1]);
        stream.name = argv[1];
    }

    // The pipeline begins
    // Lexing
    std::vector<Token> tokens = tokenize(stream);
}
