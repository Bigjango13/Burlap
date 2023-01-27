#pragma once

#include "lexer.h"
#include "ast.h"

bool runFile(std::string name);
std::vector<std::shared_ptr<StmtAST>> genAST(stream_t &stream);
std::string read_input(std::string name);
