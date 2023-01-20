#pragma once

#include <memory>

#include "lexer.h"
#include "ast.h"

std::vector<std::unique_ptr<ExprAST>> parse(std::vector<Token> &tokens);
