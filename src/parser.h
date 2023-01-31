#pragma once

#include <memory>

#include "lexer.h"
#include "ast.h"

// Casting for subclass ptrs
#define CAST_TO(obj, type) (dynamic_cast<type>(obj))

std::vector<std::shared_ptr<StmtAST>> parse(std::vector<Token> &tokens);
