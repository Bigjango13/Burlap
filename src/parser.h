#pragma once

#include <memory>

#include "lexer.h"
#include "ast.h"

// Casting for subclass ptrs
#define CAST_TO(obj, type) (dynamic_cast<type>(obj))
// Fancy shared_ptr casting
// Adapted from https://stackoverflow.com/a/21174979
/*template<typename Derived, typename Base>
std::shared_ptr<Derived> static_shared_ptr_cast(std::shared_ptr<Base> &&p) {
    auto d = static_cast<Derived *>(p.release());
    return std::shared_ptr<Derived>(d);
}*/

std::vector<std::shared_ptr<StmtAST>> parse(std::vector<Token> &tokens);
