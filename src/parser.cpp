#include "lexer.h"
#include "ast.h"

// curTok and nextTok
struct tokstream_t {
    std::vector<Token> *tokens = NULL;
    size_t at = 0;
};
static tokstream_t &getTokstream() {
    static tokstream_t tokstream;
    return tokstream;
}
static Token curTok() {
    tokstream_t &ts = getTokstream();
    return (*ts.tokens)[ts.at];
}
static Token nextTok() {
    getTokstream().at++;
    return curTok();
}

// Match: only check if they are the same, doesn't advance
static bool match(std::initializer_list<TokenType> tokens) {
    TokenType tok = curTok().tok;
    for (auto &token : tokens) {
        if (tok == token)
            return true;
    }
    return false;
}
// Eat: error is the wrong token is provided, else eat it.
static bool eat(TokenType token, std::string error) {
    if (curTok().tok == token) {
        nextTok();
        return true;
    }
    err(ErrType::error, error, curTok().err);
    return false;
}

// Time for actual parsing
void raise_unhandled() {
    err(
        ErrType::comp_bug,
        "Eek! That's not handled! Please submit a bug report to burlap.",
        curTok().err
    );
    err(
        ErrType::note,
        "Unhandled TokenType: " + std::to_string((int)curTok().tok),
        curTok().err
    );
}

static std::unique_ptr<ExprAST> parseBinopSet();
static std::unique_ptr<ExprAST> parseExpression() {
    std::unique_ptr<ExprAST> ret = nullptr;
    if (match({
        Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals,
        Plus_equals, Minus_equals, Times_equals, Div_equals,
        Plus, Minus, Times, Div, Modulo,
        Plus_plus, Minus_minus, Not
    })) {
        err(ErrType::error, "Invalid use of operator", curTok().err);
        return nullptr;
    }
    switch (curTok().tok) {
        case String:
            ret = std::make_unique<StringAST>(curTok().str);
            nextTok();
            break;
        case Int:
             ret = std::make_unique<NumberAST>(curTok().numVal);
             nextTok();
             break;
        case Float:
            ret = std::make_unique<DecimalAST>(curTok().floatVal);
            nextTok();
            break;
        case Bool:
            ret = std::make_unique<BoolAST>(curTok().str == "true");
            nextTok();
            break;
        case None:
            ret = std::make_unique<NoneAST>();
            nextTok();
            break;
        case Lparan:
            nextTok();
            ret = parseBinopSet();
            if (!eat(Rparan, "Expecting )")) {
                return nullptr;
            }
            break;
        default:
            ErrInfo errInfo = curTok().err;
            errInfo.offset += 1;
            err(ErrType::error, "Expected expression", errInfo);
            return nullptr;
    }
    return ret;
}

// Unary
static std::unique_ptr<ExprAST> parseUnary() {
    std::unique_ptr<ExprAST> expr = nullptr;
    // -, !, ++, -- are all unary prefixes
    // (-- and ++ aren't said to be post or prefix by spec, so I assumed prefix for now)
    if (match({Minus, Not, Plus_plus, Minus_minus})) {
        Token type = curTok();
        nextTok();
        return std::make_unique<UnaryAST>(
            type, std::move(parseUnary())
         );
    }
    return parseExpression();
}

// And now binops, just like unary but with more expressions and they have precedence
// Little helper so I don't write the same code four times
static std::unique_ptr<ExprAST> parseBinopHelper(
    std::initializer_list<TokenType> tokens,
    std::unique_ptr<ExprAST> callback(void)
) {
    // Use the lower precedence callback to get the left op
    std::unique_ptr<ExprAST> expr = callback();
    while (match(tokens)) {
        ErrInfo errInfo;
        if (expr == nullptr) {
            // Invalid left op
            ErrInfo errInfo = curTok().err;
            err(ErrType::error, "Missing or invalid left operand", errInfo);
            return nullptr;
        }
        Token op = curTok();
        nextTok();
        std::unique_ptr<ExprAST> right = callback();
        if (right == nullptr) {
            // Invalid right op
            err(ErrType::error, "Missing or invalid right operand", op.err);
            return nullptr;
        }
        expr = std::make_unique<BinopAST>(std::move(expr), op, std::move(right));
    }
    return expr;
}

static std::unique_ptr<ExprAST> parseBinopMath() {
    return parseBinopHelper({Plus, Minus, Times, Div, Modulo}, parseUnary);
}
static std::unique_ptr<ExprAST> parseBinopCmp() {
    // Comparison binops: ==, !=, <, >, <=, >=
    // Little easter egg, `<3` and produces the string "♡"
    if (curTok().tok == TokenType::Lt) {
        Token tok = nextTok();
        if (tok.tok == TokenType::Int && tok.numVal == 3)
            return std::make_unique<StringAST>("♡");
    }
    return parseBinopHelper(
        {Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals},
        parseBinopMath
    );
}
static std::unique_ptr<ExprAST> parseBinopLogic() {
    // Logical binops: &&, ||, ^^
    return parseBinopHelper({And, Or, Xor}, parseBinopCmp);
}
static std::unique_ptr<ExprAST> parseBinopSet() {
    // Setting binops: =, +=, -=, *=, /=
    return parseBinopHelper(
        {Equals, Plus_equals, Minus_equals, Times_equals, Div_equals},
        parseBinopLogic
    );
}

// Parsing
std::vector<std::unique_ptr<ExprAST>> parse(std::vector<Token> &tokens) {
    getTokstream().tokens = &tokens;
    //printf("%s %i %s\n", parseBinopSet()->toStr().c_str(), curTok().tok, curTok().str.c_str());
    return {};
}
