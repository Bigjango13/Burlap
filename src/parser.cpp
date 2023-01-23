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
static std::unique_ptr<ExprAST> parseBinopLogic();
static std::unique_ptr<ExprAST> parseExpression() {
    std::unique_ptr<ExprAST> ret = nullptr;
    // Invalid use of ops
    if (match({
        Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals,
        Plus_equals, Minus_equals, Times_equals, Div_equals,
        Plus, Minus, Times, Div, Modulo,
        Plus_plus, Minus_minus, Not
    })) {
        err(ErrType::error, "invalid use of operator", curTok().err);
        nextTok();
        return nullptr;
    }
    // Sanity checks passed
    switch (curTok().tok) {
        // String
        case String:
            ret = std::make_unique<StringAST>(curTok().str);
            nextTok();
            break;
        // Non-floating numbers
        case Int:
             ret = std::make_unique<NumberAST>(curTok().numVal);
             nextTok();
             break;
         // Floats
        case Float:
            ret = std::make_unique<DecimalAST>(curTok().floatVal);
            nextTok();
            break;
        // Booleans
        case Bool:
            ret = std::make_unique<BoolAST>(curTok().str == "true");
            nextTok();
            break;
        // None
        case None:
            ret = std::make_unique<NoneAST>();
            nextTok();
            break;
        // Vars
        case Identifier:
            ret = std::make_unique<VarAST>(curTok().str);
            nextTok();
            break;
        // Nested expressions
        case Lparan:
            nextTok();
            ret = parseBinopLogic();
            if (!eat(Rparan, "expecting )")) {
                return nullptr;
            }
            break;
        // Not an expression
        default:
            ErrInfo errInfo = curTok().err;
            errInfo.offset += 1;
            err(ErrType::error, "expected expression", errInfo);
            return nullptr;
    }
    return ret;
}

static std::unique_ptr<ExprAST> parseCall() {
    auto ret = parseExpression();
    // The call
    while (curTok().tok == Lparan) {
        nextTok();
        // Args
        std::vector<std::unique_ptr<ExprAST>> args = {};
        while (curTok().tok != Rparan) {
            args.push_back(parseBinopLogic());
            if (curTok().tok == Rparan) {
                break;
            }
            if (!eat(Comma, "expected ')' or ',' in argument list")) {
                return nullptr;
            }
            // Trailing comma
            if (curTok().tok == Rparan) {
                err(ErrType::warn, "trailing comma in argument list", curTok().err);
            }
        }
        nextTok();
        ret = std::make_unique<CallAST>(std::move(ret), std::move(args));
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
    return parseCall();
}

// And now binops, just like unary but with more expressions and they have precedence
// Little helper so I don't write the same code three/four times
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
            err(ErrType::error, "missing or invalid left operand", errInfo);
            return nullptr;
        }
        Token op = curTok();
        nextTok();
        std::unique_ptr<ExprAST> right = callback();
        if (right == nullptr) {
            // Invalid right op
            err(ErrType::error, "missing or invalid right operand", op.err);
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
        if (tok.tok == TokenType::Int && tok.numVal == 3) {
            nextTok();
            return std::make_unique<StringAST>("♡");
        }
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

// Statments
#define EAT_SEMICOLON() {if(!eat(Semicolon, "expected semicolon"))return nullptr;}
static std::unique_ptr<StmtAST> parseStatement();
static std::unique_ptr<StmtAST> parseBody() {
    if (!eat(Lbrace, "expected { to start body")) return nullptr;
    std::vector<std::unique_ptr<StmtAST>> exprs = {};
    while (!match({Rbrace, Eof})) {
        exprs.emplace_back(std::move(parseStatement()));
    }
    if (!eat(Rbrace, "expected } at end of body")) return nullptr;
    return std::make_unique<BodyAST>(std::move(exprs));
}

static std::unique_ptr<StmtAST> parseFunc() {
    // Eat func
    eat(Func, IMPOSSIBLE);
    // Name
    if (curTok().tok != Identifier) {
        err(ErrType::error, "expected function name", curTok().err);
        return nullptr;
    }
    std::string name = curTok().str;
    nextTok();
    if (!eat(Lparan, "expected '(' for start of argument list"))
        return nullptr;
    // Args
    std::vector<std::string> args = {};
    while (curTok().tok != Rparan) {
        if (curTok().tok == Identifier) {
            args.push_back(curTok().str);
            nextTok();
        } else {
            err(ErrType::error, "expected argument name", curTok().err);
            return nullptr;
        }
        // Comma for new arg or Rparan for end of args
        if (!match({Comma, Rparan})) {
            err(ErrType::error, "expected ',' or ')' in argument list", curTok().err);
            return nullptr;
        }
        if (curTok().tok == Comma) nextTok();
    }
    // Eat ')'
    nextTok();
    // Body
    if (curTok().tok != Lbrace) {
        err(ErrType::error, "expected '{' at start of function body", curTok().err);
        err(ErrType::hint, "forward declaration isn't supported", curTok().err);
        return nullptr;
    }
    std::unique_ptr<StmtAST> body = parseBody();
    // Return
    return std::make_unique<FuncAST>(name, args, std::move(body));
}

static std::unique_ptr<StmtAST> parseIf() {
    // Check for if/else
    bool isIf = true;
    if (!match({If, Else})) {
        err(ErrType::error, IMPOSSIBLE, curTok().err);
        return nullptr;
    }
    if (curTok().tok == Else) isIf = false;
    nextTok();
    // Parse condition
    std::unique_ptr<ExprAST> cond = nullptr;
    if (isIf) {
        cond = parseBinopLogic();
        if (!cond) return nullptr;
    }
    // Parse body
    // For else, just return body
    if (!isIf) {
        if (curTok().tok == If)
            // Else if
            return parseIf();
        else
            return parseBody();
    }
    // Parse body as a part for if
    std::unique_ptr<StmtAST> body = parseBody();
    auto ret = std::make_unique<IfAST>(std::move(cond), std::move(body));
    // Parse a possible else part (the else after a if)
    if (curTok().tok == Else) {
        ret->elseStmt = parseIf();
    }
    return ret;
}

static std::unique_ptr<StmtAST> parseReturn() {
    // Eat "return"
    eat(Return, IMPOSSIBLE);
    // Value
    if (curTok().tok == Semicolon) {
        nextTok();
        return nullptr;
    }
    std::unique_ptr<ExprAST> expr = parseBinopLogic();
    if (expr == nullptr) return nullptr;
    EAT_SEMICOLON();
    return std::make_unique<ReturnAST>(std::move(expr));
}

static std::unique_ptr<StmtAST> parseImport() {
    eat(Import, IMPOSSIBLE);
    if (!eat(Lparan, "missing '(' at start")) {
        return nullptr;
    }
    // Value
    std::unique_ptr<ExprAST> expr = parseBinopLogic();
    if (expr == nullptr) return nullptr;
    // End
    if (!eat(Rparan, "missing ')' at end")) {
        return nullptr;
    }
    EAT_SEMICOLON();
    return std::make_unique<ReturnAST>(std::move(expr));;
}

static std::unique_ptr<StmtAST> parseLet() {
    // Eat "let"
    eat(Let, IMPOSSIBLE);
    // Name
    std::string name;
    if (curTok().tok == Identifier) {
        name = curTok().str;
        nextTok();
    } else {
        err(ErrType::error, "expected variable name in declaration", curTok().err);
        return nullptr;
    }
    // Value
    std::unique_ptr<ExprAST> expr = nullptr;
    if (curTok().tok == Equals) {
        nextTok();
        expr = parseBinopLogic();
        if (expr == nullptr) return nullptr;
    }
    auto ret = std::make_unique<LetAST>(name, std::move(expr));
    EAT_SEMICOLON();
    return ret;
}

static std::unique_ptr<StmtAST> parseLoop() {
    eat(Loop, IMPOSSIBLE);
    // Start
    if (!eat(Lparan, "missing '(' at start of loop")) {
        return nullptr;
    }
    // Var name
    std::string name;
    if (curTok().tok == Identifier) {
        name = curTok().str;
        nextTok();
    } else {
        err(ErrType::error, "expected variable name in loop", curTok().err);
        return nullptr;
    }
    // "in"
    if (!eat(In, "missing 'in' keyword in loop")) {
        return nullptr;
    }
    // Iter
    std::unique_ptr<ExprAST> iter = parseBinopLogic();
    // End
    if (!eat(Rparan, "missing ')' at end of loop")) {
        return nullptr;
    }
    // Body
    std::unique_ptr<StmtAST> body = parseBody();
    return std::make_unique<LoopAST>(name, std::move(iter), std::move(body));
}

// Statements
static std::unique_ptr<StmtAST> parseStatement() {
    // All statements end with this
    switch (curTok().tok) {
        case Lbrace:
            return parseBody();
        case Func:
            return parseFunc();
        case Return:
            return parseReturn();
        case Import:
            return parseImport();
        case Let:
            return parseLet();
        case Loop:
            return parseLoop();
        case If:
            return parseIf();
        case Else:
            err(ErrType::error, "missing previous if", curTok().err);
            nextTok();
            return nullptr;
        case Eof:
            return nullptr;
        default:
            // Expression
            auto ret = parseBinopSet();
            EAT_SEMICOLON();
            return ret;
    }
}

// Parsing
std::vector<std::unique_ptr<StmtAST>> parse(std::vector<Token> &tokens) {
    getTokstream() = {&tokens};
    std::vector<std::unique_ptr<StmtAST>> ret = {};
    while (curTok().tok != Eof)
        ret.push_back(parseStatement());
    return ret;
}
