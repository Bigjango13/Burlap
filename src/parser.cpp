#include "parser.h"

// Error tracker
bool hasErrored = false;
#define ERR(type, args...) {if(type==ErrType::error){hasErrored=true;}err(type, args);}

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
    ERR(ErrType::error, error, curTok().err);
    return false;
}

// Time for actual parsing
static std::shared_ptr<ExprAST> parseBinopLogic();
static std::shared_ptr<ExprAST> parseExpression() {
    std::shared_ptr<ExprAST> ret = nullptr;
    // Invalid use of ops
    if (match({
        Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals,
        Plus_equals, Minus_equals, Times_equals, Div_equals,
        Plus, Minus, Times, Div, Modulo,
        Plus_plus, Minus_minus, Not
    })) {
        ERR(ErrType::error, "invalid use of operator", curTok().err);
        nextTok();
        return nullptr;
    }
    // Sanity checks passed
    switch (curTok().tok) {
        // String
        case String:
            ret = std::make_shared<StringAST>(curTok().str);
            nextTok();
            break;
        // Non-floating numbers
        case Int:
             ret = std::make_shared<NumberAST>(curTok().numVal);
             nextTok();
             break;
         // Floats
        case Float:
            ret = std::make_shared<DecimalAST>(curTok().floatVal);
            nextTok();
            break;
        // Booleans
        case Bool:
            ret = std::make_shared<BoolAST>(curTok().str == "true");
            nextTok();
            break;
        // None
        case None:
            ret = std::make_shared<NoneAST>();
            nextTok();
            break;
        // Vars
        case Identifier:
            ret = std::make_shared<VarAST>(curTok().str);
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
            ERR(ErrType::error, "expected expression", errInfo);
            return nullptr;
    }
    return ret;
}

static std::shared_ptr<ExprAST> parseCall() {
    auto ret = parseExpression();
    // The call
    while (CAST_TO(ret.get(), VarAST*) && curTok().tok == Lparan) {
        nextTok();
        // Args
        std::vector<std::shared_ptr<ExprAST>> args = {};
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
                ERR(ErrType::warn, "trailing comma in argument list", curTok().err);
            }
        }
        nextTok();
        ret = std::make_shared<CallAST>(std::move(ret), std::move(args));
    }
    return ret;
}

// Unary
static std::shared_ptr<ExprAST> parseUnary() {
    std::shared_ptr<ExprAST> expr = nullptr;
    // -, !, ++, -- are all unary prefixes
    // (-- and ++ aren't said to be post or prefix by spec, so I assumed prefix for now)
    if (match({Minus, Not, Plus_plus, Minus_minus})) {
        std::string op = curTok().str;
        nextTok();
        return std::make_shared<UnaryAST>(
            op, std::move(parseUnary())
         );
    }
    return parseCall();
}

// And now binops, just like unary but with more expressions and they have precedence
// Little helper so I don't write the same code three/four times
template<typename LeftReq = ExprAST>
static std::shared_ptr<ExprAST> parseBinopHelper(
    std::initializer_list<TokenType> tokens,
    std::shared_ptr<ExprAST> callback(void)
) {
    // Use the lower precedence callback to get the left op
    std::shared_ptr<ExprAST> expr = callback();
    while (match(tokens)) {
        ErrInfo errInfo;
        if (expr == nullptr || CAST_TO(expr.get(), LeftReq*) == nullptr) {
            // Invalid left op
            ErrInfo errInfo = curTok().err;
            ERR(ErrType::error, "missing or invalid left operand", errInfo);
            return nullptr;
        }
        Token op = curTok();
        nextTok();
        std::shared_ptr<ExprAST> right = callback();
        if (right == nullptr) {
            // Invalid right op
            ERR(ErrType::error, "missing or invalid right operand", op.err);
            return nullptr;
        }
        expr = std::make_shared<BinopAST>(std::move(expr), op.str, std::move(right));
    }
    return expr;
}

static std::shared_ptr<ExprAST> parseBinopMath() {
    return parseBinopHelper({Plus, Minus, Times, Div, Modulo}, parseUnary);
}
static std::shared_ptr<ExprAST> parseBinopCmp() {
    // Comparison binops: ==, !=, <, >, <=, >=
    // Little easter egg, `<3` and produces the string "♡"
    if (curTok().tok == TokenType::Lt) {
        Token tok = nextTok();
        if (tok.tok == TokenType::Int && tok.numVal == 3) {
            nextTok();
            return std::make_shared<StringAST>("♡");
        }
    }
    return parseBinopHelper(
        {Equals_equals, Not_equals, Lt, Gt, Lt_equals, Gt_equals},
        parseBinopMath
    );
}
static std::shared_ptr<ExprAST> parseBinopLogic() {
    // Logical binops: &&, ||, ^^
    return parseBinopHelper({And, Or, Xor}, parseBinopCmp);
}
static std::shared_ptr<ExprAST> parseBinopSet() {
    // Setting binops: =, +=, -=, *=, /=
    return parseBinopHelper<VarAST>(
        {Equals, Plus_equals, Minus_equals, Times_equals, Div_equals},
        parseBinopLogic
    );
}

// Statments
#define EAT_SEMICOLON() {if(!eat(Semicolon, "expected semicolon"))return nullptr;}
static std::shared_ptr<StmtAST> parseStatement();
static std::shared_ptr<StmtAST> parseBody() {
    if (!eat(Lbrace, "expected { to start body")) return nullptr;
    std::vector<std::shared_ptr<StmtAST>> exprs = {};
    while (!match({Rbrace, Eof})) {
        exprs.emplace_back(std::move(parseStatement()));
    }
    if (!eat(Rbrace, "expected } at end of body")) return nullptr;
    return std::make_shared<BodyAST>(std::move(exprs));
}

static std::shared_ptr<StmtAST> parseFunc() {
    // Eat func
    eat(Func, IMPOSSIBLE);
    // Name
    if (curTok().tok != Identifier) {
        ERR(ErrType::error, "expected function name", curTok().err);
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
            ERR(ErrType::error, "expected argument name", curTok().err);
            return nullptr;
        }
        // Comma for new arg or Rparan for end of args
        if (!match({Comma, Rparan})) {
            ERR(ErrType::error, "expected ',' or ')' in argument list", curTok().err);
            return nullptr;
        }
        if (curTok().tok == Comma) nextTok();
    }
    // Eat ')'
    nextTok();
    // Body
    if (curTok().tok != Lbrace) {
        ERR(ErrType::error, "expected '{' at start of function body", curTok().err);
        ERR(ErrType::hint, "forward declaration isn't supported", curTok().err);
        return nullptr;
    }
    std::shared_ptr<StmtAST> body = parseBody();
    // Return
    return std::make_shared<FuncAST>(name, args, std::move(body));
}

// if 0 {} else if 1 {} else {}
static std::shared_ptr<StmtAST> parseIf() {
    // Check for if/else
    bool isIf = true;
    if (!match({If, Else})) {
        ERR(ErrType::error, IMPOSSIBLE, curTok().err);
        return nullptr;
    }
    if (curTok().tok == Else) isIf = false;
    nextTok();
    // Parse condition
    std::shared_ptr<ExprAST> cond = nullptr;
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
    std::shared_ptr<StmtAST> body = parseBody();
    auto ret = std::make_shared<IfAST>(std::move(cond), std::move(body));
    // Parse a possible else part (the else after a if)
    if (curTok().tok == Else) {
        ret->elseStmt = parseIf();
    }
    return ret;
}

static std::shared_ptr<StmtAST> parseReturn() {
    // Eat "return"
    eat(Return, IMPOSSIBLE);
    // Value
    if (curTok().tok == Semicolon) {
        nextTok();
        return nullptr;
    }
    std::shared_ptr<ExprAST> expr = parseBinopLogic();
    if (expr == nullptr) return nullptr;
    EAT_SEMICOLON();
    return std::make_shared<ReturnAST>(std::move(expr));
}

static std::shared_ptr<StmtAST> parseImport() {
    eat(Import, IMPOSSIBLE);
    if (!eat(Lparan, "missing '(' at start")) {
        return nullptr;
    }
    // Value
    std::shared_ptr<ExprAST> expr = parseBinopLogic();
    if (expr == nullptr) return nullptr;
    // End
    if (!eat(Rparan, "missing ')' at end")) {
        return nullptr;
    }
    EAT_SEMICOLON();
    return std::make_shared<ImportAST>(std::move(expr));;
}

static std::shared_ptr<StmtAST> parseLet() {
    // Eat "let"
    eat(Let, IMPOSSIBLE);
    // Name
    std::string name;
    if (curTok().tok == Identifier) {
        name = curTok().str;
        nextTok();
    } else {
        ERR(ErrType::error, "expected variable name in declaration", curTok().err);
        return nullptr;
    }
    // Value
    std::shared_ptr<ExprAST> expr = nullptr;
    if (curTok().tok == Equals) {
        nextTok();
        expr = parseBinopLogic();
        if (expr == nullptr) return nullptr;
    } else if (curTok().tok != Semicolon) {
        ERR(ErrType::error, "expected '=' or ';' in var declaration", curTok().err);
        return nullptr;
    } else {
        expr = std::make_shared<NoneAST>();
    }
    auto ret = std::make_shared<LetAST>(name, std::move(expr));
    EAT_SEMICOLON();
    return ret;
}

static std::shared_ptr<StmtAST> parseLoop() {
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
        ERR(ErrType::error, "expected variable name in loop", curTok().err);
        return nullptr;
    }
    // "in"
    if (!eat(In, "missing 'in' keyword in loop")) {
        return nullptr;
    }
    // Iter
    std::shared_ptr<ExprAST> iter = parseBinopLogic();
    // End
    if (!eat(Rparan, "missing ')' at end of loop")) {
        return nullptr;
    }
    // Body
    std::shared_ptr<StmtAST> body = parseBody();
    return std::make_shared<LoopAST>(name, std::move(iter), std::move(body));
}

// Statements
static std::shared_ptr<StmtAST> parseStatement() {
    // All statements end with this
    switch (curTok().tok) {
        // Bodys
        case Lbrace:
            return parseBody();
        // Functions
        case Func:
            return parseFunc();
        // Return
        case Return:
            return parseReturn();
        // Imports
        case Import:
            return parseImport();
        // Vars
        case Let:
            return parseLet();
        // Loops
        case Loop:
            return parseLoop();
        // If/elses
        case If:
            return parseIf();
        case Else:
            ERR(ErrType::error, "missing previous if", curTok().err);
            nextTok();
            return nullptr;
        // Semicolons
        case Semicolon:
            nextTok();
            return parseStatement();
        // EOF
        case Eof:
            return nullptr;
        default:
            // Expression
            auto ret = parseBinopSet();
            if (!eat(Semicolon, "expected semicolon")) {
                if (curTok().tok != Eof)
                    nextTok();
                return nullptr;
            }
            return ret;
    }
}

// Parsing
std::vector<std::shared_ptr<StmtAST>> parse(std::vector<Token> &tokens) {
    hasErrored = false;
    getTokstream() = {&tokens};
    std::vector<std::shared_ptr<StmtAST>> ret = {};
    while (curTok().tok != Eof)
        ret.push_back(parseStatement());
    if (hasErrored)
        return {};
    return ret;
}
