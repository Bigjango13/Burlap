#pragma once
#include <memory>

#include "lexer.h"

class StmtAST {
public:
    StmtAST() {};
    virtual ~StmtAST() {};
};

class ExprAST : public StmtAST {};

// Sack atoms, String | Number | Bool | None;
class StringAST : public ExprAST {
public:
    std::string str = "";
    StringAST(std::string str) : str(str) {};
};

class NumberAST : public ExprAST {
public:
    int num = 0;
    NumberAST(int num) : num(num) {};
};

class DecimalAST : public ExprAST {
public:
    float num = 0;
    DecimalAST(float num) : num(num) {};
};

class BoolAST : public ExprAST {
public:
    bool val = false;
    BoolAST(bool val) : val(val) {};
};

class NoneAST : public ExprAST {
public:
    NoneAST() {};
};

class VarAST : public ExprAST {
public:
    std::string name;
    VarAST(std::string name) : name(name) {}
};

class CallAST : public ExprAST {
public:
    std::shared_ptr<ExprAST> expr;
    std::vector<std::shared_ptr<ExprAST>> args;
    CallAST(std::shared_ptr<ExprAST> expr, std::vector<std::shared_ptr<ExprAST>> args) :
        expr(std::move(expr)), args(std::move(args)) {};
};

class UnaryAST : public ExprAST {
public:
    std::string op;
    std::shared_ptr<ExprAST> expr;
    UnaryAST(std::string op, std::shared_ptr<ExprAST> expr) :
        op(op), expr(std::move(expr)) {};
};

class BinopAST : public ExprAST {
public:
    std::shared_ptr<ExprAST> left;
    std::string op;
    std::shared_ptr<ExprAST> right;
    BinopAST(std::shared_ptr<ExprAST> left, std::string op, std::shared_ptr<ExprAST> right) :
        left(std::move(left)), op(op), right(std::move(right)) {};
};

class BodyAST : public StmtAST {
public:
    std::vector<std::shared_ptr<StmtAST>> exprs;
    BodyAST(std::vector<std::shared_ptr<StmtAST>> exprs) : exprs(std::move(exprs)) {}
};

class FuncAST : public StmtAST {
public:
    std::string name;
    std::vector<std::string> args;
    std::shared_ptr<StmtAST> body;
    FuncAST(
        std::string name,
        std::vector<std::string> args,
        std::shared_ptr<StmtAST> body
    ) : name(name), args(args), body(std::move(body)) {}
};

class IfAST : public StmtAST {
public:
    std::shared_ptr<ExprAST> cond;
    std::shared_ptr<StmtAST> body;
    std::shared_ptr<StmtAST> elseStmt = nullptr;
    IfAST(
        std::shared_ptr<ExprAST> cond,
        std::shared_ptr<StmtAST> body
    ) : cond(std::move(cond)), body(std::move(body)) {}
};

class LetAST : public StmtAST {
public:
    std::string name;
    std::shared_ptr<ExprAST> val;
    LetAST(std::string name, std::shared_ptr<ExprAST> val) :
        name(name), val(std::move(val)) {}
};

class ReturnAST : public StmtAST {
public:
    std::shared_ptr<ExprAST> expr;
    ReturnAST(std::shared_ptr<ExprAST> expr) : expr(std::move(expr)) {}
};

class LoopAST : public StmtAST {
public:
    std::string var;
    std::shared_ptr<ExprAST> iter;
    std::shared_ptr<StmtAST> body;
    LoopAST(
        std::string var,
        std::shared_ptr<ExprAST> iter,
        std::shared_ptr<StmtAST> body
    ) : var(var), iter(std::move(iter)), body(std::move(body)) {}
};

class ImportAST : public StmtAST {
public:
    std::shared_ptr<ExprAST> expr;
    ImportAST(std::shared_ptr<ExprAST> expr) : expr(std::move(expr)) {}
};
