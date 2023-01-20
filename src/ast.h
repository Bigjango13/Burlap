#pragma once
#include <memory>

#include "lexer.h"

class ExprAST {
public:
    ExprAST() {};
    virtual ~ExprAST() {};
    virtual std::string toStr() {return "";};
};

// Sack atoms, String | Number | Bool | None;
class StringAST : public ExprAST {
public:
    std::string str = "";
    StringAST(std::string str) : str(str) {};
    std::string toStr() {return "\"" + str + "\"";}
};

class NumberAST : public ExprAST {
public:
    int num = 0;
    NumberAST(int num) : num(num) {};
    std::string toStr() {return std::to_string(num);}
};

class DecimalAST : public ExprAST {
public:
    float num = 0;
    DecimalAST(float num) : num(num) {};
    std::string toStr() {return std::to_string(num);}
};

class BoolAST : public ExprAST {
public:
    bool val = false;
    BoolAST(bool val) : val(val) {};
    std::string toStr() {return val ? "true" : "false";}
};

class NoneAST : public ExprAST {
public:
    NoneAST() {};
    std::string toStr() {return "none";}
};


class UnaryAST : public ExprAST {
public:
    Token op;
    std::unique_ptr<ExprAST> expr;
    UnaryAST(Token op, std::unique_ptr<ExprAST> expr) :
        op(op), expr(std::move(expr)) {};
    std::string toStr() {return "(" + op.str + expr->toStr() + ")";}
};

class BinopAST : public ExprAST {
public:
    std::unique_ptr<ExprAST> left;
    Token op;
    std::unique_ptr<ExprAST> right;
    BinopAST(std::unique_ptr<ExprAST> left, Token op, std::unique_ptr<ExprAST> right) :
        left(std::move(left)), op(op), right(std::move(right)) {};
    std::string toStr() {return "(" + op.str + " " + left->toStr() + " " + right->toStr() + ")";}
};
