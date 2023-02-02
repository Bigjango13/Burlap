#pragma once

#include <map>

#include "ast.h"
#include "common.h"
#include "main.h"

// The type of burlap vars
class Val {
public:
    // Values
    char type = 'n';
    int numVal = 0;
    float floatVal = 0;
    std::string strVal = "";
    // Inits
    bool isBool = false;
    Val(int numVal) : type('i'), numVal(numVal) {}
    Val(bool boolVal) : type('i'), numVal(boolVal), isBool(true) {}
    Val(float floatVal) : type('f'), floatVal(floatVal) {}
    Val(std::string strVal) : type('s'), strVal(strVal) {}
    Val() {}
    // Conversions
    std::string toStr() {
        switch (type) {
            case 'i':
                if (!isBool)
                    return std::to_string(numVal);
                else
                    return numVal ? "true" : "false";
            case 'f': {
                // Normal ftos stuff, also strip trailing zeros after one digit
                // 35.0000000 -> 35.0
                // 2453.40640 -> 2453.4064
                std::string ret = std::to_string(floatVal);
                if (ret.at(ret.find_last_not_of('0')) == '.')
                    return ret.erase(ret.find_last_not_of('0') + 2, std::string::npos);
                return ret.erase(ret.find_last_not_of('0') + 1, std::string::npos);
            } case 's':
                return strVal;
            case 'n':
                return "none";
        }
        return IMPOSSIBLE;
    }
    float toFloat() {
        if (type == 'i') return float(numVal);
        return floatVal;
    }
    int toInt() {
        if (type == 'f') return int(floatVal);
        return numVal;
    }
    // Operators
    void operator+=(Val right) {
        *this = operator+(right);
    }
    Val operator+(Val right) {
        if (type == 's' || right.type == 's') {
            return Val(toStr() + right.toStr());
        } else if (type == 'f' || right.type == 'f') {
            return Val(toFloat() + right.toFloat());
        } else if (type == 'i' || right.type == 'i') {
            return Val(numVal + right.numVal);
        }
        // None
        return Val();
    }
    void operator-=(Val right) {
        *this = operator-(right);
    }
    Val operator-(Val right) {
        if (type == 's' || right.type == 's') {
            throw std::runtime_error("Can't use '-' with strings");
        } else if (type == 'f' || right.type == 'f') {
            return Val(toFloat() - right.toFloat());
        } else if (type == 'i' || right.type == 'i') {
            return Val(numVal - right.numVal);
        }
        // None
        return Val();
    }
    void operator*=(Val right) {
        *this = operator*(right);
    }
    Val operator*(Val right) {
        if (type == 's' || right.type == 's') {
            if (right.type == 'i') {
                std::string ret = "";
                while (right.numVal > 0) {
                    ret += strVal;
                    right.numVal--;
                }
                return Val(ret);
            }
            throw std::runtime_error("Can't use '*' with strings");
        } else if (type == 'f' || right.type == 'f') {
            return Val(toFloat() * right.toFloat());
        } else if (type == 'i' || right.type == 'i') {
            return Val(numVal * right.numVal);
        }
        // None
        return Val();
    }
    void operator/=(Val right) {
        *this = operator/(right);
    }
    Val operator/(Val right) {
        if (type == 's' || right.type == 's') {
            throw std::runtime_error("Can't use '/' with strings");
        } else if (type == 'f' || right.type == 'f') {
            return Val(toFloat() / right.toFloat());
        } else if (type == 'i' || right.type == 'i') {
            return Val(toFloat() / right.toFloat());
        }
        // None
        return Val();
    }
    Val operator%(Val right) {
        if (type == 's' || right.type == 's') {
            throw std::runtime_error("Can't use '%' with strings");
        } else if (type == 'f' || right.type == 'f') {
            int lval = toInt(), rval = right.toInt();
            return lval < rval ? lval : lval % rval;
        } else if (type == 'i' || right.type == 'i') {
            int lval = toInt(), rval = right.toInt();
            return lval < rval ? lval : lval % rval;
        }
        // None
        return Val();
    }
    bool operator!() {
        if (type == 's') {
            return strVal == "";
        } else if (type == 'f') {
            return floatVal == 0.0f;
        } else if (type == 'i') {
            return numVal == 0;
        }
        // None
        return true;
    }
    bool operator==(Val right) {
        if (type == 's' || right.type == 's') {
            if (type == 's' && right.type == 's')
                return strVal == right.strVal;
            return false;
        } else if (type == 'n' || right.type == 'n') {
            return type == 'n' && right.type == 'n';
        } else if (type == 'f' || right.type == 'f') {
            return toFloat() == right.toFloat();
        } else if (type == 'i' || right.type == 'i') {
            return toInt() == right.toInt();
        }
        return false;
    }
    // Use UFO op to implement all other needed ops
    std::partial_ordering operator<=>(Val right) {
        if (type == 's' || right.type == 's') {
            return std::partial_ordering::unordered;
        } else if (type == 'n' || right.type == 'n') {
            return std::partial_ordering::unordered;
        } else if (type == 'f' || right.type == 'f') {
            return toFloat() <=> right.toFloat();
        } else if (type == 'i' || right.type == 'i') {
            return toFloat() <=> right.toFloat();
        }
        return std::partial_ordering::unordered;
    }
};

// State
typedef std::map<std::string,std::shared_ptr<Val>> vars_t;
typedef std::map<std::string,std::shared_ptr<FuncAST>> funcs_t;
struct state_t {
    vars_t vars = {};
    funcs_t funcs = {};
    // Whaddaya know! A reverse linked lists in the wild.
    state_t *outer_state = NULL;
    // Getters
    std::shared_ptr<Val> get_var(std::string &name) {
        auto var = vars.find(name);
        if (var == vars.end() && outer_state != NULL)
            // Recurse
            return outer_state->get_var(name);
        if (var != vars.end())
            return var->second;
        return NULL;
    }
    std::shared_ptr<FuncAST> get_func(std::string &name) {
        auto func = funcs.find(name);
        if (func == funcs.end() && outer_state != NULL)
            // Recurse
            return outer_state->get_func(name);
        if (func != funcs.end())
            return func->second;
        return NULL;
    }
};

// Runner class
class Runner {
    state_t state;
    bool inFunc = false;
public:
    bool isRepl = false;
    std::shared_ptr<Val> exec_body(
        std::shared_ptr<StmtAST> &body, bool _lower_scope = true
    );
    Val call_func(std::shared_ptr<CallAST> &call);
    std::shared_ptr<Val> eval(std::shared_ptr<ExprAST> &node);
    std::shared_ptr<Val> exec_single(std::shared_ptr<StmtAST> &node);
    bool exec(std::vector<std::shared_ptr<StmtAST>> &ast);
};
