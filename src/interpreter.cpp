#include <map>

#include "parser.h"
#include "interpreter.h"

// Common errors
static void noVarErr(std::string name) {
    throw std::runtime_error("No varible named " + name + " in current scope");
}

static void noFuncErr(std::string name) {
    throw std::runtime_error("No function named " + name + " in current scope");
}

static void badArgsErr(int got, int need) {
    throw std::runtime_error(
        "Wrong number of arguments, got " + std::to_string(got)
        + ", need " + std::to_string(need)
    );
}

std::shared_ptr<Val> Runner::eval(std::shared_ptr<ExprAST> &node) {
    // Literals
    if (CAST_TO(node.get(), StringAST*)) {
        std::string str = CAST_TO(node.get(), StringAST*)->str;
        return std::make_shared<Val>(str);
    } else if (CAST_TO(node.get(), NumberAST*)) {
        int num = CAST_TO(node.get(), NumberAST*)->num;
        return std::make_shared<Val>(num);
    } else if (CAST_TO(node.get(), DecimalAST*)) {
        float num = CAST_TO(node.get(), DecimalAST*)->num;
        return std::make_shared<Val>(num);
    } else if (CAST_TO(node.get(), BoolAST*)) {
        bool val = CAST_TO(node.get(), BoolAST*)->val;
        return std::make_shared<Val>(val);
    } else if (CAST_TO(node.get(), NoneAST*)) {
        return std::make_shared<Val>();
    }
    // Vars
    if (CAST_TO(node.get(), VarAST*)) {
        std::string name = CAST_TO(node.get(), VarAST*)->name;
        std::shared_ptr<Val> var = state.get_var(name);
        if (var)
            return var;
        // Error if no var
        noVarErr(name);
    }
    // Unary
    if (CAST_TO(node.get(), UnaryAST*)) {
        auto unary = std::reinterpret_pointer_cast<UnaryAST>(node);
        // Recursively call eval until it all works out
        std::shared_ptr<Val> val = eval(unary->expr);
        if (!val)
            return NULL;
        if (unary->op == "-") {
            *val = Val(0) - *val;
        } else if (unary->op == "!") {
            *val = !*val;
        } else if (unary->op == "++") {
            *val += 1;
        } else if (unary->op == "--") {
            *val -= 1;
        }
        return val;
    }
    // Binop
    if (CAST_TO(node.get(), BinopAST*)) {
        auto binop = std::reinterpret_pointer_cast<BinopAST>(node);
        // Recursively call eval until it all works out
        Val val = Val();
        std::string op = binop->op;
        if (
            op == "=" || op == "+=" || op == "-=" ||
            op == "*=" || op == "/="
        ) {
            std::shared_ptr<Val> right = eval(binop->right);
            // Left is a var, get the name
            std::string name = CAST_TO(binop->left.get(), VarAST*)->name;
            // Get var
            std::shared_ptr<Val> var = state.get_var(name);
            if (!var) {
                // No var, error.
                noVarErr(name);
            }
            // Do something
            if (op == "=")
                *var = *right;
            if (op == "+=")
                *var += *right;
            if (op == "-=")
                *var -= *right;
            if (op == "*=")
                *var *= *right;
            if (op == "/=")
                *var /= *right;
            return NULL;
        }
        std::shared_ptr<Val> left = eval(binop->left);
        // Binary ops, bit tricky due to shortchir
        if (op == "&&")
            val = !!*left && !!*eval(binop->right);
        else if (op == "||")
            val = !!*left || !!*eval(binop->right);
        else if (op == "^^")
            val = !!*left != !!*eval(binop->right);
        if (val != Val())
            return std::make_shared<Val>(val);
        // Normal ops
        std::shared_ptr<Val> right = eval(binop->right);
        if (op == "+")
            val = *left + *right;
        else if (op == "-")
            val = *left - *right;
        else if (op == "*")
            val = *left * *right;
        else if (op == "/")
            val = *left / *right;
        else if (op == "%")
            val = *left % *right;
        else if (op == "==")
            val = *left == *right;
        else if (op == "!=")
            val = *left != *right;
        else if (op == "==")
            val = *left == *right;
        else if (op == ">")
            val = *left > *right;
        else if (op == "<")
            val = *left < *right;
        else if (op == ">=")
            val = *left >= *right;
        else if (op == "<=")
            val = *left <= *right;
        return std::make_shared<Val>(val);
    }
    // Call
    if (CAST_TO(node.get(), CallAST*)) {
        auto call = std::reinterpret_pointer_cast<CallAST>(node);
        Val val = call_func(call);
        return std::make_shared<Val>(val);
    }
    // No value
    return NULL;
}

std::shared_ptr<Val> Runner::exec_single(std::shared_ptr<StmtAST> &node) {
    if (CAST_TO(node.get(), ExprAST*)) {
        // Eval expression
        auto expr = std::reinterpret_pointer_cast<ExprAST>(node);
        std::shared_ptr<Val> val = eval(expr);
        if (val && *val != Val() && isRepl && !inFunc)
            puts(val->toStr().c_str());
    } else if (CAST_TO(node.get(), BodyAST*)) {
        // Execute body
        return exec_body(node);
    } else if (CAST_TO(node.get(), FuncAST*)) {
        // Add function
        auto func = std::reinterpret_pointer_cast<FuncAST>(node);
        state.funcs[func->name] = func;
    } else if (CAST_TO(node.get(), LetAST*)) {
        // Add var
        auto var = std::reinterpret_pointer_cast<LetAST>(node);
        if (state.vars.find(var->name) != state.vars.end()) {
            // Var already exists in current scope, err
            throw std::runtime_error("Redefinition of " + var->name);
        }
        state.vars[var->name] = eval(var->val);
    } else if (CAST_TO(node.get(), IfAST*)) {
        // Execute if
        auto ifExpr = std::reinterpret_pointer_cast<IfAST>(node);
        std::shared_ptr<Val> cond = eval(ifExpr->cond);
        if (cond && !!*cond) {
            // It's true, execute body
            return exec_single(ifExpr->body);
        } else if (ifExpr->elseStmt) {
            // It's false, run the else part
            return exec_single(ifExpr->elseStmt);
        }
    } else if (CAST_TO(node.get(), LoopAST*)) {
        // Loops
        auto loop = std::reinterpret_pointer_cast<LoopAST>(node);
        // Get the var
        auto var = state.get_var(loop->var);
        if (!var) {
            // Define a new var if it doesn't exist
            state.vars[loop->var] = std::make_shared<Val>(0);
            var = state.vars[loop->var];
        }
        // Get iter
        auto iter = loop->iter;
        if (!CAST_TO(iter.get(), CallAST*)) {
            throw std::runtime_error("Only range() is currently supported for loops");
        }
        auto call = std::reinterpret_pointer_cast<CallAST>(iter);
        std::string name = CAST_TO(call->expr.get(), VarAST*)->name;
        if (name != "range") {
            throw std::runtime_error("Only range() is currently supported for loops");
        }
        if (call->args.size() != 2) {
            badArgsErr(call->args.size(), 3);
        }
        // Do the loop
        auto start = eval(call->args[0]);
        auto end = eval(call->args[1]);
        var->type = 'i';
        for (int i = start->toInt(); i <= end->toInt(); i++) {
            // Set var
            var->numVal = i;
            // Run
            auto ret = exec_single(loop->body);
            if (ret)
                return ret;
        }
    } else if (CAST_TO(node.get(), ImportAST*)) {
        // Get file
        auto import = std::reinterpret_pointer_cast<ImportAST>(node);
        std::string name = eval(import->expr)->toStr();
        // Import
        stream_t stream = {""};
        stream.str = read_input(name);
        stream.name = name;
        auto ast = genAST(stream);
        // Run
        bool oldIsRepl = isRepl;
        isRepl = false;
        if (!exec(ast)) {
            isRepl = oldIsRepl;
            throw std::runtime_error("Failed to import "+name);
        }
        isRepl = oldIsRepl;
    } else if (CAST_TO(node.get(), ReturnAST*)) {
        // Return causes errors outside of function
        if (!inFunc) {
            throw std::runtime_error("Return outside of function");
        }
        // Return from func
        auto ret = std::reinterpret_pointer_cast<ReturnAST>(node);
        auto expr = ret->expr;
        return eval(expr);
    }
    return NULL;
}

Val Runner::call_func(std::shared_ptr<CallAST> &call) {
    std::string name = CAST_TO(call->expr.get(), VarAST*)->name;
    // Inbuilt functions
    if (name == "print") {
        std::vector<std::string> strings = {};
        for (auto &arg : call->args) {
            strings.push_back(eval(arg)->toStr());
        }
        for (std::string &str : strings) {
            printf("%s ", str.c_str());
        }
        puts("");
        return Val();
    } else if (name == "range") {
        return Val(
            "Lists don't exist yet, this function can only be used in loops"
        );
    }
    // Casting
    else if (name == "string") {
        if (call->args.size() != 1)
            badArgsErr(call->args.size(), 1);
        return eval(call->args.at(0))->toStr();
    } else if (name == "bool") {
        if (call->args.size() != 1)
            badArgsErr(call->args.size(), 1);
        return !!*eval(call->args.at(0));
    } else if (name == "int") {
        if (call->args.size() != 1)
            badArgsErr(call->args.size(), 1);
        Val ret = *eval(call->args.at(0));
        if (ret.type == 's')
            // Convert string to int
            return std::stoi(ret.strVal);
        return ret.toInt();
    } else if (name == "float") {
        if (call->args.size() != 1)
            badArgsErr(call->args.size(), 1);
        Val ret = *eval(call->args.at(0));
        if (ret.type == 's')
            // Convert string to float
            return std::stof(ret.strVal);
        return ret.toFloat();
    }
    // It's a normal function
    std::shared_ptr<FuncAST> func = state.get_func(name);
    // Check if it exists
    if (!func) {
        noFuncErr(name);
        return Val();
    }
    // Check args
    if (call->args.size() != func->args.size()) {
        badArgsErr(call->args.size(), func->args.size());
    }
    // All good, prep for execution
    bool oldFunc = inFunc;
    inFunc = true;
    // Lower scope
    state_t curState = state;
    state = {};
    state.outer_state = &curState;
    // Set args
    int argNo = 0;
    for (std::string &arg : func->args) {
        state.vars[arg] = eval(call->args[argNo]);
        argNo++;
    }
    // Execute
    std::shared_ptr<Val> ret = exec_body(func->body, false);
    // Clean up
    state = curState;
    inFunc = oldFunc;
    return ret ? *ret : Val();
}

std::shared_ptr<Val> Runner::exec_body(
    std::shared_ptr<StmtAST> &bodyStmt, bool _lower_scope /*= true*/
) {
    // Lower scope
    state_t curState = {};
    if (_lower_scope) {
        curState = state;
        state = {};
        state.outer_state = &curState;
    }
    // Exec body
    auto body = CAST_TO(bodyStmt.get(), BodyAST*);
    std::shared_ptr<Val> ret = NULL;
    for (std::shared_ptr<StmtAST> &node : body->exprs) {
        ret = exec_single(node);
        // It's the return val
        if (ret) break;
    }
    // Raise scope back up
    if (_lower_scope) {
        state = curState;
    }
    // Return
    return ret;
}

bool Runner::exec(std::vector<std::shared_ptr<StmtAST>> &ast) {
    try {
        for (auto &node : ast) {
            // Exec
            std::shared_ptr<Val> val = exec_single(node);
            if (val != NULL) {
                // Print in repl mode
                puts(val->toStr().c_str());
            }
        }
        return true;
    } catch (std::runtime_error const &error) {
        printf("Runtime Error: %s\n", error.what());
        return false;
    }
}
