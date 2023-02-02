#![allow(dead_code, unused_imports)]
use crate::parser::ASTNode;
use ASTNode::*;
use crate::lexer::TokenType;
use crate::value::Value;

// Interpreter state
pub struct Interpreter {
    pub is_repl: bool,
    pub has_err: bool,
    pub in_func: bool,
}

fn eval(interpreter: &mut Interpreter, node: &ASTNode) -> Value {
    return match node {
        // Normal boring values
        StringExpr(val) => Value::Str(val.clone()),
        NumberExpr(val) => Value::Int(val.clone()),
        DecimalExpr(val) => Value::Float(val.clone()),
        BoolExpr(val) => Value::Bool(val.clone()),
        NoneExpr => Value::None,
        // TODO: Vars
        VarExpr(_val) => Value::None,
        // Unary
        UnaryExpr(unary, val) => {
            // "&**" is a tad cursed, but this is rust after all.
            let val = eval(interpreter, &**val);
            // Unary is either ! or - or something went wrong
            match unary {
                TokenType::Not => Value::Bool(!val.is_truthy()),
                TokenType::Minus => Value::Int(0) - val,
                _ => val
            }
        },
        // Binops
        BinopExpr(left, op, right) => {
            // Get values
            let left = eval(interpreter, &**left);
            let right = eval(interpreter, &**right);
            // Do the binop
            match op {
                // Normal ops
                TokenType::Plus => left + right,
                TokenType::Minus => left - right,
                TokenType::Times => left * right,
                TokenType::Div => left / right,
                TokenType::Modulo => left % right,
                TokenType::EqualsEquals => Value::Bool(left.eq(right)),
                TokenType::NotEquals => Value::Bool(!left.eq(right)),
                // Logical
                TokenType::And => Value::Bool(left.is_truthy() && right.is_truthy()),
                TokenType::Or => Value::Bool(left.is_truthy() || right.is_truthy()),
                TokenType::Xor => Value::Bool(left.is_truthy() != right.is_truthy()),
                _ => Value::None
            }
        },
        // Default to none
        _ => Value::None,
    };
}

pub fn run(interpreter: &mut Interpreter, ast: Vec<ASTNode>) {
    for node in ast {
        println!("{:?}", eval(interpreter, &node));
    }
}
