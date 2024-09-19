use crate::lexer::TokenType;
use crate::backend::value::Value;
use crate::parser::{ASTNode, ExprNode};
use crate::parser::ASTNode::*;

use std::rc::Rc;

pub fn is_const_expr(node: &ASTNode) -> bool {
    // Very basic literals
    if let StringExpr(_) | NumberExpr(_) | DecimalExpr(_) | BoolExpr(_) | ByteExpr(_) | NoneExpr = node {
        return true;
    }
    return false;
}

pub fn to_value(node: &ASTNode) -> Option<Value> {
    Some(match node {
        StringExpr(val) => {
            Value::Str(Rc::new(val.clone()))
        },
        NumberExpr(val) => {
            Value::Int(*val)
        },
        DecimalExpr(val) => {
            Value::Float(*val)
        },
        BoolExpr(val) => {
            Value::Bool(*val)
        },
        ByteExpr(val) => {
            Value::Byte(*val)
        },
        NoneExpr => {
            Value::None
        },
        // Anything else
        _ => return None,
    })
}
pub fn to_node(node: Value) -> Option<ASTNode> {
    Some(match node {
        Value::Str(val) => {
            StringExpr((*val).clone())
        },
        Value::Int(val) => {
            NumberExpr(val)
        },
        Value::Float(val) => {
            DecimalExpr(val)
        },
        Value::Bool(val) => {
            BoolExpr(val)
        },
        Value::Byte(val) => {
            ByteExpr(val)
        },
        Value::None => {
            NoneExpr
        },
        // Anything else
        _ => return None,
    })
}

pub fn fold_binop(lhs: &ASTNode, op: TokenType, rhs: &ASTNode) -> Option<ASTNode> {
    if !is_const_expr(lhs) { return None; }
    if !is_const_expr(rhs) { return None; }
    // To values
    let lv = to_value(lhs)?;
    let rv = to_value(rhs)?;
    Some(match op {
        // binop_math1
        TokenType::Plus => {
            to_node((lv + rv).ok()?)?
        },
        TokenType::Minus => {
            to_node((lv - rv).ok()?)?
        },
        // binop_math2
        TokenType::Times => {
            to_node((lv * rv).ok()?)?
        },
        TokenType::Div => {
            // Don't divide by zero
            if rv.to_float() == 0.0 {
                return None;
            }
            to_node((lv / rv).ok()?)?
        },
        TokenType::Modulo => {
            // Don't mod by zero
            if rv.to_float() == 0.0 {
                return None;
            }
            to_node((lv % rv).ok()?)?
        },
        // binop_cmp
        // TODO: TokenType::In folding/optimization (mass-compare instruction?)
        TokenType::EqualsEquals => {
            ASTNode::BoolExpr(lv.eq(&rv))
        },
        TokenType::NotEquals => {
            ASTNode::BoolExpr(!lv.eq(&rv))
        },
        TokenType::Lt => {
            ASTNode::BoolExpr(lv.to_float() < rv.to_float())
        },
        TokenType::Gt => {
            ASTNode::BoolExpr(lv.to_float() > rv.to_float())
        },
        TokenType::LtEquals => {
            ASTNode::BoolExpr(!(lv.to_float() > rv.to_float()))
        },
        TokenType::GtEquals => {
            ASTNode::BoolExpr(!(lv.to_float() < rv.to_float()))
        },
        // binop_logic
        TokenType::And => {
            if !lv.is_truthy() {
                to_node(lv)
            } else {
                to_node(rv)
            }?
        },
        TokenType::Or => {
            if lv.is_truthy() {
                to_node(lv)
            } else { 
                to_node(rv)
            }?
        },
        TokenType::Xor => {
            ASTNode::BoolExpr(lv.is_truthy() != rv.is_truthy())
        },
        // Anything else can't be folded by binops like this
        _ => return None,
    })
}


pub fn fold_node(node: &ASTNode) -> Option<ASTNode> {
    Some(match node {
        BinopExpr(lhs, op, rhs) => {
            let new_lhs = fold_node(lhs)?;
            let new_rhs = fold_node(rhs)?;
            return fold_binop(&fold_node(&new_lhs)?, op.clone(), &fold_node(&new_rhs)?);
        },
        // Already folded
        x if is_const_expr(&x) => x.clone(),
        // Anything else can't be folded
        _ => return None,
    })
}

pub fn fold_expr(node: &ExprNode) -> Option<ExprNode> {
    Some(ExprNode { node: fold_node(&node.node)?, lvalue: node.lvalue })
}
