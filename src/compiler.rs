use crate::common::IMPOSSIBLE_STATE;
use crate::lexer::TokenType;
use crate::parser::{ASTNode, ASTNode::*};
use crate::value::Value;
use crate::vm::Opcode;

#[derive(Debug)]
struct Compiler {
    pub bytes: Vec<u8>,
    pub consts: Vec<Value>,
}

impl Compiler {
    // Init
    pub fn new() -> Compiler {
        Compiler{bytes: Vec::new(), consts: Vec::new()}
    }

    pub fn push(&mut self, val: Value) {
        let index = self.consts.len();
        self.consts.push(val);
        if index > 2_usize.pow(24)-1 {
            panic!(
                "Too many constants! If you get this error, be sure to {}",
                 "open an issue and I'll fix it quickly!"
            );
        } else if index > u8::MAX.into() {
            // The len is too big for one byte, so push 3
            self.bytes.push(Opcode::PUSH3 as u8);
            self.bytes.push((index & 255) as u8);
            self.bytes.push(((index >> 8) & 255) as u8);
            self.bytes.push(((index >> 16) & 255) as u8);
        } else {
            self.bytes.push(Opcode::PUSH as u8);
            self.bytes.push(index as u8);
        }
    }
}

fn compile_binop(
    compiler: &mut Compiler,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>
) -> bool {
    // Compile sides
    if !compile_expr(compiler, &*lhs.clone()) {
        return false;
    }
    if !compile_expr(compiler, &*rhs.clone()) {
        return false;
    }
    // Compile op
    match op {
        // Simple single instructions
        TokenType::Plus | TokenType::PlusEquals => {
            compiler.bytes.push(Opcode::ADD as u8);
        },
        TokenType::Minus | TokenType::MinusEquals => {
            compiler.bytes.push(Opcode::SUB as u8);
        },
        TokenType::Times | TokenType::TimesEquals => {
            compiler.bytes.push(Opcode::MUL as u8);
        },
        TokenType::Div | TokenType::DivEquals => {
            compiler.bytes.push(Opcode::DIV as u8);
        },
        TokenType::Modulo => {
            compiler.bytes.push(Opcode::MOD as u8);
        },
        TokenType::And => {
            compiler.bytes.push(Opcode::AND as u8);
        },
        TokenType::Or => {
            compiler.bytes.push(Opcode::OR as u8);
        },
        TokenType::Xor => {
            compiler.bytes.push(Opcode::XOR as u8);
        },
        TokenType::Gt => {
            compiler.bytes.push(Opcode::GT as u8);
        },
        TokenType::Lt => {
            compiler.bytes.push(Opcode::LT as u8);
        },
        TokenType::EqualsEquals => {
            compiler.bytes.push(Opcode::EQ as u8);
        },
        // Harder ones that don't have a single instruction
        TokenType::NotEquals => {
            compiler.bytes.push(Opcode::EQ as u8);
            compiler.bytes.push(Opcode::NOT as u8);
        },
        TokenType::LtEquals => {
            compiler.bytes.push(Opcode::GT as u8);
            compiler.bytes.push(Opcode::NOT as u8);
        },
        TokenType::GtEquals => {
            compiler.bytes.push(Opcode::LT as u8);
            compiler.bytes.push(Opcode::NOT as u8);
        },
        TokenType::Equals => {},
        _ => todo!(),
    };
    // Set the variable
    if let TokenType::PlusEquals | TokenType::MinusEquals
        | TokenType::TimesEquals | TokenType::DivEquals
        | TokenType::Equals = op.clone()
    {
        if let VarExpr(s) = *lhs.clone() {
            compiler.push(Value::Str(s.to_string()));
            compiler.bytes.push(Opcode::SV as u8);
        }
    }
    return true;
}

fn compile_expr(compiler: &mut Compiler, node: &ASTNode) -> bool {
    match node {
        // Values
        VarExpr(val) => {
            compiler.push(Value::Str(val.clone()));
            compiler.bytes.push(Opcode::PV as u8);
        },
        StringExpr(val) => {
            compiler.push(Value::Str(val.clone()));
        },
        NumberExpr(val) => {
            compiler.push(Value::Int(val.clone()));
        },
        DecimalExpr(val) => {
            compiler.push(Value::Float(val.clone()));
        },
        BoolExpr(val) => {
            compiler.push(Value::Bool(val.clone()));
        },
        NoneExpr => {
            compiler.push(Value::None);
        },
        // Binops
        BinopExpr(lhs, op, rhs) => {
            return compile_binop(compiler, lhs, op, rhs);
        }
        _ => {
            return false;
        }
    };
    return true;
}

fn compile_stmt(compiler: &mut Compiler, node: &ASTNode) -> bool {
    match node {
        _ => {
            return compile_expr(compiler, node);
        }
    };
    return true;
}

pub fn compile(ast: Vec<ASTNode>) -> Option<(Vec<u8>, Vec<Value>)> {
    let mut compiler = Compiler::new();
    let ret = compile_stmt(&mut compiler, &ast[0]);
    println!("CC: {:?}", compiler);
    return Some((compiler.bytes, compiler.consts));
}
