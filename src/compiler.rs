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

    pub fn write_num(&mut self, val: u32, bytes: u8, start: usize) {
        for i in 0..bytes {
            self.bytes[start + i as usize] = ((val >> (i * 8)) & 255) as u8;
        }
    }

    pub fn push(&mut self, val: Value) {
        // Get the index, or append
        let index = self.consts.iter().position(|i| i.clone() == val)
            .unwrap_or_else(|| {
            self.consts.push(val);
            self.consts.len() - 1
        });
        // Push the instruction
        if index > 2usize.pow(24)-1 {
            panic!("Too many different constants!");
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

fn compile_unary(
    compiler: &mut Compiler,
    op: &TokenType, val: &Box<ASTNode>
) -> bool {
    match op {
        // -/!
        TokenType::Minus => {
            compiler.push(Value::Int(0));
            if !compile_expr(compiler, &*val.clone()) {
                return false;
            }
            compiler.bytes.push(Opcode::SUB as u8);
        },
        TokenType::Not => {
            if !compile_expr(compiler, &*val.clone()) {
                return false;
            }
            compiler.bytes.push(Opcode::NOT as u8);
        },
        // ++/--
        TokenType::PlusPlus => {
            if !compile_expr(compiler, &*val.clone()) {
                return false;
            }
            compiler.push(Value::Int(1));
            compiler.bytes.push(Opcode::ADD as u8);
            compiler.bytes.push(Opcode::DUP as u8);
            if let VarExpr(s) = *val.clone() {
                compiler.push(Value::Str(s.to_string()));
                compiler.bytes.push(Opcode::SV as u8);
            }
        },
        TokenType::MinusMinus => {
            if !compile_expr(compiler, &*val.clone()) {
                return false;
            }
            compiler.push(Value::Int(1));
            compiler.bytes.push(Opcode::SUB as u8);
            compiler.bytes.push(Opcode::DUP as u8);
            if let VarExpr(s) = *val.clone() {
                compiler.push(Value::Str(s.to_string()));
                compiler.bytes.push(Opcode::SV as u8);
            }
        },
        _ => panic!("{}", IMPOSSIBLE_STATE),
    }
    return true;
}

fn compile_binop(
    compiler: &mut Compiler,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>
) -> bool {
    // Compile sides
    if let TokenType::Equals = op {} else {
        // No need to compile the value if it will just be reassigned
        if !compile_expr(compiler, &*lhs.clone()) {
            return false;
        }
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
        // Handled later
        TokenType::Equals => {},
        _ => panic!("That operator isn't implemented!"),
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
        // Binops/unary
        BinopExpr(lhs, op, rhs) => {
            return compile_binop(compiler, lhs, op, rhs);
        },
        UnaryExpr(op, val) => {
            return compile_unary(compiler, op, val);
        },
        _ => {
            return false;
        }
    };
    return true;
}

fn compile_body(compiler: &mut Compiler, node: &ASTNode) -> bool {
    let BodyStmt(nodes) = node else {
        if *node == Nop {
            return true;
        }
        panic!("compile_body got non-body node!");
    };
    // Compile all nodes
    for node in nodes {
        if !compile_stmt(compiler, node) {
            // Pass it down
            return false;
        }
    }
    return true;
}

fn compile_stmt(compiler: &mut Compiler, node: &ASTNode) -> bool {
    match node {
        // Statments
        LetStmt(name, val) => {
            compile_expr(compiler, val);
            compiler.push(Value::Str(name.to_string()));
            compiler.bytes.push(Opcode::DV as u8);
        },
        IfStmt(cond, body, else_part) => {
            // The condition must be a expr, so no need to match against stmts
            compile_expr(compiler, &**cond);

            // This is for when boolean not is forgotten
            if **body == Nop {
                compiler.bytes.push(Opcode::NOT as u8);
                // Push the jump offset (which will be filled later)
                compiler.bytes.push(Opcode::JMPNT as u8);
                compiler.bytes.push(0);
                // Compile body
                let pos = compiler.bytes.len();
                compile_body(compiler, &**else_part);
                compiler.bytes[pos - 1] = (compiler.bytes.len() - pos + 1)
                    as u8;
                compiler.bytes.push(Opcode::NOP as u8);
                return true
            }

            // Push the jump offset (which will be filled later)
            compiler.bytes.push(Opcode::JMPNT as u8);
            compiler.bytes.push(0);
            let pos = compiler.bytes.len();
            // Compile true part
            compile_body(compiler, &**body);
            compiler.bytes[pos - 1] = (compiler.bytes.len() - pos + 1) as u8;

            // The else
            if **else_part != Nop {
                // Prep exit offset
                compiler.bytes[pos - 1] += 2;
                compiler.bytes.push(Opcode::JMPU as u8);
                compiler.bytes.push(0);
                let pos = compiler.bytes.len();
                // Compile else part
                compile_body(compiler, &**else_part);
                compiler.bytes[pos - 1] = (compiler.bytes.len() - pos + 1) as u8;
            }
            // Might jump too far, so cushion with NOP
            compiler.bytes.push(Opcode::NOP as u8);
        },
        WhileStmt(cond, body) => {
            // Start, exit jump + cond
            let pos = compiler.bytes.len();
            compile_expr(compiler, &**cond);
            compiler.bytes.push(Opcode::JMPNT as u8);
            compiler.bytes.push(0);
            let offpos = compiler.bytes.len();

            // Compile body
            compile_body(compiler, &**body);
            compiler.bytes[offpos - 1] = (compiler.bytes.len() - offpos + 3) as u8;

            // Backwards jump
            compiler.bytes.push(Opcode::JMPB as u8);
            compiler.bytes.push((compiler.bytes.len() - pos) as u8);
            compiler.bytes.push(Opcode::NOP as u8);
        },
        Nop => {
            // Nop isn't turned into the NOP instruction because it's useless
        },
        _ => {
            return compile_expr(compiler, node);
        }
    };
    return true;
}

pub fn compile(ast: Vec<ASTNode>) -> Option<(Vec<u8>, Vec<Value>)> {
    let mut compiler = Compiler::new();
    for node in ast {
        if !compile_stmt(&mut compiler, &node) {
            return Option::None;
        }
    }
    println!("CC: {:?}", compiler);
    return Some((compiler.bytes, compiler.consts));
}
