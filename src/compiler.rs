use std::fs::read_to_string;
use std::path::PathBuf;

use crate::Arguments;
use crate::common::{print_err, ErrType, IMPOSSIBLE_STATE};
use crate::lexer::TokenType;
use crate::parser::{ASTNode, ASTNode::*};
use crate::to_ast;
use crate::value::Value;
use crate::vm::Opcode;

use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct Program {
    // Opcodes and constants
    pub ops: Vec<u8>,
    pub consts: Vec<Value>,
    // Function locations (name : (byte pos, arg num))
    pub functis: FxHashMap<String, (usize, i32)>,
    // Import dir
    pub path: PathBuf,
}

impl Program {
    // Init
    pub fn new() -> Program {
        Program{
            ops: Vec::new(), consts: Vec::new(),
            functis: FxHashMap::default(),
            path: PathBuf::from(".")
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
            self.ops.push(Opcode::PUSH3 as u8);
            self.ops.push((index & 255) as u8);
            self.ops.push(((index >> 8) & 255) as u8);
            self.ops.push(((index >> 16) & 255) as u8);
        } else {
            self.ops.push(Opcode::PUSH as u8);
            self.ops.push(index as u8);
        }
    }
}

fn compile_unary(
    program: &mut Program,
    op: &TokenType, val: &Box<ASTNode>
) -> bool {
    match op {
        // -/!
        TokenType::Minus => {
            program.push(Value::Int(0));
            if !compile_expr(program, val) {
                return false;
            }
            program.ops.push(Opcode::SUB as u8);
        },
        TokenType::Not => {
            if !compile_expr(program, val) {
                return false;
            }
            program.ops.push(Opcode::NOT as u8);
        },
        // ++/--
        TokenType::PlusPlus => {
            if !compile_expr(program, val) {
                return false;
            }
            program.push(Value::Int(1));
            program.ops.push(Opcode::ADD as u8);
            program.ops.push(Opcode::DUP as u8);
            if let VarExpr(s) = *val.clone() {
                program.push(Value::Str(s));
                program.ops.push(Opcode::SV as u8);
            }
        },
        TokenType::MinusMinus => {
            if !compile_expr(program, val) {
                return false;
            }
            program.push(Value::Int(1));
            program.ops.push(Opcode::SUB as u8);
            program.ops.push(Opcode::DUP as u8);
            if let VarExpr(s) = *val.clone() {
                program.push(Value::Str(s));
                program.ops.push(Opcode::SV as u8);
            }
        },
        _ => panic!("{}", IMPOSSIBLE_STATE),
    }
    return true;
}

fn compile_set(program: &mut Program, var: &ASTNode) -> bool {
    // Recursively set
    if let VarExpr(s) = var.clone() {
        program.push(Value::Str(s));
        program.ops.push(Opcode::SV as u8);
    } else if let IndexExpr(list, index) = var.clone() {
        if !compile_expr(program, &list) {
            return false;
        }
        if !compile_expr(program, &index) {
            return false;
        }
        program.ops.push(Opcode::SKY as u8);
        // Indexes are attached to something, make sure it reattaches
        if !compile_set(program, &list) {
            return false;
        }
    }
    return true;
}

fn compile_binop(
    program: &mut Program,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>,
    clean: bool
) -> bool {
    // Compile sides
    if op != &TokenType::Equals {
        // No need to compile the value if it will just be reassigned
        if !compile_expr(program, lhs) {
            return false;
        }
    }
    if !compile_expr(program, rhs) {
        return false;
    }
    // Compile op
    match op {
        // Simple single instructions
        TokenType::Plus | TokenType::PlusEquals => {
            program.ops.push(Opcode::ADD as u8);
        },
        TokenType::Minus | TokenType::MinusEquals => {
            program.ops.push(Opcode::SUB as u8);
        },
        TokenType::Times | TokenType::TimesEquals => {
            program.ops.push(Opcode::MUL as u8);
        },
        TokenType::Div | TokenType::DivEquals => {
            program.ops.push(Opcode::DIV as u8);
        },
        TokenType::Modulo => {
            program.ops.push(Opcode::MOD as u8);
        },
        TokenType::And => {
            program.ops.push(Opcode::AND as u8);
        },
        TokenType::Or => {
            program.ops.push(Opcode::OR as u8);
        },
        TokenType::Xor => {
            program.ops.push(Opcode::XOR as u8);
        },
        TokenType::Gt => {
            program.ops.push(Opcode::GT as u8);
        },
        TokenType::Lt => {
            program.ops.push(Opcode::LT as u8);
        },
        TokenType::EqualsEquals => {
            program.ops.push(Opcode::EQ as u8);
        },
        // Harder ones that don't have a single instruction
        TokenType::NotEquals => {
            program.ops.push(Opcode::EQ as u8);
            program.ops.push(Opcode::NOT as u8);
        },
        TokenType::LtEquals => {
            program.ops.push(Opcode::LT as u8);
            program.ops.push(Opcode::NOT as u8);
        },
        TokenType::GtEquals => {
            program.ops.push(Opcode::GT as u8);
            program.ops.push(Opcode::NOT as u8);
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
        if !compile_set(program, lhs) {
            return false;
        }

    } else if clean {
        // Clean up the stack
        program.ops.push(Opcode::DEL as u8);
    }
    return true;
}

fn compile_expr(program: &mut Program, node: &ASTNode) -> bool {
    match node {
        // Values
        VarExpr(val) => {
            program.push(Value::Str(val.clone()));
            program.ops.push(Opcode::PV as u8);
        },
        StringExpr(val) => {
            program.push(Value::Str(val.clone()));
        },
        NumberExpr(val) => {
            program.push(Value::Int(*val));
        },
        DecimalExpr(val) => {
            program.push(Value::Float(*val));
        },
        BoolExpr(val) => {
            program.push(Value::Bool(*val));
        },
        NoneExpr => {
            program.push(Value::None);
        },
        ByteExpr(val) => {
            program.push(Value::Byte(*val));
        },
        // Binop/unary
        BinopExpr(lhs, op, rhs) => {
            return compile_binop(program, lhs, op, rhs, false);
        }
        UnaryExpr(op, val) => {
            return compile_unary(program, op, val);
        },
        // Calls
        CallExpr(name, args) => {
            // Push the call. Name, arg count, args (in reverse order)
            for arg in args {
                if !compile_expr(program, arg) {
                    return false;
                }
            }
            program.push(Value::Int(args.len() as i32));
            program.push(Value::Str(name.clone()));
            program.ops.push(Opcode::CALL as u8);
        },
        // List
        ListExpr(keys, values, fast) => {
            // Build the list
            for at in 0..values.len() {
                if !compile_expr(program, &values[at]) {
                    return false;
                }
                if !*fast {
                    program.push(Value::Str(keys[at].clone()));
                }
            }
            // Push
            program.push(Value::Int(values.len() as i32));
            if *fast {
                program.ops.push(Opcode::LFL as u8);
            } else {
                program.ops.push(Opcode::LL as u8);
            }
        },
        // Indexes
        IndexExpr(val, index) => {
            // Push
            if !compile_expr(program, val) {
                return false;
            }
            if !compile_expr(program, index) {
                return false;
            }
            program.ops.push(Opcode::INX as u8);
        },
        _ => {
            panic!("Unknown token! {:?}", node);
        }
    };
    return true;
}

fn _compile_body(
    program: &mut Program, args: &mut Arguments,
    nodes: &Vec<ASTNode>, call: bool
) -> bool {
    // Lower scope
    if !call {
        program.ops.push(Opcode::LEVI as u8);
    }
    // Compile all nodes
    for node in nodes {
        if !compile_stmt(program, args, node, false) {
            // Pass it down
            return false;
        }
    }
    // Raise scope
    if !call {
        program.ops.push(Opcode::RS as u8);
    }
    return true;
}
fn compile_body(
    program: &mut Program, args: &mut Arguments, node: &ASTNode, call: bool
) -> bool {
    let BodyStmt(nodes) = node else {
        if *node == Nop {
            return true;
        }
        panic!("compile_body got non-body node!");
    };
    return _compile_body(program, args, nodes, call);
}

fn compile_stmt(
    program: &mut Program, args: &mut Arguments, node: &ASTNode, dirty: bool
) -> bool {
    match node {
        // Statements
        LetStmt(name, val) => {
            compile_expr(program, val);
            program.push(Value::Str(name.to_string()));
            program.ops.push(Opcode::DV as u8);
        },
        IfStmt(cond, body, else_part) => {
            // The condition must be a expr, so no need to match against stmts
            compile_expr(program, cond);

            // This is for when boolean not is forgotten
            if **body == Nop {
                program.ops.push(Opcode::NOT as u8);
                // Push the jump offset (which will be filled later)
                program.ops.push(Opcode::JMPNT as u8);
                program.ops.push(0);
                // Compile body
                let pos = program.ops.len();
                compile_stmt(program, args, else_part, false);
                program.ops[pos - 1] = (program.ops.len() - pos + 1)
                    as u8;
                return true
            }

            // Push the jump offset (which will be filled later)
            program.ops.push(Opcode::JMPNT as u8);
            program.ops.push(0);
            let pos = program.ops.len();
            // Compile true part
            compile_body(program, args, body, false);
            program.ops[pos - 1] = (program.ops.len() - pos + 1) as u8;

            // The else
            if **else_part != Nop {
                // Prep exit offset
                program.ops[pos - 1] += 2;
                program.ops.push(Opcode::JMPU as u8);
                program.ops.push(0);
                let pos = program.ops.len();
                // Compile else part
                compile_stmt(program, args, else_part, false);
                program.ops[pos - 1] = (program.ops.len() - pos + 1) as u8;
            }
        },
        LoopStmt(var, iter, body) => {
            // Load iter
            compile_expr(program, iter);
            program.ops.push(Opcode::TITR as u8);
            let pos = program.ops.len();
            program.ops.push(Opcode::NXT as u8);

            // Exit jump
            program.ops.push(Opcode::JMPNT as u8);
            program.ops.push(0);
            let offpos = program.ops.len();

            // Lower scope
            program.ops.push(Opcode::LEVI as u8);

            // Set the loop var
            program.push(Value::Str(var.to_string()));
            program.ops.push(Opcode::DOS as u8);

            // Body
            compile_body(program, args, body, true);

            // Raise scope
            program.ops.push(Opcode::RS as u8);

            // Backwards jump
            program.ops.push(Opcode::JMPB as u8);
            program.ops.push((program.ops.len() - pos) as u8);
            // Clean up the iter
            program.ops.push(Opcode::DEL as u8);
            program.ops[offpos - 1] = (program.ops.len() - offpos) as u8;
        },
        WhileStmt(cond, body) => {
            // Start, exit jump + cond
            let pos = program.ops.len();
            compile_expr(program, cond);
            program.ops.push(Opcode::JMPNT as u8);
            program.ops.push(0);
            let offpos = program.ops.len();

            // Compile body
            compile_body(program, args, body, false);

            // Backwards jump
            program.ops.push(Opcode::JMPB as u8);
            program.ops.push((program.ops.len() - pos) as u8);
            program.ops[offpos - 1] = (program.ops.len() - offpos + 1) as u8;
        },
        BodyStmt(nodes) => return _compile_body(program, args, nodes, false),
        FunctiStmt(name, fargs, body) => {
            // Declare function
            program.push(Value::Int(fargs.len() as i32));
            program.push(Value::Str(name.to_string()));
            program.ops.push(Opcode::FN as u8);
            // Jump around function
            program.ops.push(Opcode::JMPU as u8);
            program.ops.push(0);
            let pos = program.ops.len();
            // Load args
            for arg in fargs {
                program.push(Value::Str(arg.to_string()));
                program.ops.push(Opcode::DV as u8);
            }
            // Compile body
            compile_body(program, args, body, true);
            // Return
            program.push(Value::None);
            program.ops.push(Opcode::RET as u8);
            // Fill jump
            program.ops[pos - 1] = (program.ops.len() - pos + 1) as u8;
        },
        ReturnStmt(ret) => {
            // Compile return value
            compile_expr(program, ret);
            // Return return value
            program.ops.push(Opcode::RET as u8);
        },
        ImportStmt(file) => {
            // Open file
            let mut path = program.path.clone();
            path.push(file);
            // Try x.sk first
            path.set_extension("sk");
            if let Ok(src) = read_to_string(path.to_str().unwrap()) {
                args.source = src;
            } else {
                // Try x.sack
                path.set_extension("sack");
                if let Ok(src) = read_to_string(path.to_str().unwrap()) {
                    args.source = src;
                } else {
                    // No such file
                    print_err(
                        format!("cannot import {}", file).as_str(),
                        ErrType::Err,
                        args.extensions.contains(&"color".to_string())
                    );
                    return false;
                }
            }
            // Gen ast
            let Some(ast) = to_ast(args) else {
                return false;
            };
            // Fix import path
            let old_path = program.path.clone();
            program.path = program.path.join(file);
            program.path.pop();
            // Compile
            if !compile(ast, args, program) {
                return false;
            }
            program.path = old_path;
        },
        Nop => {
            // Nop isn't turned into the NOP instruction because it's useless
        },
        // Expressions
        // Binops don't always return, so let them manage cleaning the stack
        BinopExpr(lhs, op, rhs) => {
            return compile_binop(program, lhs, op, rhs, !dirty);
        },
        _ => {
            let ret = compile_expr(program, node);
            if !dirty {
                // Remove unused values from the stack
                program.ops.push(Opcode::DEL as u8);
            }
            return ret;
        }
    };
    return true;
}

pub fn compile(
    ast: Vec<ASTNode>, args: &mut Arguments, program: &mut Program
) -> bool {
    if ast.is_empty() {
        return true;
    }
    // Compile
    for node in &ast[..ast.len()-1] {
        if !compile_stmt(program, args, node, false) {
            return false;
        }
    }
    // If repl, compile the last value without cleaning up
    // Else just compile normally
    if !compile_stmt(program, args, ast.last().unwrap(), args.is_repl) {
        return false;
    }
    // Jumps go onto the next instruction, so a nop is needed at the end
    program.ops.push(Opcode::NOP as u8);
    return true;
}
