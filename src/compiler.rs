use std::cmp::Ordering;
use std::path::PathBuf;

use crate::Arguments;
use crate::common::IMPOSSIBLE_STATE;
use crate::lexer::TokenType;
use crate::parser::{ASTNode, ASTNode::*};
use crate::value::Value;
use crate::vm::Opcode;

#[derive(Debug)]
pub struct Program {
    // Opcodes and constants
    pub ops: Vec<u32>,
    pub consts: Vec<Value>,
    // Function locations (name, byte pos, arg num))
    pub functis: Vec<(String, usize, i32)>,
    // Import dir
    pub path: PathBuf,

    // Side tables
    line_table: Vec<(u32, u32, u32)>,
    file_table: Vec<(u32, u32, String)>,

    // Compiler info
    // The functi being compiled (name, args, address)
    functi: (String, u8, u32),
    // If RS and LEVI are needed
    needs_scope: bool,
    // Where in the byte code the current file started
    inc_start: u32,
}

impl Program {
    // Init
    pub fn new() -> Program {
        Program{
            ops: vec![], consts: vec![],
            functis: Vec::new(),
            path: PathBuf::from("."),
            line_table: vec![], file_table: vec![],
            needs_scope: false, inc_start: 0,
            functi: ("".to_string(), 0, 0),
        }
    }

    fn bin_range<T: Clone>(index: u32, table: &Vec<(u32, u32, T)>) -> Option<T> {
        table.binary_search_by(
            |x| {
                if x.0 > index {
                    Ordering::Greater
                } else if x.1 < index {
                    Ordering::Less
                } else { Ordering::Equal }
            }
        ).and_then(|x| Ok(table[x].2.clone())).ok()
    }

    pub fn get_info(&mut self, index: u32) -> (u32, String) {
        let file = Self::bin_range(index, &self.file_table)
            .unwrap_or("Unknown File".to_string());
        let line = Self::bin_range(index, &self.line_table)
            .unwrap_or(0);
        (line, file)
    }

    /*pub fn push(&mut self, val: Value) {
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
            self.ops.push(((index >> 16) & 255) as u8);
            self.ops.push(((index >> 8) & 255) as u8);
            self.ops.push((index & 255) as u8);
        } else {
            self.ops.push(Opcode::PUSH as u8);
            self.ops.push(index as u8);
        }
    }

    pub fn fill_jmp(&mut self, start: usize, mut i: usize) {
        if i == 0 {
            i = self.ops.len() - start - 2;
        }
        self.ops[start] = ((i >> 16) & 255) as u8;
        self.ops[start + 1] = ((i >> 8) & 255) as u8;
        self.ops[start + 2] = (i & 255) as u8;
    }*/
}

/*fn compile_unary(
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

fn compile_short_binop(
    program: &mut Program,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>,
    clean: bool
) -> bool {
    // Compiles short circuiting operators (&& and ||)
    // Uses jump instructions to:
    // Turn `a() && b()` into `r = a(); if r { r = b() }; r`
    // Turn `a() || b()` into `r = a(); if !r { r = b() }; r`
    if !compile_expr(program, lhs) {
        return false;
    }
    program.ops.push(Opcode::DUP as u8);
    if op == &TokenType::Or {
        program.ops.push(Opcode::NOT as u8);
    }
    // Start jump
    program.ops.push(Opcode::JMPNT as u8);
    let pos = program.ops.len();
    program.ops.push(0);
    program.ops.push(0);
    program.ops.push(0);
    // It's 'b' (the left)
    program.ops.push(Opcode::DEL as u8);
    if !compile_expr(program, rhs) {
        return false;
    }
    // End the jump
    program.fill_jmp(pos, 0);
    if clean {
        program.ops.push(Opcode::DEL as u8);
    }
    return true;
}

fn compile_binop(
    program: &mut Program,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>,
    clean: bool
) -> bool {
    // Short circuiting ops are special
    if op == &TokenType::And || op == &TokenType::Or {
        return compile_short_binop(program, lhs, op, rhs, clean);
    }
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
        TokenType::Modulo | TokenType::ModEquals => {
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
        TokenType::In => {
            program.ops.push(Opcode::IN as u8);
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
        | TokenType::ModEquals | TokenType::Equals = op.clone()
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
        CallExpr(expr, args) => {
            // Push the call. Name, arg count, args (in reverse order)
            for arg in args {
                if !compile_expr(program, arg) {
                    return false;
                }
            }
            program.push(Value::Int(args.len() as i32));
            if let ASTNode::VarExpr(name) = *expr.clone() {
                if name.contains("::") {
                    // Variable name
                    if !compile_expr(program, expr) {
                        return false;
                    }
                } else {
                    // Function name
                    program.push(Value::Functi(name.clone()));
                }
            } else {
                if !compile_expr(program, expr) {
                    return false;
                }
            }
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
    nodes: &Vec<ASTNode>, manual_scope: bool
) -> bool {
    // Lower scope
    let scope_pos = program.ops.len();
    let old_needs_scope = program.needs_scope;
    program.needs_scope = false;
    if !manual_scope {
        program.ops.push(Opcode::NOP as u8);
    }
    // Compile all nodes
    for node in nodes {
        if !compile_stmt(program, args, node, false) {
            // Pass it down
            return false;
        }
    }
    // Raise scope
    if !manual_scope && program.needs_scope {
        program.ops.push(Opcode::RS as u8);
        program.ops[scope_pos] = Opcode::LEVI as u8;
    }
    program.needs_scope = old_needs_scope;
    return true;
}
fn compile_body(
    program: &mut Program, args: &mut Arguments, node: &ASTNode, manual_scope: bool
) -> bool {
    let BodyStmt(nodes) = node else {
        if *node == Nop {
            return true;
        }
        panic!("compile_body got non-body node!");
    };
    return _compile_body(program, args, nodes, manual_scope);
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
            program.needs_scope = true;
        },
        IfStmt(cond, body, else_part) => {
            // The condition must be a expr, so no need to match against stmts
            compile_expr(program, cond);

            // This is for when boolean not is forgotten
            if **body == Nop {
                program.ops.push(Opcode::NOT as u8);
                // Push the jump offset (which will be filled later)
                program.ops.push(Opcode::JMPNT as u8);
                let pos = program.ops.len();
                program.ops.push(0);
                program.ops.push(0);
                program.ops.push(0);
                // Compile body
                compile_stmt(program, args, else_part, false);
                program.fill_jmp(pos, 0);
                return true;
            }

            // Push the jump offset (which will be filled later)
            program.ops.push(Opcode::JMPNT as u8);
            let pos = program.ops.len();
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);
            // Compile true part
            compile_body(program, args, body, false);
            let offset = program.ops.len() - pos - 2;

            // The else
            if **else_part != Nop {
                program.fill_jmp(pos, offset + 4);
                // Prep exit offset
                program.ops.push(Opcode::JMPU as u8);
                let pos = program.ops.len();
                program.ops.push(0);
                program.ops.push(0);
                program.ops.push(0);
                // Compile else part
                compile_stmt(program, args, else_part, false);
                program.fill_jmp(pos, 0);
            } else {
                program.fill_jmp(pos, offset);
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
            let offpos = program.ops.len();
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);

            // Lower scope
            program.ops.push(Opcode::LEVI as u8);

            // Set the loop var
            program.push(Value::Str(var.to_string()));
            program.ops.push(Opcode::DV as u8);

            // Body
            compile_body(program, args, body, true);

            // Raise scope
            program.ops.push(Opcode::RS as u8);

            // Backwards jump
            program.ops.push(Opcode::JMPB as u8);
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);
            program.fill_jmp(program.ops.len() - 3, program.ops.len() - pos - 1);
            // Clean up the iter
            program.fill_jmp(offpos, 0);
            program.ops.push(Opcode::DEL as u8);
        },
        WhileStmt(cond, body) => {
            // Start, exit jump + cond
            let pos = program.ops.len();
            compile_expr(program, cond);
            program.ops.push(Opcode::JMPNT as u8);
            let offpos = program.ops.len();
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);

            // Compile body
            compile_body(program, args, body, false);

            // Backwards jump
            program.ops.push(Opcode::JMPB as u8);
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);
            program.fill_jmp(program.ops.len() - 3, program.ops.len() - pos - 1);
            program.fill_jmp(offpos, 0);
        },
        BodyStmt(nodes) => return _compile_body(program, args, nodes, false),
        FunctiStmt(name, fargs, body) => {
            // Declare function
            program.push(Value::Int(fargs.len() as i32));
            program.push(Value::Str(name.to_string()));
            program.ops.push(Opcode::FN as u8);
            // Jump around function
            program.ops.push(Opcode::JMPU as u8);
            let pos = program.ops.len();
            program.ops.push(0);
            program.ops.push(0);
            program.ops.push(0);
            // Update compiler state
            program.functi = (
                name.clone(), fargs.len() as u8, program.ops.len() as u32
            );
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
            program.fill_jmp(pos, 0);
        },
        ReturnStmt(ret) => {
            let mut do_tco = false;
            if let CallExpr(expr, args) = *ret.clone() {
                if let ASTNode::VarExpr(name) = *expr {
                    do_tco = name == program.functi.0
                        && args.len() == program.functi.1 as usize;
                }
                if do_tco {
                    // Tail call
                    for ref arg in args.iter().rev() {
                        if !compile_expr(program, arg) {
                            return false;
                        }
                    }
                    program.ops.push(Opcode::TCO as u8);
                    program.ops.push(0);
                    program.ops.push(0);
                    program.ops.push(0);
                    program.fill_jmp(
                        program.ops.len() - 3,
                        program.ops.len() - program.functi.2 as usize - 1
                    );
                }
            }
            if !do_tco {
                // Compile return value
                if !compile_expr(program, ret) {
                    return false;
                }
            }
            // Return return value
            program.ops.push(Opcode::RET as u8);
        },
        ImportStmt() => {
            program.file_table.push((
                program.inc_start, program.ops.len() as u32, args.name.clone()
            ));
            program.inc_start = program.ops.len() as u32;
        },
        EndImportStmt(file) => {
            program.file_table.push((
                program.inc_start, program.ops.len() as u32, file.clone()
            ));
            program.inc_start = program.ops.len() as u32;
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
    program.inc_start = program.ops.len() as u32;
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
    // End file
    program.file_table.push((
        program.inc_start, program.ops.len() as u32, args.name.clone()
    ));
    return true;
}
*/

pub fn compile(
    ast: Vec<ASTNode>, args: &mut Arguments, program: &mut Program
) -> bool {
    return true;
}