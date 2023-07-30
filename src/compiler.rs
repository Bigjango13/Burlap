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
}

impl Program {
    // Init
    pub fn new() -> Program {
        Program{
            ops: vec![], consts: vec![],
            functis: Vec::new(),
            path: PathBuf::from("."),
            line_table: vec![], file_table: vec![],
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
}

#[allow(unused)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Reg {
    R0 = 0, R1 = 1, R2 = 2, R3 = 3, R4 = 4,
    R5 = 5, R6 = 6, R7 = 7, R8 = 8, R9 = 9,
    R10 = 10, R11 = 11, R12 = 12, R13 = 13,
    R14 = 14, R15 = 15, Stack = 16
}

#[inline]
fn to_reg(i: u8) -> Reg {
    unsafe { std::mem::transmute(i) }
}

pub struct Compiler {
    pub program: Program,

    // Compiler info
    // The functi being compiled (name, args, address)
    //functi: (String, u8, u32),
    // If RS and LEVI are needed
    needs_scope: bool,
    // If VARG and CARG are needed
    needs_args: bool,
    // Where in the byte code the current file started
    inc_start: u32,
    // Registers
    regs: [bool; 17]
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            program: Program::new(),
            needs_scope: false, inc_start: 0,
            //functi: ("".to_string(), 0, 0),
            regs: [true; 17], needs_args: false
        }
    }

    // Instruction wrappers
    #[inline]
    pub fn add_op_args(&mut self, op: Opcode, a: u8, b: u8, c: u8) {
        self.program.ops.push(
            ((op as u32) << 24)
            + ((a as u32) << 16)
            + ((b as u32) << 8)
            + (c as u32)
        );
    }

    #[inline]
    pub fn add_op(&mut self, op: Opcode) {
        self.add_op_args(op, 0, 0, 0);
    }

    #[inline]
    fn copy(&mut self, src: Reg, dst: Reg) {
        self.add_op_args(Opcode::CP, src as u8, dst as u8, 0);
    }

    // Register allocation
    fn alloc_reg(&mut self) -> Reg {
        let Some(reg) = self.regs.iter().position(|i| *i) else {
            // No available registers, fallback to stack
            return Reg::Stack;
        };
        self.regs[reg] = false;
        return to_reg(reg as u8);
    }

    #[inline]
    fn free_reg(&mut self, reg: Reg) {
        if 16 <= reg as u8 {
            return;
        }
        self.regs[reg as usize] = true;
    }

    fn push(&mut self, val: Value) -> Reg {
        // Get the index, or append
        let index = self.program.consts.iter().position(|i| i.clone() == val)
            .unwrap_or_else(|| {
            self.program.consts.push(val);
            self.program.consts.len() - 1
        });
        // Push the instruction
        if index > 2usize.pow(24)-1 {
            panic!("Too many different constants! You have over 16777215 constants!!");
        } else if index > 2usize.pow(16)-1 {
            // The len is too big for two bytes, so use three
            self.add_op_args(
                Opcode::LDL,
                ((index >> 16) & 255) as u8,
                ((index >> 8) & 255) as u8,
                (index & 255) as u8
            );
            return Reg::Stack;
        } else {
            // Get a register and push
            let reg = self.alloc_reg();
            self.add_op_args(
                Opcode::LD,
                ((index >> 8) & 255) as u8,
                (index & 255) as u8,
                reg as u8
            );
            return reg;
        }
    }

    fn fill_jmp(&mut self, pos: usize, mut i: usize, reg: Option<Reg>) {
        if i == 0 {
            i = self.program.ops.len() - pos + 1;
        }
         let op = &mut self.program.ops[pos - 1];
        if let Some(x) = reg {
            if i > 2usize.pow(16)-1 {
                panic!("jump offset is over 2 bytes!");
            }
            *op += (x as u8 as u32) << 16;
        } else {
            if i > 2usize.pow(24)-1 {
                panic!("jump offset is over 3 bytes!");
            }
            *op += (i & 255 << 16) as u32;
        }
        *op += (i & 255 << 8) as u32;
        *op += (i & 255 << 0) as u32;
    }
}


fn compile_unary(
    compiler: &mut Compiler,
    op: &TokenType, val: &Box<ASTNode>
) -> Option<Reg> {
    Some(match op {
        // -/!
        TokenType::Minus => {
            let ret = compile_expr(compiler, val)?;
            let tmp = compiler.push(Value::Int(0));
            compiler.add_op_args(Opcode::SUB, tmp as u8, ret as u8, ret as u8);
            compiler.free_reg(tmp);
            ret
        },
        TokenType::Not => {
            let ret = compile_expr(compiler, val)?;
            compiler.add_op_args(Opcode::NOT, ret as u8, ret as u8, 0);
            ret
        },
        // ++/--
        TokenType::PlusPlus => {
            let ret = compile_expr(compiler, val)?;
            let tmp = compiler.push(Value::Int(1));
            compiler.add_op_args(Opcode::ADD, ret as u8, tmp as u8, ret as u8);
            compiler.free_reg(tmp);
            let VarExpr(ref s) = **val else {
                panic!("++ needs a var, how did you do this?");
            };
            let tmp = compiler.push(Value::Str(s.clone()));
            compiler.add_op_args(Opcode::SV, tmp as u8, ret as u8, 0);
            compiler.free_reg(tmp);
            ret
        },
        TokenType::MinusMinus => {
            let ret = compile_expr(compiler, val)?;
            let tmp = compiler.push(Value::Int(1));
            compiler.add_op_args(Opcode::SUB, ret as u8, tmp as u8, ret as u8);
            compiler.free_reg(tmp);
            let VarExpr(ref s) = **val else {
                panic!("-- needs a var, how did you do this?");
            };
            let tmp = compiler.push(Value::Str(s.clone()));
            compiler.add_op_args(Opcode::SV, tmp as u8, ret as u8, 0);
            compiler.free_reg(tmp);
            ret
        },
        _ => panic!("{}", IMPOSSIBLE_STATE),
    })
}


fn compile_set(compiler: &mut Compiler, lvalue: &ASTNode, value: Reg) -> Option<()> {
    // Recursively set
    if let VarExpr(s) = lvalue.clone() {
        let nreg = compiler.push(Value::Str(s));
        compiler.add_op_args(Opcode::SV, nreg as u8, value as u8, 0);
        compiler.free_reg(nreg);
        compiler.free_reg(value);
        return Some(());
    } else if let IndexExpr(list, index) = lvalue.clone() {
        let lreg = compile_expr(compiler, &list)?;
        let ireg = compile_expr(compiler, &index)?;
        compiler.add_op_args(Opcode::SKY, lreg as u8, ireg as u8, value as u8);
        compiler.free_reg(lreg);
        compiler.free_reg(ireg);
        // Indexes are attached to something, make sure it reattaches
        return compile_set(compiler, &list, value);
    }
    panic!("Cannot compile_set for something other then a variable or index");
}

fn compile_short_binop(
    compiler: &mut Compiler,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>,
    clean: bool
) -> Option<Reg> {
    // Compiles short circuiting operators (&& and ||)
    // Uses jump instructions to:
    // Turn `a() && b()` into `r = a(); if r  { r = b() }; r`
    // Turn `a() || b()` into `r = a(); if !r { r = b() }; r`
    let lhs = compile_expr(compiler, lhs)?;
    let dup_tmp = compiler.alloc_reg();
    compiler.add_op_args(Opcode::NOT, lhs as u8, dup_tmp as u8, 0);
    if op != &TokenType::Or {
        // Double not is faster then a copy
        compiler.add_op_args(Opcode::NOT, dup_tmp as u8, dup_tmp as u8, 0);
    }
    // Start jump
    compiler.add_op(Opcode::JMPNT);
    let pos = compiler.program.ops.len();
    compiler.free_reg(dup_tmp);
    // It's 'b' (the left)
    let rhs = compile_expr(compiler, rhs)?;
    compiler.copy(rhs, lhs);
    compiler.free_reg(rhs);
    // End the jump
    compiler.fill_jmp(pos, 0, Some(dup_tmp));
    if clean {
        if lhs == Reg::Stack {
            compiler.add_op(Opcode::POP);
        }
        compiler.free_reg(lhs);
    }
    return Some(lhs);
}

fn compile_binop(
    compiler: &mut Compiler,
    lhs: &Box<ASTNode>, op: &TokenType, rhs: &Box<ASTNode>,
    clean: bool
) -> Option<Reg> {
    // Short circuiting ops are special
    if op == &TokenType::And || op == &TokenType::Or {
        return compile_short_binop(compiler, lhs, op, rhs, clean);
    }
    // Compile sides
    let lreg = if op != &TokenType::Equals {
        // No need to compile the value if it will just be reassigned
        compile_expr(compiler, lhs)? as u8
    } else {
        // Unused reg so things will break if someone uses it
        47
    };
    let rreg = compile_expr(compiler, rhs)? as u8;
    // Compile op
    match op {
        // Simple single instructions
        TokenType::Plus | TokenType::PlusEquals => {
            compiler.add_op_args(Opcode::ADD, lreg, rreg, rreg);
        },
        TokenType::Minus | TokenType::MinusEquals => {
            compiler.add_op_args(Opcode::SUB, lreg, rreg, rreg);
        },
        TokenType::Times | TokenType::TimesEquals => {
            compiler.add_op_args(Opcode::MUL, lreg, rreg, rreg);
        },
        TokenType::Div | TokenType::DivEquals => {
            compiler.add_op_args(Opcode::DIV, lreg, rreg, rreg);
        },
        TokenType::Modulo | TokenType::ModEquals => {
            compiler.add_op_args(Opcode::MOD, lreg, rreg, rreg);
        },
        TokenType::And => {
            compiler.add_op_args(Opcode::AND, lreg, rreg, rreg);
        },
        TokenType::Or => {
            compiler.add_op_args(Opcode::OR, lreg, rreg, rreg);
        },
        TokenType::Xor => {
            compiler.add_op_args(Opcode::XOR, lreg, rreg, rreg);
        },
        TokenType::Gt => {
            compiler.add_op_args(Opcode::GT, lreg, rreg, rreg);
        },
        TokenType::Lt => {
            compiler.add_op_args(Opcode::LT, lreg, rreg, rreg);
        },
        TokenType::EqualsEquals => {
            compiler.add_op_args(Opcode::EQ, lreg, rreg, rreg);
        },
        TokenType::In => {
            compiler.add_op_args(Opcode::IN, lreg, rreg, rreg);
        },
        // Harder ones that don't have a single instruction
        TokenType::NotEquals => {
            compiler.add_op_args(Opcode::EQ, lreg, rreg, rreg);
            compiler.add_op_args(Opcode::NOT, lreg, rreg, rreg);
        },
        TokenType::LtEquals => {
            compiler.add_op_args(Opcode::LT, lreg, rreg, rreg);
            compiler.add_op_args(Opcode::NOT, lreg, rreg, rreg);
        },
        TokenType::GtEquals => {
            compiler.add_op_args(Opcode::GT, lreg, rreg, rreg);
            compiler.add_op_args(Opcode::NOT, lreg, rreg, rreg);
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
        compile_set(compiler, lhs, to_reg(rreg))?;
        return Some(Reg::Stack);
    } else if clean {
        // Clean up the stack
        if lreg == Reg::Stack as u8 {
            compiler.add_op(Opcode::POP);
        }
        compiler.free_reg(to_reg(rreg));
        compiler.free_reg(to_reg(lreg));
        return Some(Reg::Stack);
    }
    compiler.free_reg(to_reg(lreg));
    return Some(to_reg(rreg));
}

fn compile_expr(compiler: &mut Compiler, node: &ASTNode) -> Option<Reg> {
    Some(match node {
        // Values
        VarExpr(val) => {
            let reg = compiler.push(Value::Str(val.clone()));
            // Load var inplace
            compiler.add_op_args(Opcode::LV, reg as u8, reg as u8, 0);
            reg
        },
        StringExpr(val) => {
            compiler.push(Value::Str(val.clone()))
        },
        NumberExpr(val) => {
            compiler.push(Value::Int(*val))
        },
        DecimalExpr(val) => {
            compiler.push(Value::Float(*val))
        },
        BoolExpr(val) => {
            compiler.push(Value::Bool(*val))
        },
        NoneExpr => {
            compiler.push(Value::None)
        },
        ByteExpr(val) => {
            compiler.push(Value::Byte(*val))
        },
        // Binop/unary
        BinopExpr(lhs, op, rhs) => {
            return compile_binop(compiler, lhs, op, rhs, false);
        }
        UnaryExpr(op, val) => {
            return compile_unary(compiler, op, val);
        },
        // Calls
        CallExpr(expr, args) => {
            // Push the args (in reverse order)
            for arg in args {
                // TODO: Find a way to force it to compile on the stack so it doesn't copy
                let a = compile_expr(compiler, arg)?;
                compiler.copy(a, Reg::Stack);
                compiler.free_reg(a);
            }
            // Get address
            let (address, name) = if let ASTNode::VarExpr(ref n) = **expr {
                // Lookup function address
                if let Some(addr) = compiler.program.functis.iter().find_map(
                    |i| if &i.0 == n && i.2 == args.len() as i32 { Some(i.1) } else { None }
                ) {
                    (addr, "".to_string())
                } else if args.len() == 0 && n == "args" {
                    // Saving args is needed
                    compiler.needs_args = true;
                    let ret = compiler.alloc_reg();
                    compiler.add_op_args(Opcode::CARG, ret as u8, 0, 0);
                    return Some(ret);
                } else {
                    (0, n.clone())
                }
            } else {
                (0, "".to_string())
            };
            // Compile the call
            if address == 0 {
                // Variable call (VCALL)
                let expr = if name == "" || name.contains("::"){
                    compile_expr(compiler, expr)?
                } else {
                    compiler.push(Value::Functi(name.clone()))
                };
                compiler.add_op_args(Opcode::VCALL, expr as u8, args.len() as u8, 0);
                compiler.free_reg(expr);
            } else {
                compiler.add_op_args(
                    Opcode::CALL,
                    ((address >> 16) & 255) as u8,
                    ((address >> 8) & 255) as u8,
                    (address & 255) as u8
               );
            }
            return Some(Reg::Stack);
        },
        // List
        /*ListExpr(keys, values, fast) => {
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
        },*/
        _ => {
            panic!("Unknown token! {:?}", node);
        }
    })
}
/*
fn _compile_body(
    compiler: &mut Compiler, args: &mut Arguments,
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
    compiler: &mut Compiler, args: &mut Arguments, node: &ASTNode, manual_scope: bool
) -> bool {
    let BodyStmt(nodes) = node else {
        if *node == Nop {
            return true;
        }
        panic!("compile_body got non-body node!");
    };
    return _compile_body(program, args, nodes, manual_scope);
}*/

fn compile_stmt(
    compiler: &mut Compiler, args: &mut Arguments, node: &ASTNode, dirty: bool
) -> Option<()> {
    match node {
        // Statements
        LetStmt(name, val) => {
            let vreg = compile_expr(compiler, val)?;
            let nreg = compiler.push(Value::Str(name.to_string()));
            compiler.add_op_args(Opcode::DV, nreg as u8, vreg as u8, 0);
            compiler.free_reg(vreg);
            compiler.free_reg(nreg);
            compiler.needs_scope = true;
        },
        /*IfStmt(cond, body, else_part) => {
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
            program.ops.push(Opcode::DOS as u8);

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
        },*/

        Nop => {
            // Nop isn't turned into the NOP instruction because it's useless
        },
        // Expressions
        // Binops don't always return, so let them manage cleaning the stack
        BinopExpr(lhs, op, rhs) => {
            let reg = compile_binop(compiler, lhs, op, rhs, !dirty)?;
            if dirty && reg != Reg::Stack {
                // Copy to stack
                compiler.copy(reg, Reg::Stack);
                compiler.free_reg(reg);
            }
        },
        _ => {
            let reg = compile_expr(compiler, node)?;
            if !dirty {
                // Remove unused values from the stack
                if reg == Reg::Stack {
                    compiler.add_op(Opcode::POP);
                }
            } else if reg != Reg::Stack {
                // Move the result to the stack
                compiler.copy(reg, Reg::Stack);
            }
            compiler.free_reg(reg);
        }
    };
    return Some(());
}

pub fn compile(
    ast: Vec<ASTNode>, args: &mut Arguments, compiler: &mut Compiler
) -> bool {
    if ast.is_empty() {
        return true;
    }
    compiler.inc_start = compiler.program.ops.len() as u32;
    // Compile
    for node in &ast[..ast.len()-1] {
        if !compile_stmt(compiler, args, node, false).is_some() {
            return false;
        }
    }
    // If repl, compile the last value without cleaning up
    // Else just compile normally
    if !compile_stmt(compiler, args, ast.last().unwrap(), args.is_repl).is_some() {
        return false;
    }
    // Jumps go onto the next instruction, so a nop is needed at the end
    compiler.add_op(Opcode::NOP);
    // End file
    compiler.program.file_table.push((
        compiler.inc_start, compiler.program.ops.len() as u32, args.name.clone()
    ));
    return true;
}
