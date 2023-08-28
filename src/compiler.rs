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
    // If RS and LEVI are needed
    needs_scope: bool,
    // If VARG and CARG are needed
    needs_args: bool,
    // Where in the byte code the current file started
    inc_start: u32,
    // Registers
    regs: [bool; 17],
    // Break addresses (so the jump can be filled)
    break_addrs: Vec<usize>,
    // Loop top (so continue can be filled)
    loop_top: usize,
    // Limits registers to just the stack
    on_stack_only: bool,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            program: Program::new(),
            needs_scope: false, inc_start: 0,
            regs: [true; 17], needs_args: false,
            break_addrs: vec![], loop_top: 0,
            on_stack_only: true,
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

    fn move_(&mut self, src: Reg, dst: Reg) {
        if src == dst {
            return;
        }
        self.copy(src, dst);
        if src == Reg::Stack {
            self.add_op(Opcode::POP);
        }
    }

    #[inline]
    fn copy(&mut self, src: Reg, dst: Reg) {
        self.add_op_args(Opcode::CP, src as u8, dst as u8, 0);
    }

    #[inline]
    fn dup(&mut self) {
        self.add_op_args(Opcode::CP, Reg::Stack as u8, Reg::Stack as u8, 0);
    }

    // Register allocation
    fn alloc_reg(&mut self) -> Reg {
        if self.on_stack_only {
            // Only stack allowed
            return Reg::Stack;
        }
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
            //self.add_op(Opcode::POP);
            return;
        }
        self.regs[reg as usize] = true;
    }

    fn push_to_stack(&mut self, val: Value) {
        // Get the index, or append
        let index = self.program.consts.iter().position(|i| i.clone() == val)
            .unwrap_or_else(|| {
            self.program.consts.push(val);
            self.program.consts.len() - 1
        });
        // Push the instruction
        if index > 2usize.pow(24)-1 {
            panic!("Too many different constants! You have over 16777215 constants!!");
        }
        self.add_op_args(
            Opcode::LDL,
            ((index >> 16) & 255) as u8,
            ((index >> 8) & 255) as u8,
            (index & 255) as u8
        );
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
            let tmp = compiler.push(Value::Int(0));
            let ret = compile_expr(compiler, val)?;
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
            if ret == Reg::Stack {
                compiler.dup();
            }
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
            if ret == Reg::Stack {
                compiler.dup();
            }
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
        let ireg = compile_expr(compiler, &index)?;
        let lreg = compile_expr(compiler, &list)?;
        compiler.add_op_args(Opcode::SKY, lreg as u8, ireg as u8, value as u8);
        compiler.free_reg(value);
        compiler.free_reg(ireg);
        // Indexes are attached to something, make sure it reattaches
        return compile_set(compiler, &list, lreg);
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
    if dup_tmp == Reg::Stack && lhs == Reg::Stack {
        compiler.dup();
        compiler.add_op_args(Opcode::NOT, dup_tmp as u8, dup_tmp as u8, 0);
    } else {
        compiler.add_op_args(Opcode::NOT, lhs as u8, dup_tmp as u8, 0);
    }
    if op != &TokenType::Or {
        // Double not is faster then a copy
        compiler.add_op_args(Opcode::NOT, dup_tmp as u8, dup_tmp as u8, 0);
    }
    // Start jump
    compiler.add_op(Opcode::JMPNT);
    let pos = compiler.program.ops.len();
    compiler.free_reg(dup_tmp);
    // It's 'b' (the left)
    if lhs == Reg::Stack {
        compiler.add_op(Opcode::POP);
    }
    let rhs = compile_expr(compiler, rhs)?;
    compiler.move_(rhs, lhs);
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
            compiler.add_op_args(Opcode::NOT, rreg, rreg, 0);
        },
        TokenType::LtEquals => {
            compiler.add_op_args(Opcode::LT, lreg, rreg, rreg);
            compiler.add_op_args(Opcode::NOT, rreg, rreg, 0);
        },
        TokenType::GtEquals => {
            compiler.add_op_args(Opcode::GT, lreg, rreg, rreg);
            compiler.add_op_args(Opcode::NOT, rreg, rreg, 0);
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
            // Push the args onto the stack (in reverse order)
            let old_on_stack = compiler.on_stack_only;
            compiler.on_stack_only = true;
            for arg in args.iter().rev() {
                compile_expr(compiler, arg)?;
            }
            compiler.on_stack_only = old_on_stack;
            // Get address
            let (address, name) = if let ASTNode::VarExpr(ref n) = **expr {
                // Lookup function address
                let n = n.clone().split("::").nth(1).unwrap().to_string();
                if let Some(addr) = compiler.program.functis.iter().find_map(
                    |i| if i.0 == n && i.2 == args.len() as i32 { Some(i.1) } else { None }
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
            Reg::Stack
        },
        // List
        ListExpr(keys, values, fast) => {
            // Build the list
            let old_on_stack = compiler.on_stack_only;
            compiler.on_stack_only = true;
            for at in (0..values.len()).rev() {
                compile_expr(compiler, &values[at])?;
                if !*fast {
                    compiler.push_to_stack(Value::Str(keys[at].clone()));
                }
            }
            compiler.on_stack_only = old_on_stack;
            // Push
            let len = values.len();
            let reg = compiler.alloc_reg();
            if *fast {
                compiler.add_op_args(
                    Opcode::LFL,
                    reg as u8,
                    ((len >> 8) & 255) as u8,
                    (len & 255) as u8
                );
            } else {
                compiler.add_op_args(
                    Opcode::LL,
                    reg as u8,
                    ((len >> 8) & 255) as u8,
                    (len & 255) as u8
                );
            }
            reg
        },
        // Indexes
        IndexExpr(val, index) => {
            // Push
            let index = compile_expr(compiler, index)?;
            let expr = compile_expr(compiler, val)?;
            compiler.add_op_args(Opcode::INX, expr as u8, index as u8, expr as u8);
            compiler.free_reg(index);
            expr
        },
        _ => {
            panic!("Unknown token! {:?}", node);
        }
    })
}

fn _compile_body(
    compiler: &mut Compiler, args: &mut Arguments,
    nodes: &Vec<ASTNode>, manual_scope: bool
) -> Option<()> {
    // Lower scope
    let scope_pos = compiler.program.ops.len();
    let old_needs_scope = compiler.needs_scope;
    compiler.needs_scope = false;
    if !manual_scope {
        compiler.add_op(Opcode::NOP);
    }
    // Compile all nodes
    for node in nodes {
        compile_stmt(compiler, args, node, false)?;
    }
    // Raise scope
    if !manual_scope && compiler.needs_scope {
        compiler.add_op(Opcode::RS);
        compiler.program.ops[scope_pos] = Opcode::LEVI as u8 as u32;
    }
    compiler.needs_scope = old_needs_scope;
    return Some(());
}
fn compile_body(
    compiler: &mut Compiler, args: &mut Arguments, node: &ASTNode, manual_scope: bool
) -> Option<()> {
    let BodyStmt(nodes) = node else {
        if *node == Nop {
            return Some(());
        }
        panic!("compile_body got non-body node!");
    };
    _compile_body(compiler, args, nodes, manual_scope)
}

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
        IfStmt(cond, body, else_part) => {
            // The condition must be a expr, so no need to match against stmts
            let cond = compile_expr(compiler, cond)?;

            // This is for when boolean not is forgotten
            if **body == Nop {
                compiler.add_op_args(Opcode::NOT, cond as u8, cond as u8, 0);
                // Push the jump offset (which will be filled later)
                compiler.add_op(Opcode::JMPNT);
                let pos = compiler.program.ops.len();
                // Compile body
                compile_stmt(compiler, args, else_part, false)?;
                compiler.fill_jmp(pos, 0, Some(cond));
                compiler.free_reg(cond);
                return Some(());
            }

            // Push the jump offset (which will be filled later)
            compiler.add_op(Opcode::JMPNT);
            let pos = compiler.program.ops.len();
            // Compile true part
            compile_body(compiler, args, body, false)?;

            // The else
            if **else_part != Nop {
                // Prep exit offset
                compiler.add_op(Opcode::JMP);
                let exit_pos = compiler.program.ops.len();
                // Fill else jump
                compiler.fill_jmp(pos, 0, Some(cond));
                // Compile else part
                compile_stmt(compiler, args, else_part, false)?;
                compiler.fill_jmp(exit_pos, 0, None);
            } else {
                // No else
                compiler.fill_jmp(pos, 0, Some(cond));
            }
            compiler.free_reg(cond);
        },
        LoopStmt(var, iter, body) => {
            // Load iter
            let iter = compile_expr(compiler, iter)?;
            compiler.add_op_args(Opcode::ITER, iter as u8, iter as u8, 0);
            let item = compiler.alloc_reg();
            let old_top = compiler.loop_top;
            let last_size = compiler.break_addrs.len();
            compiler.loop_top = compiler.program.ops.len();
            compiler.add_op_args(Opcode::NXT, iter as u8, item as u8, 2);

            // Exit jump
            compiler.add_op(Opcode::JMP);
            let jmp_pos = compiler.program.ops.len();

            // Lower scope
            compiler.add_op(Opcode::LEVI);
            // Set the loop var
            let varname = compiler.push(Value::Str(var.to_string()));
            compiler.add_op_args(Opcode::DOS, varname as u8, item as u8, 0);
            compiler.free_reg(varname);

            // Body
            compile_body(compiler, args, body, true)?;
            // Raise scope
            compiler.add_op(Opcode::RS);
            // Backwards jump
            compiler.add_op(Opcode::JMPB);
            compiler.fill_jmp(
                compiler.program.ops.len(),
                compiler.program.ops.len() - compiler.loop_top - 1,
                None
            );
            // Fill breaks
            for addr in &compiler.break_addrs.clone()[last_size..] {
                compiler.fill_jmp(*addr, 0, None);
            }
            compiler.break_addrs.truncate(last_size);
            compiler.loop_top = old_top;
            // Clean up the iter
            compiler.fill_jmp(jmp_pos, 0, None);
            compiler.free_reg(iter);
            if iter == Reg::Stack {
                compiler.add_op(Opcode::POP);
            }
            compiler.free_reg(item);
        },
        WhileStmt(cond, body) => {
            // Start (so it can loop back)
            let old_top = compiler.loop_top;
            compiler.loop_top = compiler.program.ops.len();
            let last_size = compiler.break_addrs.len();
            // Condition
            let cond = compile_expr(compiler, cond)?;
            // Exit jump
            compiler.add_op(Opcode::JMPNT);
            let exit_jump_pos = compiler.program.ops.len();

            // Compile body
            compile_body(compiler, args, body, false)?;

            // Backwards jump
            compiler.add_op(Opcode::JMPB);
            compiler.fill_jmp(
                compiler.program.ops.len(),
                compiler.program.ops.len() - compiler.loop_top - 1,
                None
            );
            // Fill breaks
            for addr in &compiler.break_addrs.clone()[last_size..] {
                compiler.fill_jmp(*addr, 0, None);
            }
            // Exit jump
            compiler.fill_jmp(exit_jump_pos, 0, Some(cond));
            compiler.loop_top = old_top;
            compiler.free_reg(cond);
        },
        BreakStmt => {
            // Filled later
            compiler.add_op(Opcode::JMP);
            compiler.break_addrs.push(compiler.program.ops.len());
        },
        ContinueStmt => {
            compiler.add_op(Opcode::JMPB);
            compiler.fill_jmp(compiler.program.ops.len(), compiler.program.ops.len() - compiler.loop_top - 1, None);
        },
        BodyStmt(nodes) => return _compile_body(compiler, args, nodes, false),
        FunctiStmt(node) => {
            // Jump around function
            compiler.add_op(Opcode::JMP);
            let pos = compiler.program.ops.len();
            // Declare function
            compiler.program.functis.push((
                node.name.clone(),
                compiler.program.ops.len(),
                node.arg_names.len() as i32
            ));
            // Arg saving
            let start = compiler.program.ops.len();
            compiler.add_op(Opcode::NOP);
            // Load args from stack
            for arg in &node.arg_names {
                let name = compiler.push(Value::Str(arg.to_string()));
                compiler.add_op_args(Opcode::DV, name as u8, Reg::Stack as u8, 0);
                compiler.free_reg(name);
            }
            // Compile body
            compile_body(compiler, args, &*node.body, true)?;
            // Return
            compiler.push_to_stack(Value::None);
            compiler.add_op(Opcode::RET);
            // Save args
            if compiler.needs_args {
                compiler.program.ops[start] = ((Opcode::SARG as u32) << 24)
                    + ((node.arg_names.len() as u32 & 255) << 16);
                compiler.needs_args = false;
            }
            // Fill jump
            compiler.fill_jmp(pos, 0, None);
        },
        ReturnStmt(ret) => {
            if let CallExpr(expr, args) = *ret.clone() {
                let functi = compiler.program.functis.last().unwrap().clone();
                let do_tco = if let ASTNode::VarExpr(name) = *expr {
                    name == functi.0 && args.len() == functi.1 as usize
                } else { false };
                // Tail call is possible!
                if do_tco {
                    // Push args
                    let old_on_stack = compiler.on_stack_only;
                    compiler.on_stack_only = true;
                    for ref arg in args.iter().rev() {
                        compile_expr(compiler, arg)?;
                    }
                    compiler.on_stack_only = old_on_stack;
                    // Jump
                    compiler.add_op(Opcode::RCALL);
                    compiler.fill_jmp(
                        compiler.program.ops.len(),
                        compiler.program.ops.len() - functi.2 as usize - 1,
                        None
                    );
                    return Some(());
                }
            }
            // Compile return value
            let old_on_stack = compiler.on_stack_only;
            compiler.on_stack_only = true;
            compile_expr(compiler, ret)?;
            compiler.on_stack_only = old_on_stack;
            // Return return value
            compiler.add_op(Opcode::RET);
        },
        ImportStmt() => {
            compiler.program.file_table.push((
                compiler.inc_start, compiler.program.ops.len() as u32, args.name.clone()
            ));
            compiler.inc_start = compiler.program.ops.len() as u32;
        },
        EndImportStmt(file) => {
            compiler.program.file_table.push((
                compiler.inc_start, compiler.program.ops.len() as u32, file.clone()
            ));
            compiler.inc_start = compiler.program.ops.len() as u32;
        },

        Nop => {
            // Nop isn't turned into the NOP instruction because it's useless
        },
        // Expressions
        // Binops don't always return, so let them manage cleaning the stack
        BinopExpr(lhs, op, rhs) => {
            let reg = compile_binop(compiler, lhs, op, rhs, !dirty)?;
            if dirty && reg != Reg::Stack {
                // Copy to stack
                compiler.move_(reg, Reg::Stack);
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
                compiler.move_(reg, Reg::Stack);
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
