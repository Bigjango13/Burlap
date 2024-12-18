// This is a bytecode VM
use std::cell::RefCell;
use std::rc::Rc;
use std::fs::OpenOptions;
use std::io::{Write, Read, Seek, SeekFrom};
use std::io;
#[cfg(feature = "cffi")]
use std::path::PathBuf;

use crate::Arguments;
use crate::common::IMPOSSIBLE_STATE;
use crate::backend::vm::compiler::Program;
use crate::backend::vm::dis::dis_single;
use crate::backend::value::{FileInfo, Value};
#[cfg(feature = "cffi")]
use crate::backend::vm::cffi::{load_functi, load_library};
#[cfg(feature = "cffi")]
use crate::backend::vm::cffi::call as ffi_call;

use rustc_hash::FxHashMap;
use rand::Rng;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum Opcode {
    // The almighty NOP
    NOP,

    // Regs
    // LoaD ([u16 pool index "src", register "dst"])
    LD,
    // LoaD Longer ([u24 pool index])
    LDL,
    // CoPy ([register "src", register "dst"])
    CP,
    // POP
    POP,

    // Functions
    // Save ARGs ([u8 "arg count"])
    SARG,
    // Copy ARGs ([register "dst"])
    CARG,
    // CALL ([u24 "address"])
    CALL,
    // Variable CALL function ([register "functi", u8 "arg #"])
    VCALL,
    // Returning CALL function ([u24 "address"])
    RCALL,
    // RETurn (ret)
    RET,

    // Variables
    // Load Variable Local/Global ([u16 "offset", register "dst"])
    LV_L,
    LV_G,
    // Set Variable Local/Global ([u16 "offset", register "value"])
    SV_L,
    SV_G,
    // Push LoCals ([u16 "amount", u8 "arg number"])
    PLC,
    // Push GloBals ([u16 "amount"])
    // Only used once in normal programs, needed more for interactive sessions (such as the REPL)
    PGB,
    // Add Locals Offset ([u16 "offset", reg "reftype"])
    ALO,

    // Lists
    // Load Fast List ([dst, u16 size, values on stack])
    LFL,
    // Load List ([dst, u16 size, values and keys on stack])
    LL,
    // INdeX ([register "list", register "index", register "dst"])
    INX,
    // into ITER ([register "value", register "dst"])
    ITER,
    // NeXT ([register "iter", register "dst", u8 "jump if not empty"])
    NXT,
    // Set KeY ([register "list", register "index", register "value"])
    SKY,

    // Math
    // ADD ([register "a", register "b", register "dst"])
    ADD,
    // SUBtract ([register "a", register "b", register "dst"])
    SUB,
    // MULtiply ([register "a", register "b", register "dst"])
    MUL,
    // DIVide ([register "a", register "b", register "dst"])
    DIV,
    // MODulo ([register "a", register "b", register "dst"])
    MOD,

    // Boolean
    // AND ([register "a", register "b", register "dst"])
    AND,
    // OR ([register "a", register "b", register "dst"])
    OR,
    // XOR ([register "a", register "b", register "dst"])
    XOR,
    // NOT ([register "a", register "dst"])
    NOT,

    // Comparison
    // EQuals ([register "a", register "b", register "dst"])
    EQ,
    // Greator Than ([register "a", register "b", register "dst"])
    GT,
    // Less Than ([register "a", register "b", register "dst"])
    LT,
    // IN ([register "a", register "b", register "dst"])
    IN,

    // Jumps
    // JuMP ([u24 "address"])
    JMP,
    // JuMP Backward ([u24 "address"])
    JMPB,
    // JuMP if NoT ([register "cond", u16 "offset"])
    JMPNT,
}


// A functie is a sack functions implemented in rust
type Functie = fn(&mut Vm, Vec<Value>) -> Result<Value, String>;

// Call frames
struct CallFrame {
    args: Option<Vec<Value>>,
    return_addr: usize,
    regs: [Value; 16],
    local_size: usize,
}

#[inline]
pub fn shift2(a: u8, b: u8) -> usize {
    return ((a as usize) << 8) + (b as usize);
}

#[inline]
pub fn shift3(a: u8, b: u8, c: u8) -> usize {
    return ((a as usize) << 16) + ((b as usize) << 8) + (c as usize);
}

// VM state
pub struct Vm {
    // Extensions
    pub args: Arguments,
    // File name
    pub filename: String,

    // State
    pub has_err: bool,
    pub in_func: bool,

    // Variables
    globals: Vec<Value>,
    locals: Vec<Value>,
    // Functies
    functies: FxHashMap<String, Functie>,

    // Call frames
    call_frames: Vec<CallFrame>,

    // The program
    pub program: Program,
    // Sacks stack and regs
    stack: Vec<Value>,
    regs: [Value; 16],
    // Misc
    pub jump: bool,
    pub at: usize,
}

impl Vm {
    // Init
    pub fn new(args: Arguments, program: Program) -> Vm {
        // Builtin functions
        let mut functies = FxHashMap::with_capacity_and_hasher(
            16, Default::default()
        );
        // Builtins
        functies.insert("print".to_string(), sk_print as Functie);
        functies.insert("input".to_string(), sk_input as Functie);
        functies.insert("type".to_string(), sk_type as Functie);
        functies.insert("len".to_string(), sk_len as Functie);
        functies.insert("count".to_string(), sk_count as Functie);
        functies.insert("range".to_string(), sk_range as Functie);
        functies.insert("rand".to_string(), sk_rand as Functie);
        // File IO
        #[cfg(not(target_family = "wasm"))]
        {
            functies.insert("open".to_string(), sk_open as Functie);
            functies.insert("close".to_string(), sk_close as Functie);
            functies.insert("read".to_string(), sk_read as Functie);
            functies.insert("write".to_string(), sk_write as Functie);
            functies.insert("seek".to_string(), sk_seek as Functie);
            functies.insert("flush".to_string(), sk_flush as Functie);
        }
        // Casts
        functies.insert("int".to_string(), sk_int as Functie);
        functies.insert("float".to_string(), sk_float as Functie);
        functies.insert("string".to_string(), sk_string as Functie);
        functies.insert("byte".to_string(), sk_byte as Functie);
        // Non-togglable internals
        functies.insert("__burlap_range".to_string(), sk_fastrange as Functie);
        // Burlap internal functies
        if args.extension_functies {
            functies.insert(
                "__burlap_typed_eq".to_string(), sk_typed_eq as Functie
            );
            functies.insert(
                "__burlap_print".to_string(), sk_real_print as Functie
            );
            functies.insert(
                "__burlap_throw".to_string(), sk_throw as Functie
            );
            functies.insert(
                "__burlap_set_var".to_string(), sk_set_var as Functie
            );
            functies.insert(
                "__burlap_load_var".to_string(), sk_load_var as Functie
            );
            #[cfg(feature = "cffi")]
            functies.insert(
                "__burlap_load_lib".to_string(), sk_libload as Functie
            );
            #[cfg(feature = "cffi")]
            functies.insert(
                "__burlap_load_functi".to_string(), sk_functiload as Functie
            );
            #[cfg(feature = "cffi")]
            functies.insert(
                "__burlap_ffi_call".to_string(), sk_call_c as Functie
            );
            #[cfg(feature = "cffi")]
            functies.insert(
                "__burlap_ptr".to_string(), sk_ptr as Functie
            );
        }
        if args.extension_debugging_functies {
            functies.insert(
                "__burlap_debug_on".to_string(), sk_debug_on as Functie
            );
            functies.insert(
                "__burlap_debug_off".to_string(), sk_debug_off as Functie
            );
        }
        const NONE: Value = Value::None;
        Vm {
            stack: vec![], call_frames: vec![], jump: false,
            at: 0, filename: "".to_string(), locals: vec![],
            args, has_err: false, in_func: false, functies,
            globals: vec![], regs: [NONE; 16], program,
        }
    }

    // Unmangle a var name
    #[inline]
    #[allow(dead_code)]
    fn unmangle(name: &str) -> String {
        name.split("::").last().unwrap().to_string()
    }

    // Get a vec of all symbol names
    #[cfg(feature = "fancyrepl")]
    pub fn get_symbols(&self, add_keywords: bool) -> Vec<String> {
        let mut ret: Vec<String> = vec![];
        /*
        // Globals
        ret.extend(self.globals.iter().map(|x| Self::unmangle(x))
            .collect::<Vec<String>>());
        // Locals
        ret.extend(self.var_names.iter().map(|x| Self::unmangle(x))
            .collect::<Vec<String>>());
        */
        // Functions
        ret.extend(
            self.program.functis.iter().map(|i| i.0.clone())
            .collect::<Vec<String>>()
        );
        // Functies
        ret.extend(self.functies.keys().cloned().collect::<Vec<String>>());
        // Keywords
        if add_keywords {
            ret.extend(vec![
                "true", "false", "none", "functi", "let", "return", "in",
                "if", "else", "loop", "while", "import",
            ].iter().map(|i| i.to_string()).collect::<Vec<String>>());
        }
        return ret;
    }

    // Getting/setting vars
    fn get_var_offset(&mut self, offset: u16, global: bool) -> (&mut Vec<Value>, usize) {
        let vec = if global { &mut self.globals } else { &mut self.locals };
        let off: i32 = if !global { vec.len() as i32 - (offset as i32 + 1) } else { offset as i32 };
        return (vec, off as usize);
    }
    
    pub fn get_var(&mut self, offset: u16, global: bool) -> Value {
        let (vec, off) = self.get_var_offset(offset, global);
        return vec[off as usize].clone();
    }

    pub fn set_var(&mut self, offset: u16, val: Value, global: bool) {
        let (vec, off) = self.get_var_offset(offset, global);
        vec[off as usize] = val;
    }

    // Functies
    fn bad_args(
        &self, name: &str, got: usize, need: usize
    ) -> Result<(), String> {
        Err(if got > need {
            format!("too many args for {} (got {} need {})", name, got, need)
        } else {
            format!("too few args for {} (got {} need {})", name, got, need)
        })
    }

    // Pops locals from the stack
    fn pop_locals(&mut self, local_size: usize) {
        self.locals.truncate(local_size);
    }

    // Calling
    pub fn call_name(
        &mut self, name: String, mut arg_num: u8
    ) -> Result<(), String> {
        // Check builtins
        if let Some(functie) = self.functies.get(&name) {
            // Get args
            let mut args: Vec<Value> = vec![];
            while arg_num > 0 {
                args.push(self.stack.pop().unwrap());
                arg_num -= 1;
            }
            args = args.into_iter().rev().collect();
            // Call
            let ret = functie(self, args)?;
            self.stack.push(ret);
            return Ok(());
        };
        // Check non-builtins
        let mut exists: bool = false;
        for (fname, addr, argn) in &self.program.functis {
            // Same name
            if &name == fname {
                exists = true;
                // Correct args
                if argn == &(arg_num as i32) {
                    // Go to it!
                    self.call(*addr);
                    return Ok(());
                }
            }
        }
        // Die
        if exists {
            return Err(format!("incorrect number of arguments for \"{name}\""));
        }
        return Err(format!("no function called \"{name}\", this should never happen"));
    }

    // Call a function
    pub fn call(&mut self, addr: usize) {
        // Store reteurn address and registers
        const NONE: Value = Value::None;
        let mut regs = [NONE; 16];
        std::mem::swap(&mut self.regs, &mut regs);
        self.call_frames.push(
            CallFrame {
                args: None,
                return_addr: self.at,
                local_size: self.locals.len(),
                regs
            }
        );
        // Jump there
        self.at = addr;
        self.jump = true;
    }

    /*pub fn cur_op(&mut self) -> u8 {
        ((self.program.ops[self.at] & 0xFF000000) >> 24).try_into().unwrap()
    }*/

    #[inline]
    pub fn cur_opcode(&mut self) -> (Opcode, u8, u8, u8) {
        let op = self.program.ops[self.at];
        return (
            // Unsafe because casting an int to enum might not be valid
            // 99% of the time it will be, the 1% is when someone is monkeying around with burlap
            unsafe {
                std::mem::transmute::<u8, Opcode>(
                    ((op & 0xFF000000) >> 24).try_into().unwrap()
                )
            },
            ((op & 0xFF0000) >> 16).try_into().unwrap(),
            ((op & 0xFF00) >> 8).try_into().unwrap(),
            (op & 0xFF).try_into().unwrap(),
        );
    }

    // Registers
    #[inline]
    pub fn set_reg(&mut self, reg: u8, value: Value) {
        if reg < 16 {
            // [0..15] are normal
            self.regs[reg as usize] = value;
        } else if reg == 16 {
            // 16 is the stack stack
            self.stack.push(value);
        } else {
            panic!("Attempt to mutate a non-mutable register!");
        }
    }

    #[inline]
    pub fn get_reg_mut(&mut self, reg: u8) -> &mut Value {
        if reg < 16 {
            // [0..15] are normal
            &mut self.regs[reg as usize]
        } else if reg == 16 {
            // 16 is the stack stack
            self.stack.last_mut().expect("Overpopped stack!")
        } else {
            panic!("Attempt to mutate a non-mutable register!");
        }
    }

    #[inline]
    pub fn get_reg_ref(&self, reg: u8) -> &Value {
        if reg < 16 {
            // [0..15] are normal
            &self.regs[reg as usize]
        } else if reg == 16 {
            // 16 is the stack stack
            self.stack.last().expect("Overpopped stack!")
        } else if 17 <= reg && reg <= 115 {
            // [17..117] is consts
            &self.program.consts[reg as usize - 17]
        } else if 116 <= reg && reg <= 186 {
            // [116..186] is locals
            &self.locals[reg as usize - 116]
        } else {
            // [187..255] is globals
            &self.globals[reg as usize - 187]
        }
    }

    #[inline]
    pub fn get_reg(&mut self, reg: u8) -> Value {
        if reg < 16 {
            // [0..15] are normal
            self.regs[reg as usize].clone()
        } else if reg == 16 {
            // 16 is the stack stack
            self.stack.pop().expect("Overpopped stack!")
        } else if 17 <= reg && reg <= 115 {
            // [17..117] is consts
            self.program.consts[reg as usize - 17].clone()
        } else if 116 <= reg && reg <= 186 {
            // [116..186] is locals
            self.locals[reg as usize - 116].clone()
        } else {
            // [187..255] is globals
            self.globals[reg as usize - 187].clone()
        }
    }

    pub fn jump(&mut self, offset: i32) {
        if offset < 0 && -offset as usize > self.at{
            panic!("Negative jump out of bounds!");
        } else if offset < 0 {
            // Backward jump
            self.at -= -offset as usize;
        } else {
            // Forward jump
            self.at += offset as usize;
            if self.at >= self.program.ops.len() {
                panic!("Positive jump out of bounds!");
            }
        }
        self.jump = true;
    }
}

// Builtin Functions (prefixed with 'sk_')
// Print
fn sk_print(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if vm.args.extension_va_print {
        // VA print extension
        for i in args {
            print!("{} ", i.to_string()?);
        }
        println!();
    } else if args.len() != 1 {
        // Invalid args
        vm.bad_args("print", args.len(), 1)?;
    } else {
        // Normal printing
        println!("{}", args[0].to_string()?);
    }
    return Ok(Value::None);
}

// Input
fn sk_input(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("input", args.len(), 1)?;
    }
    // Print the prompt
    print!("{}", args[0].to_string()?);
    let _ = std::io::stdout().flush();
    // Get input
    let mut buffer = String::new();
    return Ok(match io::stdin().read_line(&mut buffer) {
        Err(_) => Value::Str(Rc::new("".to_string())),
        _ => Value::Str(Rc::new(buffer.trim_end().to_string()))
    });
}

// Type
fn sk_type(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("type", args.len(), 1)?;
    }
    return Ok(Value::Str(Rc::new(args[0].get_type())));
}

fn len_helper(iterable: &Value) -> Option<i32> {
    // Get the len
    Some(if let Value::FastList(l) = iterable {
        l.len()
    } else if let Value::List(l) = iterable {
        l.len()
    } else if let Value::Str(s) = iterable {
        s.chars().count()
    } else {
        return None;
    } as i32)
}

// Count
fn sk_count(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() == 1 {
        let Some(size) = len_helper(&args[0]) else {
            return Err("count() argument 1 must be a list or string".to_string());
        };
        return Ok(Value::Int(size));
    } else if args.len() == 2 {
        Ok(Value::Int(if let Value::FastList(l) = &args[0] {
            l.iter().filter(|v| args[1].eq(v)).count()
        } else if let Value::List(l) = &args[0] {
            l.iter().filter(|(_, v)| args[1].eq(v)).count()
        } else if let Value::Str(s) = &args[0] {
            let Value::Str(s2) = &args[1] else {
                return Ok(Value::Int(0));
            };
            s.as_str().matches(s2.as_str()).count()
        } else {
            return Err("count() argument 1 must be a list".to_string());
        } as i32))   
    } else {
        // Invalid args
        vm.bad_args("count", args.len(), 2)?;
        panic!("{}", IMPOSSIBLE_STATE)
    }
}

// Len
fn sk_len(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("len", args.len(), 1)?;
    }
    let Some(real_len) = len_helper(&args[0]) else {
        return Err("len() argument 1 must be a list or string".to_string());
    };
    if real_len == 0 {
        return Ok(Value::None);
    }
    return Ok(Value::Int(real_len - 1));
}

// Range
fn sk_range(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("range", args.len(), 2)?;
    }
    // Get the values
    let (mut min, max) = (args[0].to_int(), args[1].to_int());
    // Find out which way it's going
    let offset: i32 = if min > max { -1 } else { 1 };
    // Loop and get values
    let mut ret = Vec::<Value>::new();
    while min != max {
        ret.push(Value::Int(min));
        min += offset;
    }
    ret.push(Value::Int(min));
    return Ok(Value::FastList(Rc::new(ret)));
}

// Rand
fn sk_rand(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("rand", args.len(), 2)?;
    }
    let (mut min, mut max) = (args[0].to_int(), args[1].to_int());
    if min > max {
        std::mem::swap(&mut min, &mut max);
    }
    return Ok(Value::Int(rand::thread_rng().gen_range(min..max+1)));
}

// File IO
fn sk_open(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("open", args.len(), 2)?;
    }
    let Value::Str(ref file) = args[0] else {
        return Err("invalid file name".to_string());
    };
    let file = file.to_string();
    let Value::Str(ref mode) = args[1] else {
        return Err("invalid file mode".to_string());
    };
    // Open with mode
    let (infofile, mode) = match mode.as_str() {
        // Write
        "w" | "wb" => {(
            OpenOptions::new().write(true).create(true).truncate(false)
                .open(file.clone()),
            if **mode == "w" {2} else {-2}
        )},
        // Read
        "r" | "rb" => {(
            OpenOptions::new().read(true).open(file.clone()),
            if **mode == "r" {1} else {-1}
        )},
        // Append
        "a" => {(
            OpenOptions::new().append(true).create(true).open(file.clone()),
            0
        )},
        _ => return Err("invalid file mode".to_string()),
    };
    // Check
    let Ok(fileinfo) = infofile else {
        return Err("failed to open file".to_string());
    };
    // File info
    let fi = Rc::new(RefCell::new(FileInfo{
        name: file, mode,
        closed: false,
        file: Some(fileinfo)
    }));
    return Ok(Value::File(fi));
}

fn sk_close(vm: &mut Vm, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("close", args.len(), 1)?;
    }
    let Value::File(ref mut info) = args[0] else {
        return Err(format!("cannot close {}", args[0].get_type()));
    };
    info.borrow_mut().closed = true;
    info.borrow_mut().file = None;
    Ok(Value::None)
}

fn sk_flush(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("flush", args.len(), 1)?;
    }
    // Burlap doesn't buffer file io
    Ok(Value::None)
}

fn sk_read(vm: &mut Vm, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("read", args.len(), 1)?;
    }
    let Value::File(ref mut info) = args[0] else {
        return Err(format!("cannot read from {}", args[0].get_type()));
    };
    if info.borrow().closed {
        return Err("cannot read from closed file".to_string());
    }
    let mode = info.borrow().mode;
    if mode.abs() != 1 {
        return Err("can only read from 'r'/'rb'".to_string());
    }
    // Read the file
    let mut ret: Vec<u8> = vec![];
    if let Err(e) = info.borrow_mut().file.as_ref().unwrap()
        .read_to_end(&mut ret) {
        return Err(e.to_string());
    }
    if mode == 1 {
        // Return a string
        let Ok(string) = String::from_utf8(ret) else {
            return Err("invalid string".to_string());
        };
        return Ok(Value::Str(Rc::new(string)));
    }
    return Ok(Value::FastList(Rc::new(ret.iter().map(|i| Value::Byte(*i)).collect())));
}

fn sk_seek(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("seek", args.len(), 2)?;
    }
    // Check and get file
    let Value::File(ref mut info) = args[0].clone() else {
        return Err(format!("cannot seek on {}", args[0].get_type()));
    };
    if info.borrow().closed {
        return Err("cannot seek on closed file".to_string());
    }
    if info.borrow().mode.abs() != 2 {
        return Err("can only seek on 'w'/'wb'".to_string());
    }
    // Get position
    let pos = args[1].to_int();
    if pos < 0 {
        return Err("position cannot be negative".to_string());
    }
    let max = info.borrow_mut().file.as_ref().unwrap()
        .seek(SeekFrom::End(0)).unwrap();
    let pos: u64 = pos.try_into().unwrap();
    if pos > max {
        return Err("cannot seek to position larger than file".to_string());
    }
    // Seek
    info.borrow_mut().file.as_ref().unwrap()
        .seek(SeekFrom::Start(pos)).unwrap();
    Ok(Value::None)
}

fn sk_write(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("write", args.len(), 2)?;
    }
    // Now check args
    let Value::File(ref mut info) = args[0].clone() else {
        return Err(format!("cannot write to {}", args[0].get_type()));
    };
    if info.borrow().closed {
        return Err("cannot write to closed file".to_string());
    }
    let mode = info.borrow().mode;
    if mode.abs() != 2 && mode != 0 {
        return Err("can only write to 'w'/'wb'/'a'".to_string());
    }
    let Value::Str(ref str) = args[1] else {
        return Err(format!("expected String got {}", args[1].get_type()));
    };
    if let Err(err) =
        write!(info.borrow_mut().file.as_ref().unwrap(), "{}", str)
    {
        return Err(err.to_string());
    }
    return Ok(Value::None);
}

// Casting
// Int
fn sk_int(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("int", args.len(), 1)?;
    }
    return Ok(Value::Int(args[0].to_int()));
}

// Float
fn sk_float(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("float", args.len(), 1)?;
    }
    return Ok(Value::Float(args[0].to_float()));
}

// String
fn sk_string(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("string", args.len(), 1)?;
    }
    // Bytes are a special case
    return Ok(Value::Str(Rc::new(if let Value::Byte(byte) = args[0] {
        (byte as char).to_string()
    } else {
        let Ok(str) = args[0].to_string() else {
            // Failed casts return none
            return Ok(Value::None);
        };
        str
    })));
}


// Byte
fn sk_byte(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("byte", args.len(), 1)?;
    }
    Ok(match &args[0] {
        // Strings
        Value::Str(s) if s.chars().count() == 1 =>
            Value::Byte(s.chars().next().unwrap() as u8),
        Value::Str(s) if s.chars().count() > 1 => {
            let mut ret: Vec<Value> = vec![];
            for chr in s.chars() {
                ret.push(Value::Byte(chr as u8));
            }
            Value::FastList(Rc::new(ret))
        }
        Value::Str(_) =>
            return Err("cannot convert empty string to bytes".to_string()),
        // Int and identity
        Value::Int(i) => Value::Byte((*i % 256).try_into().unwrap()),
        Value::Byte(b) => Value::Byte(*b),
        // Anything else
        e => return Err(format!("cannot convert {} to byte", e.get_type())),
    })
}

// Pointer
#[cfg(feature = "cffi")]
fn sk_ptr(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("__burlap_ptr", args.len(), 1)?;
    }
    if let Value::Ptr(_) = args[0].clone() {
        return Ok(args[0].clone());
    }
    let num = args[0].to_int();
    if num < 0 {
        return Err("Pointers can't be negative".to_string());
    }
    return Ok(Value::Ptr(num as usize));
}

// Internal functies
fn sk_typed_eq(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args("__burlap_typed_eq", args.len(), 2)?;
    }
    return Ok(Value::Bool(args[0] == args[1]));
}

fn sk_real_print(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args("__burlap_print", args.len(), 1)?;
    }
    println!("{:?}", args[0]);
    return Ok(Value::None);
}

fn sk_throw(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        vm.bad_args("__burlap_throw", args.len(), 1)?;
    }
    Err(args[0].to_string()?)
}

fn sk_set_var(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args("__burlap_set_var", args.len(), 2)?;
    }
    let Value::RefType(offset, global) = args[0] else {
        return Err("__burlap_set_var requires a __burlap_reftype".to_string());
    };
    (if global { &mut vm.globals } else { &mut vm.locals })[offset as usize] = args[1].clone();
    return Ok(Value::None);
}

fn sk_load_var(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        vm.bad_args("__burlap_load_var", args.len(), 1)?;
    }
    let Value::RefType(offset, global) = args[0] else {
        return Err("__burlap_set_var requires a __burlap_reftype".to_string());
    };
    return Ok((if global { &vm.globals } else { &vm.locals })[offset as usize].clone());
}

#[cfg(feature = "cffi")]
fn sk_libload(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        vm.bad_args("__burlap_load_library", args.len(), 1)?;
    }
    let (_, file) = vm.program.get_info(vm.at as u32);
    let mut path = PathBuf::from("./".to_owned() + &file);
    path.pop();
    path.push(args[0].to_string()?);
    return Ok(Value::Ptr(load_library(path.to_str().unwrap().to_string())?))
}

#[cfg(feature = "cffi")]
fn sk_functiload(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args("__burlap_load_functi", args.len(), 2)?;
    }
    // First arg must be library handle
    let Value::Ptr(handle) = args[0] else {
        return Err("First argument must be library handle".to_string());
    };
    return Ok(Value::Ptr(load_functi(handle, args[1].to_string()?)?));
}

#[cfg(feature = "cffi")]
fn sk_call_c(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 3 {
        vm.bad_args("__burlap_ffi_call", args.len(), 3)?;
    }
    // First arg must be function pointer
    let Value::Ptr(func) = args[0] else {
        return Err("First argument must be function pointer".to_string());
    };
    // Second arg must be list
    let Some(c_args) = args[1].values() else {
        return Err("Second argument must be list".to_string());
    };
    // Third arg must be return value
    let Value::Str(ret_type) = args[2].clone() else {
        return Err(
            "Third argument must be string (try using `type()`?)".to_string()
        );
    };
    return ffi_call(func, c_args, (*ret_type).clone());
}

fn sk_fastrange(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args("__burlap_range", args.len(), 2)?;
    }
    let (at, max) = (args[0].to_int(), args[1].to_int());
    // For (0, 100) step is 1, for (100, 0) it's -1, etc..
    let step = if max.gt(&at) {1} else {-1};
    return Ok(Value::RangeType(at, max, step));
}

// Debugging functies
fn sk_debug_on(vm: &mut Vm, _args: Vec<Value>) -> Result<Value, String> {
    vm.args.is_debug = true;
    Ok(Value::None)
}
fn sk_debug_off(vm: &mut Vm, _args: Vec<Value>) -> Result<Value, String> {
    vm.args.is_debug = false;
    Ok(Value::None)
}

// Sets a key in a list and returns it
fn set_key(
    vlist: &mut Value, key: Value, val: Value
) -> Result<(), String> {
    // TODO: This code sucks
    let Value::List(ref mut list_rc) = vlist else {
        let Value::FastList(ref mut list_rc) = vlist else {
            return Err(format!(
                "failed to index {} with {}",
                vlist.to_string()?, key.to_string()?
            ));
        };
        let list = Rc::make_mut(list_rc);
        // Push
        let as_int = key.to_int();
        if key.get_type() == "Number" && as_int >= 0 {
            let as_uint = as_int as usize;
            let entry = list.get_mut(as_uint)
                .map(|s| *s = val.clone());
            if entry.is_none() {
                if as_uint == list.len() {
                    // Push
                    if val != Value::None {
                        list.push(val);
                    }
                } else {
                    return Err(
                        "cannot assign to out of bounds key".to_string()
                    );
                }
            } else if val == Value::None {
                // Remove that key
                list.remove(as_uint);
            }
        } else {
            // Convert to normal list
            let mut slowlist = Vec::<(String, Value)>::with_capacity(
                list.len()
            );
            for i in list {
                slowlist.push(("".to_string(), i.clone()));
            }
            // Set
            *vlist = Value::List(Rc::new(slowlist));
            return set_key(vlist, key, val);
        }
        return Ok(());
    };
    let list = Rc::make_mut(list_rc);
    // Insert
    if key.get_type() == "Number" {
        let key = key.to_int();
        if key < 0 || key as usize > list.len() {
            return Err(
                "cannot assign to out of bounds key".to_string()
            );
        }
        let key = key as usize;
        if key == list.len() {
            // Add new key
            if val != Value::None {
                list.push(("".to_string(), val));
            }
        } else {
            if val == Value::None {
                // Delete key
                list.remove(key);
            } else {
                // Modify key
                list[key].1 = val;
            }
        }
    } else {
        // Add or create string key
        let key = key.to_string()?;
        let mut index = usize::MAX;
        for (at, ref mut i) in list.iter_mut().enumerate() {
            if i.0 == key {
                if val == Value::None {
                    index = at;
                    break;
                } else {
                    i.1 = val;
                    return Ok(());
                }
            }
        }
        // Modify
        if val != Value::None {
            list.push((key, val));
        } else if index != usize::MAX {
            list.remove(index);
        }
    }
    return Ok(());
}

// The big switch, runs every instruction
#[inline]
fn exec_next(vm: &mut Vm) -> Result<(), String> {
    let (op, a, b, c) = vm.cur_opcode();
    match op {
        Opcode::NOP => {},

        // Regs
        Opcode::LD => {
            let val = vm.program.consts[shift2(a, b)].clone();
            vm.set_reg(c, val);
        },
        Opcode::LDL => {
            let val = vm.program.consts[shift3(a, b, c)].clone();
            vm.set_reg(16, val);
        },
        Opcode::CP => {
            // Not using `.get_reg` as it pops the stack, and CP needs to be used for dup
            let val = vm.get_reg_ref(a).clone();
            vm.set_reg(b, val);
        },
        Opcode::POP => {
            vm.stack.pop().unwrap();
        },

        // Functions
        Opcode::SARG => {
            let len = vm.stack.len();
            let args: Vec<Value> = vm.stack[len - a as usize .. len]
                .iter().cloned().collect();
            vm.call_frames.last_mut().unwrap().args = Some(args);
        },
        Opcode::CARG => {
            let args = Value::FastList(
                Rc::new(if !vm.call_frames.is_empty() {
                    vm.call_frames.last_mut().unwrap().args.clone()
                        .expect("No saved args at `args()` call!")
                } else {
                    vm.args.program_args.iter().map(|i| Value::Str(Rc::new(i.clone()))).collect()
                })
            );
            vm.set_reg(a, args);
        },
        Opcode::CALL => {
            vm.call(shift3(a, b, c));
        },
        Opcode::VCALL => {
            let functi = vm.get_reg(a);
            let Value::Functi(fn_name) = functi else {
                return Err(format!("cannot call {}", functi.get_type()));
            };
            vm.call_name((*fn_name).clone(), b)?;
        }
        Opcode::RCALL => {
            // Jump
            vm.at = shift3(a, b, c);
        },
        Opcode::RET => {
            let frame = vm.call_frames.pop().unwrap();
            let pos = frame.return_addr;
            vm.at = pos + 1;
            vm.jump = true;
            vm.regs = frame.regs;
            vm.pop_locals(frame.local_size);
        }

        // Lists
        Opcode::LFL => {
            let mut size = shift2(b, c);
            let mut list = Vec::<Value>::with_capacity(size);
            while size > 0 {
                size -= 1;
                let val = vm.stack.pop().unwrap();
                if let Value::None = val {
                    continue;
                }
                list.push(val);
            }
            vm.set_reg(a, Value::FastList(Rc::new(list.into_iter().rev().collect())));
        },
        Opcode::LL => {
            let mut size = shift2(b, c);
            let mut list = Vec::<(String, Value)>::with_capacity(size);
            // Get the keys and values and put them into the list
            while size > 0 {
                size -= 1;
                let Value::Str(key) = vm.stack.pop().unwrap() else {
                    return Err("Non-string list index".to_string());
                };
                let val = vm.stack.pop().unwrap();
                if let Value::None = val {
                    continue;
                }
                let key = (*key).clone();
                /*if key == "" {
                    key = (old_size - list.len() - 1).to_string();
                }*/
                // Store
                list.push((key, val));
            }
            vm.set_reg(a, Value::List(Rc::new(list.into_iter().rev().collect())));
        },
        Opcode::INX => {
            let list = vm.get_reg(a);
            let index = vm.get_reg(b);
            let value = list.index(&index).unwrap_or(Value::None);
            vm.set_reg(c, value);
        },
        Opcode::ITER => {
            // Convert to an iterator
            let iter = vm.get_reg(a).to_iter()?;
            vm.set_reg(b, iter);
        },
        Opcode::NXT => {
            // Get the value
            let iter = vm.get_reg_mut(a);
            if let Some(val) = iter.iter_next()? {
                // Add value
                vm.set_reg(b, val);
                // Jump because not empty
                vm.jump((c as u32).try_into().unwrap());
            }
        },
        Opcode::SKY => {
            let key: Value;
            let val: Value;
            let mut list = vm.get_reg(a);
            key = vm.get_reg(b);
            val = vm.get_reg(c);
            set_key(&mut list, key, val)?;
            vm.set_reg(a, list);
        },

        // Variables
        t_op @ (Opcode::LV_L | Opcode::LV_G) => {
            // Get var
            let var = vm.get_var(shift2(a, b) as u16, t_op == Opcode::LV_G);
            // Load
            vm.set_reg(c, var);
        },
        t_op @ (Opcode::SV_L | Opcode::SV_G) => {
            // Get value
            let val = vm.get_reg(c);
            // Set
            vm.set_var(shift2(a, b) as u16, val, t_op == Opcode::SV_G);
        },
        t_op @ (Opcode::PLC | Opcode::PGB) => {
            let (mut len, vec) = if t_op == Opcode::PLC {
                // Copy args, if needed
                let vec = &mut vm.locals;
                let mut c = c;
                while c != 0 {
                    let val = vm.stack.pop().unwrap();
                    vec.push(val);
                    c -= 1;
                }
                // Locals says how many more
                (shift2(a, b), vec)
            } else {
                // Globals says how many total
                let vec = &mut vm.globals;
                let mut len = shift2(a, b);
                if len <= vec.len() { return Ok(()) }
                len -= vec.len();
                (len, vec)
            };
            // Fill
            vec.reserve(len);
            while len != 0 {
                vec.push(Value::None);
                len -= 1;
            }
        },
        Opcode::ALO => {
            // Get offset
            let offset = shift2(a, b) + vm.locals.len() - 1;
            // Set
            vm.set_reg(c, Value::RefType(offset as i32, false));
        },

        // Binops
        Opcode::ADD => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, (lhs + rhs)?);
        },
        Opcode::SUB => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, (lhs - rhs)?);
        },
        Opcode::MUL => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, (lhs * rhs)?);
        },
        Opcode::DIV => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, (lhs / rhs)?);
        },
        Opcode::MOD => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, (lhs % rhs)?);
        },
        Opcode::IN => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg_ref(a);
            let contains = lhs.contains(&rhs);
            // Side effects
            let ltype = lhs.get_type();
            if a == 16 {
                vm.stack.pop().unwrap();
            }
            if let Some(b) = contains {
                vm.set_reg(c, Value::Bool(b));
            } else {
                return Err(
                    format!("Cannot use in on {:?} and {:?}", ltype, rhs.get_type())
                );
            }
        },
        Opcode::EQ => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.eq(&rhs)));
        },
        Opcode::LT => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.to_float() < rhs.to_float()));
        },
        Opcode::GT => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.to_float() > rhs.to_float()));
        },
        Opcode::AND => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.is_truthy() && rhs.is_truthy()));
        },
        Opcode::OR => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.is_truthy() || rhs.is_truthy()));
        },
        Opcode::XOR => {
            let rhs = vm.get_reg(b);
            let lhs = vm.get_reg(a);
            vm.set_reg(c, Value::Bool(lhs.is_truthy() != rhs.is_truthy()));
        },
        Opcode::NOT => {
            if a == b {
                let v = vm.get_reg_mut(a);
                *v = Value::Bool(!v.is_truthy());
            } else {
                let v = vm.get_reg(a);
                vm.set_reg(b, Value::Bool(!v.is_truthy()));
            }
        },

        // Jumps
        Opcode::JMP => {
            vm.jump(shift3(a, b, c).try_into().unwrap());
        },
        Opcode::JMPB => {
            let offset: i32 = shift3(a, b, c).try_into().unwrap();
            vm.jump(-offset);
        },
        Opcode::JMPNT => {
            let cond = vm.get_reg(a);
            // Check if it should jump
            if !cond.is_truthy() {
                vm.jump(shift2(b, c).try_into().unwrap());
            }
        }
    };
    // If nothing has returned an error, everything is fine
    Ok(())
}

pub fn run(vm: &mut Vm) -> bool {
    if vm.program.ops.is_empty() {
        return true;
    }
    vm.call_frames = vec![];
    vm.stack = vec![];
    /*if vm.args.is_debug {
        println!("Consts: {:?}", vm.program.consts);
        println!("Ops: {:?}", vm.program.ops);
    }*/
    loop {
        if vm.args.is_debug {
            // Print debugging info
            let (line, filename) = vm.program.get_info(vm.at as u32);
            let op = dis_single(&vm.program, vm.at);
            println!("{filename}:{line}: {op}");
        }
        // Run
        if let Err(s) = exec_next(vm) {
            let (line, filename) = vm.program.get_info(vm.at as u32);
            println!("Runtime Error at {}:{}: {}", filename, line, s);

            // Backtrace
            if vm.args.backtrace {
                for i in vm.call_frames.iter().rev() {
                    let (line, filename) = vm.program.get_info(i.return_addr as u32 + 1);
                    println!("Triggered by call at {}:{}", filename, line);
                }
            }

            vm.at = vm.program.ops.len() - 1;
            return false;
        }
        if vm.args.is_debug {
            println!("Stk: {:?}, regs: {:?}", vm.stack, vm.regs);
        }

        // Move forward
        if vm.jump {
            vm.jump = false;
            continue;
        }
        if vm.at + 1 != vm.program.ops.len() {
            vm.at += 1;
        } else {
            // At the end
            if vm.args.is_debug {
                // print the stack at the end
                println!("FINAL: {:?}", vm.stack);
            }
            if vm.args.is_repl && !vm.stack.is_empty() {
                // Print the result
                if vm.stack[0] != Value::None {
                    print!("{}", vm.stack[0].to_string()
                        .map(|x| x + "\n").unwrap_or("".to_string())
                    );
                }
            }
            break;
        }
    }
    return true;
}
