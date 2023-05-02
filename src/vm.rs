use std::cell::RefCell;
use std::rc::Rc;
use std::fs::OpenOptions;
use std::io::{Write, Read};
use std::io;

use crate::Arguments;
use crate::cffi::{load_functi, load_library};
use crate::compiler::Program;
use crate::value::{FileInfo, Value};

use indexmap::map::IndexMap;
use rustc_hash::FxHashMap;

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum Opcode {
    // The almighty NOP
    NOP,

    // Stack
    // PUSH value ([const u8 index] -> value)
    PUSH,
    // PUSH value ([const u24 index] -> value)
    PUSH3,
    // DUPlicate head (value -> value, value)
    DUP,
    // DELete head (value)
    DEL,

    // Scope (Call scope is handled by CALL and RET)
    // Raise Scope
    RS,
    // LowEr VarIable scope
    LEVI,

    // Functions
    // declare FuNction (name, arg#)
    FN,
    // CALL function (name, arg#, args...)
    CALL,
    // RETurn (ret)
    RET,

    // Variables
    // Push Variable ("name" -> value)
    PV,
    // Declare Variable ("name", value)
    DV,
    // Set Variable ("name", value)
    SV,
    // Declare Or Set ariable ("name", value)
    DOS,

    // Lists
    // Load List (size, keys-and-values -> list)
    LL,
    // Load Fast List (size, values -> list)
    LFL,
    // INdeX (list, key -> value)
    INX,
    // To ITeR (value -> iter)
    TITR,
    // NeXT (iter -> (iter, value, true) | (iter, false))
    NXT,
    // Set KeY (value, list, key -> list)
    SKY,

    // Math
    // ADD (value, value -> value)
    ADD,
    // SUBtract (value, value -> value)
    SUB,
    // MULtiply (value, value -> value)
    MUL,
    // DIVide (value, value -> value)
    DIV,
    // MODulo (value, value -> value)
    MOD,

    // Boolean
    // AND (value, value -> value)
    AND,
    // OR (value, value -> value)
    OR,
    // XOR (value, value -> value)
    XOR,
    // NOT (value, value -> value)
    NOT,

    // Comparison
    // EQuals (value, value -> value)
    EQ,
    // Greator Than (value, value -> value)
    GT,
    // Less Than (value, value -> value)
    LT,

    // Jumps
    // JuMP Unconditionally ([u8])
    JMPU,
    // JuMP Backward, unconditionally ([u8])
    JMPB,
    // JuMP if NoT ([u8], offset)
    JMPNT,
}


// A functie is a sack functions implemented in rust
type Functie = fn(&mut Vm, Vec<Value>) -> Result<Value, String>;

// VM state
pub struct Vm {
    // Extensions
    pub args: Arguments,

    // State
    pub has_err: bool,
    pub in_func: bool,

    // Variables
    // Global vars
    is_global: bool,
    globals: FxHashMap<String, Value>,
    // Functies
    functies: FxHashMap<String, Functie>,
    // Non-global variables
    var_names: Vec<String>,
    var_vals: Vec<Value>,
    var_min: usize,
    // Scope
    scope: Vec<(usize, usize, u8)>,

    // The program
    pub program: Program,
    // Sacks stack
    stack: Vec<Value>,
    pub jump: bool,
    pub at: usize,
}

impl Vm {
    // Init
    pub fn new(args: Arguments) -> Vm {
        // Builtin functions
        let mut functies = FxHashMap::with_capacity_and_hasher(
            16, Default::default()
        );
        // Builtins
        functies.insert("print".to_string(), sk_print as Functie);
        functies.insert("input".to_string(), sk_input as Functie);
        functies.insert("type".to_string(), sk_type as Functie);
        functies.insert("len".to_string(), sk_len as Functie);
        functies.insert("range".to_string(), sk_range as Functie);
        // File IO
        functies.insert("open".to_string(), sk_open as Functie);
        functies.insert("close".to_string(), sk_close as Functie);
        functies.insert("read".to_string(), sk_read as Functie);
        functies.insert("write".to_string(), sk_write as Functie);
        functies.insert("flush".to_string(), sk_flush as Functie);
        // Casts
        functies.insert("int".to_string(), sk_int as Functie);
        functies.insert("float".to_string(), sk_float as Functie);
        functies.insert("string".to_string(), sk_string as Functie);
        // Non-togglable internals
        functies.insert("__burlap_range".to_string(), sk_fastrange as Functie);
        // Burlap internal functies
        if args.extensions.contains(&"burlap-extensions".to_string()) {
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
                "__burlap_load_lib".to_string(), sk_libload as Functie
            );
            functies.insert(
                "__burlap_load_functi".to_string(), sk_functiload as Functie
            );
            functies.insert(
                "__burlap_ptr".to_string(), sk_ptr as Functie
            );
        }
        // I really wish Rust had defaults, but it doesn't
        Vm {
            args, has_err: false, in_func: false, functies,
            is_global: true, globals: FxHashMap::default(),
            var_names: Vec::new(), var_vals: Vec::new(),
            var_min: 0, stack: Vec::new(), scope: Vec::new(),
            program: Program::new(), jump: false, at: 0
        }
    }

    // Getting vars
    fn get_local(&self, name: &String) -> Result<Value, String> {
        // Gets a local var
        let mut index = self.var_names.len();
        while index > self.var_min {
            index -= 1;
            if &self.var_names[index] == name {
                return Ok(self.var_vals[index].clone());
            }
        }
        // Failed to get var, return an error
        return Err(format!("no variable called \"{}\"", name));
    }

    fn get_global(&self, name: &String) -> Result<Value, String> {
        // Gets a var in the global scope
        return match self.globals.get(name) {
            Some(val) => Ok(val.clone()),
            _ => Err(format!("no variable called \"{}\"", name))
        };
    }

    pub fn get_var(&self, name: &String) -> Result<Value, String> {
        if self.is_global {
            // Don't bother checking local scope
            return self.get_global(name);
        }
        return self.get_local(name).or_else(|_| self.get_global(name));
    }

    pub fn check_for_var(&self, name: &String) -> bool {
        if !self.is_global {
            // Check locals
            if self.var_names[self.var_min..].contains(name) {
                return true;
            }
        }
        // Check globals
        return self.globals.contains_key(name);
    }

    // Create a variable
    pub fn make_var(
        &mut self, name: &String, val: Value
    ) -> Result<(), String> {
        // Check if it already exists
        if self.check_for_var(name) {
            return Err(format!("cannot redefine \"{}\"", name));
        }
        // Create
        if self.is_global {
            self.globals.insert(name.clone(), val);
        } else {
            self.var_names.push(name.clone());
            self.var_vals.push(val);
        }
        Ok(())
    }

    // Set a variable
    pub fn set_local(&mut self, name: &String, val: Value) -> bool {
        // Sets a local var
        let mut index = self.var_names.len();
        while index > self.var_min {
            index -= 1;
            if &self.var_names[index] == name {
                // Now that I have it, set it
                self.var_vals[index] = val;
                return true;
            }
        }
        // Failed to get var
        return false;
    }

    pub fn set_global(&mut self, name: &String, val: Value) -> bool {
        // Sets a global value
        if !self.globals.contains_key(name) {
            return false;
        }
        self.globals.insert(name.clone(), val);
        return true;
    }

    pub fn set_var(&mut self, name: &String, val: Value) -> Result<(), String>
    {
        // Changes a variable to a different value
        if self.set_local(name, val.clone()) {
            return Ok(());
        }
        if self.set_global(name, val) {
            return Ok(());
        }
        return Err(format!("no variable called \"{}\"", name));
    }

    // Scope
    pub fn lower_scope(&mut self, call: bool) {
        // Impossible for a lowered scope to be global
        self.is_global = false;
        // Where the vars need to be cut off at
        let old_top = self.var_names.len();
        // Functions can't access higher yet non-global scopes
        let old_min = self.var_min;
        if call {
            self.var_min = old_top;
        }
        let count: u8 = if call {
            0
        } else {
            let (_, _, c) = *self.scope.last().unwrap_or(&(0, 0, 0));
            c + 1
        };
        // Push data so the scope can be raised
        self.scope.push((old_min, old_top, count));
    }
    pub fn raise_scope(&mut self) -> Result<(), String> {
        let Some((old_min, old_top, _)) = self.scope.pop() else {
            return Err("cannot raise global scope".to_string());
        };
        // Raise scope back up
        self.is_global = self.scope.is_empty();
        self.var_min = old_min;
        // Remove new vars
        self.var_names.truncate(old_top);
        self.var_vals.truncate(old_top);

        Ok(())
    }

    fn bad_args(
        &self, name: &String, got: usize, need: usize
    ) -> Result<(), String> {
        Err(if got > need {
            format!("too many args for {} (got {} need {})", name, got, need)
        } else {
            format!("too few args for {} (got {} need {})", name, got, need)
        })
    }

    // Call a function
    #[inline]
    pub fn call(
        &mut self, name: &String, args: &Vec<Value>
    ) -> Result<(), String> {

        // Non-builtin functions
        let Some((pos, arg_num)) = self.program.functis.get(name) else
        {
            // Builtin functions
            if let Some(functie) = self.functies.get(name) {
                // Reverse (normally the vm pops the args in reverse)
                let args = args.clone().into_iter().rev().collect();
                let ret = functie(self, args)?;
                self.push(ret);
                return Ok(());
            }
            return Err(format!("no function called \"{}\"", name));
        };
        // Dereference
        let (pos, arg_num) = (*pos, *arg_num);
        // Check args
        if arg_num != args.len() as i32 {
            self.bad_args(name, args.len(), arg_num as usize)?;
        }
        // Store return address
        self.push(Value::Int(self.at as i32));
        // Jump there
        self.at = pos;
        self.jump = true;
        // Push args
        for arg in args {
            self.push(arg.clone());
        }
        // Lower scope
        self.lower_scope(true);
        return Ok(());
    }

    pub fn cur_op(&mut self) -> u8 {
        self.program.ops[self.at]
    }

    pub fn cur_opcode(&mut self) -> Opcode {
        // Unsafe because casting an int to enum might not be valid
        return unsafe {std::mem::transmute(self.program.ops[self.at])};
    }

    pub fn next_op(&mut self) -> u8 {
        self.at += 1;
        if self.at > self.program.ops.len() {
            panic!("Read too many ops!");
        }
        self.program.ops[self.at]
    }

    // Push/pop
    #[inline]
    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn read(&mut self, size: u8) -> i32 {
        // Reads multiple bytes into a single number
        let mut ret: i32 = 0;
        for _ in 0..size {
            ret <<= 8;
            ret += self.next_op() as i32;
        }
        return ret;
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
    if vm.args.extensions.contains(&"va-print".to_string()) {
        // VA print extension
        for i in args {
            print!("{} ", i.to_string());
        }
        println!();
    } else if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"print".to_string(), args.len(), 1)?;
    } else {
        // Normal printing
        println!("{}", args[0].to_string());
    }
    return Ok(Value::None);
}

// Input
fn sk_input(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"input".to_string(), args.len(), 1)?;
    }
    // Print the prompt
    print!("{}", args[0].to_string());
    let _ = std::io::stdout().flush();
    // Get input
    let mut buffer = String::new();
    return Ok(match io::stdin().read_line(&mut buffer) {
        Err(_) => Value::Str("".to_string()),
        _ => Value::Str(buffer.trim_end().to_string())
    });
}

// Type
fn sk_type(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"type".to_string(), args.len(), 1)?;
    }
    return Ok(Value::Str(args[0].get_type()));
}

// Len
fn sk_len(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"len".to_string(), args.len(), 1)?;
    }
    // Check that the type is a list
    if let Value::FastList(l) = &args[0] {
        return Ok(Value::Int(l.len() as i32));
    }
    if let Value::List(l) = &args[0] {
        return Ok(Value::Int(l.len() as i32));
    }
    return Err("len() argument 1 must be a list".to_string());
}

// Range
fn sk_range(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args(&"range".to_string(), args.len(), 2)?;
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
    return Ok(Value::FastList(ret));
}

// File IO
fn sk_open(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args(&"open".to_string(), args.len(), 2)?;
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
            OpenOptions::new().write(true).create(true).truncate(true)
                .open(file.clone()),
            if mode == "w" {2} else {-2}
        )},
        // Read
        "r" | "rb" => {(
            OpenOptions::new().read(true).open(file.clone()),
            if mode == "r" {1} else {-1}
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
        closed: false,
        file: Some(fileinfo)
    }));
    return Ok(Value::File(file, mode, fi));
}

fn sk_close(vm: &mut Vm, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"close".to_string(), args.len(), 1)?;
    }
    let Value::File(_, _, ref mut info) = args[0] else {
        return Err(format!("cannot close from {}", args[0].get_type()));
    };
    info.borrow_mut().closed = true;
    info.borrow_mut().file = None;
    Ok(Value::None)
}

fn sk_flush(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"flush".to_string(), args.len(), 1)?;
    }
    // Burlap doesn't buffer file io
    Ok(Value::None)
}

fn sk_read(vm: &mut Vm, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"read".to_string(), args.len(), 1)?;
    }
    let Value::File(_, mode, ref mut info) = args[0] else {
        return Err(format!("cannot read from {}", args[0].get_type()));
    };
    if info.borrow().closed {
        return Err(format!("cannot read from closed file"));
    }
    if mode.abs() != 1 {
        return Err(format!("can only read from 'r'/'rb'"));
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
            return Err(format!("invalid string"));
        };
        return Ok(Value::Str(string));
    }
    return Ok(Value::FastList(ret.iter().map(|i| Value::Byte(*i)).collect()));
}

fn sk_write(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args(&"write".to_string(), args.len(), 2)?;
    }
    let Value::File(_, mode, ref mut info) = args[0].clone() else {
        return Err(format!("cannot write to {}", args[0].get_type()));
    };
    if info.borrow().closed {
        return Err(format!("cannot write to closed file"));
    }
    if mode.abs() != 2 && mode != 0 {
        return Err(format!("can only write to 'w'/'wb'/'a'"));
    }
    // Now check args
    let Value::Str(ref str) = args[1] else {
        return Err(format!("expected String got {}", args[1].get_type()));
    };
    if let Err(err) =
        write!(info.borrow_mut().file.as_ref().unwrap(), "{}", str) {
        return Err(err.to_string());
    }
    return Ok(Value::None);
}

// Casting
// Int
fn sk_int(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"int".to_string(), args.len(), 1)?;
    }
    return Ok(Value::Int(args[0].to_int()));
}

// Float
fn sk_float(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"float".to_string(), args.len(), 1)?;
    }
    return Ok(Value::Float(args[0].to_float()));
}

// String
fn sk_string(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"string".to_string(), args.len(), 1)?;
    }
    return Ok(Value::Str(args[0].to_string()));
}

// Pointer
fn sk_ptr(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"__burlap_ptr".to_string(), args.len(), 1)?;
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
        vm.bad_args(&"__burlap_typed_eq".to_string(), args.len(), 2)?;
    }
    return Ok(Value::Bool(args[0] == args[1]));
}

fn sk_real_print(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        // Invalid args
        vm.bad_args(&"__burlap_print".to_string(), args.len(), 1)?;
    }
    println!("{:?}", args[0]);
    return Ok(Value::None);
}

fn sk_throw(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        vm.bad_args(&"__burlap_throw".to_string(), args.len(), 1)?;
    }
    Err(args[0].to_string())
}

fn sk_libload(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 {
        vm.bad_args(&"__burlap_load_library".to_string(), args.len(), 1)?;
    }
    return Ok(Value::Ptr(load_library(args[0].to_string())?))
}

fn sk_functiload(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args(&"__burlap_load_functi".to_string(), args.len(), 2)?;
    }
    // First arg must be library handle
    let Value::Ptr(handle) = args[0] else {
        return Err("First argument must be library handle".to_string());
    };
    return Ok(Value::Ptr(load_functi(handle, args[1].to_string())?));
}

fn sk_fastrange(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args(&"__burlap_range".to_string(), args.len(), 2)?;
    }
    let (at, max) = (args[0].to_int(), args[1].to_int());
    // For (0, 100) step is 1, for (100, 0) it's -1, etc..
    let step = if max.gt(&at) {1} else {-1};
    return Ok(Value::RangeType(at, max, step));
}

// Sets a key in a list and returns it
fn set_key(
    vlist: Value, key: Value, val: Value
) -> Result<Value, String> {
    let Value::List(mut list) = vlist else {
        let Value::FastList(mut list) = vlist else {
            return Err(format!(
                "failed to index {} with {}",
                vlist.to_string(), key.to_string()
            ));
        };
        // Push
        let as_int = key.to_int();
        if key.get_type() == "Number" && as_int >= 0 {
            let as_uint = as_int as usize;
            let entry = list.get_mut(as_uint)
                .and_then(|s| Some(*s = val.clone()));
            if entry.is_none() {
                if as_uint != list.len() + 1 {
                    list.push(val);
                } else {
                    return Err(
                        "cannot assign to out of bounds key".to_string()
                    );
                }
            }
        } else {
            // Convert to normal list
            let mut slowlist = IndexMap::<String, Value>::with_capacity(
                list.len()
            );
            let mut at = 0;
            for i in list {
                slowlist.insert(at.to_string(), i);
                at += 1;
            }
            // Set
            return set_key(Value::List(slowlist), key, val);
        }
        return Ok(Value::FastList(list));
    };
    // Insert
    if key.get_type() == "Number" {
        let entry = list.entry(key.to_string())
            .and_modify(|s| *s = val.clone());
        // Try to insert a new key
        if key.to_int() != entry.index().try_into().unwrap_or(-1) {
            return Err(
                "cannot assign to out of bounds key".to_string()
            );
        }
        entry.or_insert(val);
    } else {
        // Add or create
        let key = key.to_string();
        list.entry(key).and_modify(|s| *s = val.clone()).or_insert(val);
    }
    Ok(Value::List(list))
}

// The big switch, runs every instruction
#[inline]
fn exec_next(vm: &mut Vm) -> Result<(), String> {
    match vm.cur_opcode() {
        Opcode::NOP => {},

        // Push
        Opcode::PUSH => {
            let index = vm.read(1);
            vm.push(vm.program.consts[index as usize].clone());
        },
        Opcode::PUSH3 => {
            let index = vm.read(3);
            vm.push(vm.program.consts[index as usize].clone());
        },
        Opcode::DUP => {
            let val = vm.stack.last();
            vm.push(val.expect("Overpopped stack!").clone());
        },
        Opcode::DEL => {
            vm.stack.pop();
        },

        // Scope
        // Raise scope
        Opcode::RS => {
            vm.raise_scope()?;
        },
        Opcode::LEVI => {
            vm.lower_scope(false);
        },

        // Lists
        Opcode::LL => {
            let Value::Int(mut size) = vm.pop() else {
                return Err("Non-int list size".to_string());
            };
            // Get the keys and values and put them into the list
            let mut list = IndexMap::<String, Value>::with_capacity(
                size as usize,
            );
            while size > 0 {
                let Value::Str(key) = vm.pop() else {
                    return Err("Non-string list index".to_string());
                };
                let val = vm.pop();
                // Store
                list.insert(key, val);
                size -= 1;
            }
            list.reverse();
            vm.push(Value::List(list));
        },
        Opcode::LFL => {
            let Value::Int(mut size) = vm.pop() else {
                return Err("Non-int list size".to_string());
            };
            // Get values
            let mut list = Vec::<Value>::with_capacity(size as usize);
            while size > 0 {
                list.push(vm.pop());
                size -= 1;
            }
            list.reverse();
            vm.push(Value::FastList(list));
        },
        Opcode::INX => {
            let index = vm.pop();
            let list = vm.pop();
            match list.index(index.clone()) {
                 Some(x) => vm.push(x.clone()),
                 _ => return Err(format!(
                    "failed to index {} with {}",
                    list.to_string(), index.to_string()
                )),
            }
        },
        Opcode::TITR => {
            // Convert to an iterator
            let iter = vm.pop().to_iter()?;
            vm.push(iter);
        },
        Opcode::NXT => {
            // Get the value
            let val = vm.stack.last_mut()
                .ok_or("Overpopped stack!")?.iter_next()?;
            if let Some(val) = val {
                // Push value
                vm.push(val);
                vm.push(Value::Bool(true));
            } else {
                // End of list
                vm.push(Value::Bool(false));
            }
        },
        Opcode::SKY => {
            let key = vm.pop();
            let list = vm.pop();
            let val = vm.pop();
            vm.push(set_key(list, key, val)?)
        },

        // Variables
        Opcode::PV => {
            // Get varname
            let Value::Str(varname) = vm.pop() else {
                return Err("variable name must be string".to_string());
            };
            // Get var
            let var = vm.get_var(&varname)?;
            // Push
            vm.push(var);
        },
        Opcode::SV => {
            // Get varname
            let Value::Str(varname) = vm.pop() else {
                return Err("variable name must be string".to_string());
            };
            // Set
            let val = vm.pop();
            vm.set_var(&varname, val)?;
        },
        Opcode::DV => {
            let Value::Str(varname) = vm.pop() else {
                return Err("variable name must be string".to_string());
            };
            let val = vm.pop();
            vm.make_var(&varname, val)?;
        },
        Opcode::DOS => {
            let Value::Str(varname) = vm.pop() else {
                return Err("variable name must be string".to_string());
            };
            let val = vm.pop();
            if vm.make_var(&varname, val.clone()).is_err() {
                vm.set_var(&varname, val)?;
            }
        },

        // Binops
        Opcode::ADD => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push((lhs + rhs)?);
        },
        Opcode::SUB => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push((lhs - rhs)?);
        },
        Opcode::MUL => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push((lhs * rhs)?);
        },
        Opcode::DIV => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push((lhs / rhs)?);
        },
        Opcode::MOD => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push((lhs % rhs)?);
        },
        Opcode::EQ => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.eq(rhs)));
        },
        Opcode::LT => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.to_float() < rhs.to_float()));
        },
        Opcode::GT => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.to_float() > rhs.to_float()));
        },
        Opcode::AND => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.is_truthy() && rhs.is_truthy()));
        },
        Opcode::OR => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.is_truthy() || rhs.is_truthy()));
        },
        Opcode::XOR => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs.is_truthy() != rhs.is_truthy()));
        },
        Opcode::NOT => {
            let v = vm.pop();
            vm.push(Value::Bool(!v.is_truthy()));
        },

        // Jumps
        Opcode::JMPU => {
            let offset = vm.read(1);
            vm.jump(offset);
        },
        Opcode::JMPB => {
            let offset = vm.read(1);
            vm.jump(-offset);
        },
        Opcode::JMPNT => {
            let offset = vm.read(1);
            let cond = vm.pop();
            // Check if it should jump
            if !cond.is_truthy() {
                vm.jump(offset);
            }
        },

        // Functions
        Opcode::FN => {
            let Value::Str(name) = vm.pop() else {
                return Err("Non-string function name".to_string());
            };
            let Value::Int(args_num) = vm.pop() else {
                return Err("Non-int arg number".to_string());
            };
            // Add the function
            vm.program.functis.insert(name, (vm.at + 3, args_num));
        }

        Opcode::CALL => {
            let Value::Str(name) = vm.pop() else {
                return Err("Non-string function name".to_string());
            };
            let Value::Int(mut arg_num) = vm.pop() else {
                return Err("Non-int arg number".to_string());
            };
            // Get the args
            let mut args = Vec::<Value>::with_capacity(arg_num as usize);
            while arg_num > 0 {
                args.push(vm.pop());
                arg_num -= 1;
            }
            // Call
            vm.call(&name, &args)?;
        },

        Opcode::RET => {
            let Value::Int(pos) = vm.stack.swap_remove(vm.stack.len() - 2) else
            {
                return Err("Non-int return address".to_string());
            };
            // Fix scope
            loop {
                let Some((_, _, c)) = vm.scope.last() else {
                    break;
                };
                if *c == 0 {
                    vm.raise_scope()?;
                    break;
                }
                vm.raise_scope()?;
            }
            // Move
            vm.at = pos as usize + 1;
            vm.jump = true;
        }
    };
    // If nothing has returned an error, everything is fine
    Ok(())
}

pub fn run(vm: &mut Vm) -> bool {
    if vm.program.ops.is_empty() {
        return true;
    }
    //vm.at = 0;
    vm.stack = vec![];
    loop {
        if vm.args.is_debug {
            // Print debugging info
            let opcode = vm.cur_opcode();
            let op = vm.cur_op();
            println!("{}: {:?}({}) {:?}", vm.at, opcode, op, vm.stack);
        }
        // Run
        if let Err(s) = exec_next(vm) {
            println!("Runtime Error: {}", s);
            vm.at = vm.program.ops.len() - 1;
            return false;
        }

        // Move forward
        if vm.jump {
            vm.jump = false;
            continue;
        }
        if vm.at + 1 != vm.program.ops.len() {
            vm.next_op();
        } else {
            // At the end
            if vm.args.is_debug {
                // print the stack at the end
                println!("FINAL: {:?}", vm.stack);
            }
            if vm.args.is_repl && !vm.stack.is_empty() {
                // Print the result
                if vm.stack[0] != Value::None {
                    println!("{}", vm.stack[0].to_string());
                }
            }
            break;
        }
    }
    return true;
}
