use std::collections::HashMap;
use std::path::PathBuf;
use std::io::Write;
use std::io;

use crate::common::IMPOSSIBLE_STATE;
use crate::parser::{ASTNode, ASTNode::*};
use crate::import_file;
use crate::lexer::TokenType;
use crate::value::Value;

use indexmap::map::IndexMap;

// Functies are burlap functions in rust (like print)
type Functie = fn(&mut Vm, Vec<Value>) -> Value;

// Sub struct for extentions/functies
pub struct Functies {
    builtin: HashMap<String, Functie>
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum Opcode {
    // The almightly NOP
    NOP,

    // Stack
    // PUSH value ([const u8 index] -> value)
    PUSH,
    // PUSH value ([const u24 index] -> value)
    PUSH3,
    // DUPlicate head (value -> value, value)
    DUP,

    // Variable
    // Push Variable ("name" -> value)
    PV,
    // Declare Variable ("name", value)
    DV,
    // Set Variable ("name", value)
    SV,

    // Scope
    // Raise Scope
    RS,
    // Lower Scope
    LS,

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

    // Calls
    // CALL function ([u8], name, args...)
    CALL,
    // RETurn (ret)
    RET,
}

// VM state
pub struct Vm {
    // Some config for the VM
    pub is_repl: bool,
    pub has_err: bool,
    pub in_func: bool,

    // Global vars and functions (which are always global)
    is_global: bool,
    globals: HashMap<String, Value>,
    functions: HashMap<String, ASTNode>,
    functies: Functies,

    // Variables in the current scope
    // Each scope stacks
    var_names: Vec<String>,
    var_vals: Vec<Value>,
    var_min: usize,

    // Extentions
    pub extentions: Vec<String>,

    // Used for importing
    pub import_path: PathBuf,

    // Sacks stack
    stack: Vec<Value>,
    // Ops and constants
    ops: Vec<u8>,
    pub jump: bool,
    at: usize,
    consts: Vec<Value>,
}

impl Vm {
    // Init
    pub fn new(
        is_repl: bool, import_path: PathBuf, extentions: Vec<String>
    ) -> Vm {
        // Builtin functions
        let functies = Functies{builtin: HashMap::from([
            // Builtins
            ("print".to_string(), sk_print as Functie),
            ("input".to_string(), sk_input as Functie),
            ("type".to_string(), sk_type as Functie),
            ("len".to_string(), sk_len as Functie),
            // Casts
            ("int".to_string(), sk_int as Functie),
            ("float".to_string(), sk_float as Functie),
            ("string".to_string(), sk_string as Functie),
        ])};
        // This is one of those times I really really wish Rust had defaults
        Vm {
            is_repl, has_err: false, in_func: false,
            is_global: true, globals: HashMap::new(),
            functions: HashMap::new(), functies,
            var_names: Vec::new(), var_vals: Vec::new(),
            var_min: 0, extentions, import_path, stack: Vec::new(),
            ops: Vec::new(), jump: false, at: 0, consts: Vec::new(),
        }
    }
    // Getting vars
    fn get_local(&self, name: &String) -> Value {
        // Gets a local var
        let mut index = self.var_names.len();
        while index > self.var_min {
            index -= 1;
            if &self.var_names[index] == name {
                return self.var_vals[index].clone();
            }
        }
        // Failed to get var, return an error
        return Value::Error(format!("no variable called \"{}\"", name));
    }
    fn get_global(&self, name: &String) -> Value {
        // Gets a var in the global scope
        return match self.globals.get(name) {
            Some(val) => val.clone(),
            _ => Value::Error(format!("no variable called \"{}\"", name))
        };
    }
    pub fn get_var(&self, name: &String) -> Value {
        // Try to get it in the normal scope first
        return match self.get_local(name) {
            // Not found, try global scope
            Value::Error(_) => self.get_global(name),
            // It's in the local scope
            x => x,
        }
    }
    // Create a variable
    pub fn make_var(
        &mut self, name: &String, val: Value, global: bool, force: bool
    ) -> bool {
        if global {
            // Check
            if let Value::Error(_) = self.get_global(name) {
            } else if force {} else {
                return false;
            }
            // Add
            self.globals.insert(name.clone(), val);
        } else {
            // Check
            if let Value::Error(_) = self.get_local(name) {
            } else if force {} else {
                return false;
            }
            // Add
            self.var_names.push(name.clone());
            self.var_vals.push(val);
        }
        return true;
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
    pub fn set_var(&mut self, name: &String, val: Value, global: bool) -> bool {
        // Sets a variable, returns true if it was successful
        return if global {
            self.set_global(name, val)
        } else {
            self.set_local(name, val)
        };
    }
    pub fn change_var(&mut self, name: &String, val: Value) -> bool {
        // Changes a variable to a diffrent value
        if !self.set_local(name, val.clone()) {
            return self.set_global(name, val)
        }
        return true;
    }
    // Raise/lower scope
    pub fn lower_scope(&mut self, call: bool) -> (bool, usize, usize) {
        // Lowers the scope
        // Impossible for a lowered scope to be global
        let old_global = self.is_global;
        self.is_global = false;
        // Functions can't access higher yet non-global scopes
        let old_min = self.var_min;
        if call {
            self.var_min = self.var_names.len();
        }
        // Where the vars need to be cut off at
        let old_top = self.var_names.len();
        // Return data needed to raise the dead/scope
        return (old_global, old_min, old_top);
    }
    pub fn raise_scope(&mut self, (old_global, old_min, old_top): (bool, usize, usize)) {
        // Raises the scope back
        self.is_global = old_global;
        self.var_min = old_min;
        // Remove new vars
        while old_top < self.var_names.len() {
            self.var_names.pop();
            self.var_vals.pop();
        }
    }
    fn bad_args(&self, name: &String, got: usize, need: usize) -> Value {
        if got > need {
            return Value::Error(
                format!("too mamy args for {} (got {} need {})", name, got, need)
            );
        }
        return Value::Error(
            format!("too few args for {} (got {} need {})", name, got, need)
        );
    }
    // Call a function
    pub fn call(&mut self, name: &String, args: &Vec<Value>) -> Value {
        /*/ Builtin functions
        match (&self).functies.builtin.get(name) {
            Some(functie) => { return functie(self, args.clone()); },
            _ => ()
        }
        // Try to get the function from the name
        let function: &ASTNode;
        match (&self).functions.get(name) {
            Some(val) => function = val,
            _ => { return Value::Error(format!("no function called \"{}\"", name)); }
        };
        // Get the args and body
        let arg_names: Vec<String>;
        let body: ASTNode;
        if let FunctiStmt(_name, named_args, fbody) = function {
            arg_names = named_args.clone();
            body = (**fbody).clone();
        } else {
            // Impossible!
            return Value::Error(IMPOSSIBLE_STATE.to_string());
        }
        // Lower scope
        let old_func = self.in_func;
        self.in_func = true;
        let scope_data = self.lower_scope(true);
        // Push the args
        let mut index = 0;
        for arg_name in &arg_names {
            if index >= args.len() {
                // Error
                self.in_func = old_func;
                self.raise_scope(scope_data);
                return self.bad_args(name, args.len(), arg_names.len());
            }
            // Push arg
            self.make_var(&arg_name, args[index].clone(), false, true);
            index += 1;
        }
        // Check for too many args
        if index != args.len() {
            self.in_func = old_func;
            self.raise_scope(scope_data);
            return self.bad_args(name, args.len(), arg_names.len());
        }
        // Run body
        let ret = exec(self);
        // Raise scope
        self.in_func = old_func;
        self.raise_scope(scope_data);
        // TODO*/
        return Value::None;
    }

    pub fn cur_op(&mut self) -> u8 {
        self.ops[self.at]
    }
    pub fn cur_opcode(&mut self) -> Opcode {
        return unsafe {std::mem::transmute(self.ops[self.at])};
    }
    pub fn next_op(&mut self) -> u8 {
        self.at += 1;
        if self.at > self.ops.len() {
            panic!("Read too many ops!");
        }
        self.ops[self.at]
    }

    // Push/pop
    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }
    pub fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(v) => v,
            _ => panic!("Overpopped stack!")
        }
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
            if self.at >= self.ops.len() {
                panic!("Positive jump out of bounds!");
            }
        }
        self.jump = true;
    }
}

// Builtin Functions (prefixed with 'sk_')
// Print
fn sk_print(vm: &mut Vm, args: Vec<Value>) -> Value {
    if vm.extentions.contains(&"va-print".to_string()) {
        // VA print extention
        for i in args {
            print!("{} ", i.to_string());
        }
        println!("");
    } else if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"print".to_string(), args.len(), 1);
    } else {
        // Normal printing
        println!("{}", args[0].to_string());
    }
    return Value::None;
}

// Input
fn sk_input(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"input".to_string(), args.len(), 1);
    }
    // Print the prompt
    print!("{}", args[0].to_string());
    let _ = std::io::stdout().flush();
    // Get input
    let mut buffer = String::new();
    return match io::stdin().read_line(&mut buffer) {
        Err(_) => Value::Str("".to_string()),
        _ => Value::Str(buffer.trim_end().to_string())
    };
}

// Type
fn sk_type(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"type".to_string(), args.len(), 1);
    }
    return Value::Str(args[0].get_type());
}

// Len
fn sk_len(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"type".to_string(), args.len(), 1);
    }
    // Check that the type is a list
    if let Value::List(l) = &args[0] {
        return Value::Int(l.len() as i32);
    }
    return Value::Error("len() argument 1 must be a list".to_string());
}

// Casting
// Int
fn sk_int(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"int".to_string(), args.len(), 1);
    }
    return Value::Int(args[0].to_int());
}

// Float
fn sk_float(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"float".to_string(), args.len(), 1);
    }
    return Value::Float(args[0].to_float());
}

// String
fn sk_string(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return vm.bad_args(&"string".to_string(), args.len(), 1);
    }
    return Value::Str(args[0].to_string());
}

// The big switch, runs every instruction
fn exec_next(vm: &mut Vm) -> Result<Value, String> {
    // Unsafe because casting an int to enum might not be valid
    match vm.cur_opcode() {
        Opcode::NOP => {},

        // Push
        Opcode::PUSH => {
            let index = vm.read(1);
            vm.push(vm.consts[index as usize].clone());
        },
        Opcode::PUSH3 => {
            let index = vm.read(3);
            vm.push(vm.consts[index as usize].clone());
        },
        Opcode::DUP => {
            let val = vm.stack.last();
            vm.push(val.expect("Overpopped stack!").clone());
        },

        // Binops
        Opcode::ADD => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(lhs + rhs);
        },
        Opcode::SUB => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(lhs - rhs);
        },
        Opcode::MUL => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(lhs * rhs);
        },
        Opcode::DIV => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(lhs / rhs);
        },
        Opcode::MOD => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(lhs % rhs);
        },
        Opcode::EQ => {
            let rhs = vm.pop();
            let lhs = vm.pop();
            vm.push(Value::Bool(lhs == rhs));
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

        // Variables
        Opcode::PV => {
            // Get varname
            let Value::Str(varname) = vm.pop() else {
                return Err(IMPOSSIBLE_STATE.to_string());
            };
            // Get var
            let var = vm.get_var(&varname);
            if let Value::Error(s) = var {
                return Err(s);
            }
            // Push
            vm.push(var);
        },
        Opcode::SV => {
            // Get varname
            let Value::Str(varname) = vm.pop() else {
                return Err(IMPOSSIBLE_STATE.to_string());
            };
            // Set
            let val = vm.pop();
            if !vm.set_var(&varname, val, vm.is_global) {
                return Err(format!("no variable called \"{}\"", varname));
            }
        },
        Opcode::DV => {
            let Value::Str(varname) = vm.pop() else {
                return Err(IMPOSSIBLE_STATE.to_string());
            };
            let val = vm.pop();
            vm.make_var(&varname, val, vm.is_global, true);
        },

        // Scope
        Opcode::LS => {
            //vm.scope = vm.lower_scope(false);
        }
        Opcode::RS => {
            //vm.raise_scope(vm.scope);
        }

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
        Opcode::CALL => {
            // Get args
            let mut arg_num = vm.read(1);
            let Value::Str(name) = vm.pop() else {
                panic!("Non-string function name");
            };
            let mut args = Vec::<Value>::with_capacity(arg_num as usize);
            while arg_num != 0 {
                args.push(vm.pop());
                arg_num -= 1;
            }
            // Call
            vm.call(&name, &args);
        },

        Opcode::RET => {},
    };
    return Ok(Value::None);
}

pub fn run(vm: &mut Vm, ops: Vec<u8>, consts: Vec<Value>) -> bool {
    vm.ops = ops;
    vm.at = 0;
    vm.consts = consts;
    vm.stack = Vec::new();
    loop {
        // Print debugging info
        let opcode = vm.cur_opcode();
        let op = vm.cur_op();
        println!("{}: {:?}({}) {:?}", vm.at, opcode, op, vm.stack);
        // Run
        if let Err(s) = exec_next(vm) {
            println!("{}", s);
            return false;
        }

        // Move forward
        if vm.jump {
            vm.jump = false;
            continue;
        }
        if vm.at + 1 != vm.ops.len() {
            vm.next_op();
        } else {
            // At the end
            println!("FINAL: {:?}", vm.stack);
            break;
        }
    }
    return true;
}
