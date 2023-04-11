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
    // Register instructions
    // PUSH value ([const u8 index] -> value)
    PUSH,
    // PUSH value ([const u24 index] -> value)
    PUSH3,
    // Push Variable ("name" -> value)
    PV,
    // Declare Variable ("name", value)
    DV,
    // Set Variable ("name", value)
    SV,
    // Math instructions
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
    // Boolean instructions
    // AND
    AND,
    // OR
    OR,
    // XOR
    XOR,
    // NOT
    NOT,
    // Comparison
    // EQuals
    EQ,
    // Greator Than
    GT,
    // Less Than
    LT,
    // Misc
    // CALL a function (CALL "print")
    CALL
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
            ops: Vec::new(), at: 0, consts: Vec::new(),
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
        // Sets a varible, returns true if it was successful
        return if global {
            self.set_global(name, val)
        } else {
            self.set_local(name, val)
        };
    }
    pub fn change_var(&mut self, name: &String, val: Value) -> bool {
        // Changes a varible to a diffrent value
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
        // Builtin functions
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
        // TODO
        return Value::None;
    }

    pub fn cur_op(&mut self) -> u8 {
        self.ops[self.at]
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

// Eval (for expressions)
fn eval(vm: &mut Vm) -> Result<Value, String> {
    // Unsafe because casting an int to enum might not be valid
    match unsafe {std::mem::transmute(vm.cur_op())} {
        // Push
        Opcode::PUSH => {
            let index = vm.read(1);
            vm.push(vm.consts[index as usize].clone());
        },
        Opcode::PUSH3 => {
            let index = vm.read(3);
            vm.push(vm.consts[index as usize].clone());
        },
        Opcode::ADD => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(lhs + rhs);
        },
        // Binops
        Opcode::SUB => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(lhs - rhs);
        },
        Opcode::MUL => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(lhs * rhs);
        },
        Opcode::DIV => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(lhs / rhs);
        },
        Opcode::MOD => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(lhs % rhs);
        },
        Opcode::EQ => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(Value::Bool(lhs == rhs));
        },
        Opcode::LT => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(Value::Bool(lhs.to_float() < rhs.to_float()));
        },
        Opcode::GT => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(Value::Bool(lhs.to_float() > rhs.to_float()));
        },
        Opcode::AND => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(Value::Bool(lhs.is_truthy() && rhs.is_truthy()));
        },
        Opcode::OR => {
            let lhs = vm.pop();
            let rhs = vm.pop();
            vm.push(Value::Bool(lhs.is_truthy() || rhs.is_truthy()));
        },
        Opcode::XOR => {
            let lhs = vm.pop();
            let rhs = vm.pop();
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
        Opcode::CALL => todo!()
    };
    return Ok(Value::None);
}

fn exec(vm: &mut Vm) -> Result<Value, String> {
    /*return match node {
        BodyStmt(body) => {
            // Lower scope
            let scope_data = vm.lower_scope(false);
            // Run each statement
            let mut ret: Value;
            for i in body {
                ret = exec(vm, i);
                if let Value::Error(_) = ret {
                    return ret;
                }
                if ret != Value::Null {
                    // Raise scope
                    vm.raise_scope(scope_data);
                    return ret;
                }
            }
            // Raise scope
            vm.raise_scope(scope_data);
            Value::Null
        },
        ReturnStmt(ret) => {
            // Return
            eval(vm, &**ret)
        },
        IfStmt(cond, body, elsestmt) => {
            // Execute either body or else body
            let condition = eval(vm, &**cond);
            if let Value::Error(_) = condition {
                return condition;
            }
            if condition.is_truthy() {
                exec(vm, &**body)
            } else {
                // If there isn't a else cond, this will run an empty body
                exec(vm, &**elsestmt)
            }
        },
        // Declaring vars
        LetStmt(name, val) => {
            let val = eval(vm, &**val);
            if let Value::Error(_) = val {
                return val;
            }
            if vm.make_var(name, val, vm.is_global, false) {
                Value::Null
            } else {
                Value::Error(format!("cannot redefine variable \"{}\"", name))
            }
        },
        // Declaring functions
        FunctiStmt(name, _args, _body) => {
            vm.functions.insert(name.clone(), node.clone());
            Value::Null
        },
        // Loops
        LoopStmt(var, iter, body) => {
            // Make the var if it doesn't exist
            if let Value::Error(_) = vm.get_var(var) {
                vm.make_var(var, Value::Int(0), vm.is_global, false);
            }
            // Check iter
            let (mut min, max): (i32, i32);
            if let CallExpr(name, args) = &**iter {
                // Check name
                if name != "range" {
                    return Value::Error(
                        "only range is currently supported for iterative loops"
                    .to_string());
                }
                // Check args
                if args.len() != 2 {
                    return vm.bad_args(&name, args.len(), 2);
                }
                // Get args
                min = eval(vm, &args[0]).to_int();
                max = eval(vm, &args[1]).to_int();
            } else {
                return Value::Error(
                    "only range is currently supported for iterative loops".to_string()
                );
            }
            // Do the loop
            while min <= max {
                vm.set_var(var, Value::Int(min), vm.is_global);
                exec(vm, &*body.clone());
                min += 1;
            }
            Value::Null
        },
        // While loop
        WhileStmt(cond, body) => {
            // It's really easy, just check cond and loop
            while eval(vm, &**cond).is_truthy() {
                exec(vm, &*body.clone());
            }
            Value::Null
        },
        // Import
        ImportStmt(name) => {
            // Get file name
            let fname = eval(vm, &**name);
            let name = fname.to_string();
            // Get the file path
            let mut path = vm.import_path.clone();
            path.push(name.clone());
            // Import
            if import_file(vm, &mut path) {
                Value::Null
            } else {
                Value::Error(format!("failed to import {}", name))
            }
        },*/
    let val = eval(vm)?;
    if vm.is_repl && val != Value::Null && val != Value::None {
        println!("{}", val.to_string());
    }
    Ok(Value::None)
}

pub fn run(vm: &mut Vm, ops: Vec<u8>, consts: Vec<Value>) -> bool {
    vm.ops = ops;
    vm.at = 0;
    vm.consts = consts;
    vm.stack = Vec::new();
    loop {
        if let Err(s) = exec(vm) {
            println!("{}", s);
            return false;
        }
        println!("{:?}", vm.stack);
        if vm.at + 1 != vm.ops.len() {
            vm.next_op();
        } else {
            break;
        }
    }
    return true;
}
