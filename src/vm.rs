use std::collections::HashMap;
use std::path::PathBuf;
use std::io::Write;
use std::io;

use crate::Arguments;
use crate::compiler::Program;
use crate::value::Value;

use indexmap::map::IndexMap;

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
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

    // Variables
    // Push Variable ("name" -> value)
    PV,
    // Declare Variable ("name", value)
    DV,
    // Set Variable ("name", value)
    SV,

    // Lists
    // Load list (size, keys-and-values -> list)
    LL,
    // INdeX (list, key -> value)
    INX,
    // To ITeR (value -> iter)
    TITR,
    // NeXT (iter -> (iter, value, true) | (iter, false))
    NXT,

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

    // Functions
    // declare FuNction (name, arg#)
    FN,
    // CALL function (name, arg#, args...)
    CALL,
    // RETurn (ret)
    RET,
}


// A functie is a sack functions implemented in rust
type Functie = fn(&mut Vm, Vec<Value>) -> Result<Value, String>;

// VM state
pub struct Vm {
    // Extensions
    pub args: Arguments,
    // Used for importing
    pub import_path: PathBuf,

    // State
    pub has_err: bool,
    pub in_func: bool,

    // Variables
    // Global vars
    is_global: bool,
    globals: HashMap<String, Value>,
    // Functies
    functies: HashMap<String, Functie>,
    // Non-global variables
    var_names: Vec<String>,
    var_vals: Vec<Value>,
    var_min: usize,

    // The program
    pub program: Program,
    // Sacks stack
    stack: Vec<Value>,
    pub jump: bool,
    at: usize,
}

impl Vm {
    // Init
    pub fn new(args: Arguments, import_path: PathBuf) -> Vm {
        // Builtin functions
        let mut functies = HashMap::from([
            // Builtins
            ("print".to_string(), sk_print as Functie),
            ("input".to_string(), sk_input as Functie),
            ("type".to_string(), sk_type as Functie),
            ("len".to_string(), sk_len as Functie),
            ("range".to_string(), sk_range as Functie),
            // Casts
            ("int".to_string(), sk_int as Functie),
            ("float".to_string(), sk_float as Functie),
            ("string".to_string(), sk_string as Functie),
            // Non-togglable internals
            ("__burlap_range".to_string(), sk_fastrange as Functie),
        ]);
        // Burlap internal functies
        if args.extensions.contains(&"burlap-extensions".to_string()) {
            functies.insert(
                "__burlap_typed_eq".to_string(), sk_typed_eq as Functie
            );
            functies.insert(
                "__burlap_print_stack".to_string(), sk_print_stack as Functie
            );
        }
        // I really wish Rust had defaults, but it doesn't
        Vm {
            args, has_err: false, in_func: false, import_path,
            is_global: true, globals: HashMap::new(), functies,
            var_names: Vec::new(), var_vals: Vec::new(),
            var_min: 0, stack: Vec::new(),
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
        return Err(format!("no variable called \"{}\"", name).to_string());
    }

    fn get_global(&self, name: &String) -> Result<Value, String> {
        // Gets a var in the global scope
        return match self.globals.get(name) {
            Some(val) => Ok(val.clone()),
            _ => Err(format!("no variable called \"{}\"", name).to_string())
        };
    }

    pub fn get_var(&self, name: &String) -> Result<Value, String> {
        // Try to get it in the normal scope first
        return match self.get_local(name) {
            // Not found, try global scope
            Err(_) => self.get_global(name),
            // It's in the local scope
            x => x,
        }
    }

    // Create a variable
    pub fn make_var(&mut self, name: &String, val: Value) {
        // Change if it already exists
        if self.change_var(name, val.clone()) {
            return;
        }
        // Create
        if self.is_global {
            self.globals.insert(name.clone(), val);
        } else {
            self.var_names.push(name.clone());
            self.var_vals.push(val);
        }
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
        // Changes a variable to a different value
        if !self.set_local(name, val.clone()) {
            return self.set_global(name, val)
        }
        return true;
    }

    // Scope TODO
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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

    fn bad_args(
        &self, name: &String, got: usize, need: usize
    ) -> Result<(), String> {
        Err(if got > need {
            format!("too many args for {} (got {} need {})", name, got, need)
        } else {
            format!("too few args for {} (got {} need {})", name, got, need)
        }.to_string())
    }

    // Call a function
    pub fn call(
        &mut self, name: &String, args: &Vec<Value>
    ) -> Result<(), String> {
        // Builtin functions
        if let Some(functie) = self.functies.get(name) {
            // Reverse (normally the vm pops the args in reverse)
            let args = args.clone().into_iter().rev().collect();
            let ret = functie(self, args)?;
            self.push(ret);
            return Ok(());
        }

        // Non-builtin functions
        let Some((pos, arg_num)) = self.program.functis.get(name) else
        {
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
        return Ok(());
    }

    pub fn cur_op(&mut self) -> u8 {
        self.program.ops[self.at]
    }

    pub fn cur_opcode(&mut self) -> Opcode {
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
    let mut ret = IndexMap::<String, Value>::new();
    while min != max {
        ret.insert(min.to_string(), Value::Int(min));
        min += offset;
    }
    ret.insert(min.to_string(), Value::Int(min));
    return Ok(Value::List(ret));
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

// Internal functies
fn sk_typed_eq(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        // Invalid args
        vm.bad_args(&"__burlap_typed_eq".to_string(), args.len(), 2)?;
    }
    return Ok(Value::Bool(args[0] == args[1]));
}

fn sk_fastrange(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 2 {
        vm.bad_args(&"__burlap_range".to_string(), args.len(), 2)?;
    }
    let (at, max) = (args[0].to_int(), args[1].to_int());
    let step = if max.gt(&at) {1} else {-1};
    return Ok(Value::RangeType(at, max, step));
}

fn sk_print_stack(vm: &mut Vm, args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 0 {
        vm.bad_args(&"__burlap_print_stack".to_string(), args.len(), 0)?;
    }
    println!("{:?}", vm.stack);
    return Ok(Value::None);
}

// The big switch, runs every instruction
fn exec_next(vm: &mut Vm) -> Result<(), String> {
    // Unsafe because casting an int to enum might not be valid
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

        // Lists
        Opcode::LL => {
            let Value::Int(mut size) = vm.pop() else {
                return Err("Non-int list size".to_string());
            };
            // Get the keys and values and put them into the list
            let mut list = IndexMap::<String, Value>::with_capacity(
                size as usize
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
            vm.push(Value::List(list));
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
            if !vm.set_var(&varname, val, vm.is_global) {
                return Err(format!("no variable called \"{}\"", varname));
            }
        },
        Opcode::DV => {
            let Value::Str(varname) = vm.pop() else {
                return Err("variable name must be string".to_string());
            };
            let val = vm.pop();
            vm.make_var(&varname, val);
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
            let ret = vm.pop();
            let Value::Int(pos) = vm.pop() else {
                return Err("Non-int return address".to_string());
            };
            // Move
            vm.at = pos as usize + 1;
            vm.jump = true;
            // Push ret back
            vm.push(ret);
        }

        #[allow(unreachable_patterns)]
        _ => {
            panic!("Unhandled opcode! {:?}", vm.cur_opcode());
        },
    };
    // If nothing has returned an error, everything is fine
    Ok(())
}

pub fn run(vm: &mut Vm, program: Program) -> bool {
    if program.ops.is_empty() {
        return true;
    }
    vm.at = 0;
    vm.stack = vec![];
    vm.program = program;
    loop {
        if vm.args.is_debug {
            // Print debugging info
            let opcode = vm.cur_opcode();
            let op = vm.cur_op();
            println!("{}: {:?}({}) {:?}", vm.at, opcode, op, vm.stack);
        }
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
        if vm.at + 1 != vm.program.ops.len() {
            vm.next_op();
        } else {
            // At the end
            if vm.args.is_debug {
                // print the stack at the end
                println!("FINAL: {:?}", vm.stack);
            }
            if vm.args.is_repl && vm.stack.len() >= 1 {
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
