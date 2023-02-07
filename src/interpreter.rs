use std::collections::HashMap;
use std::path::PathBuf;
use std::fmt;
use std::io::Write;
use std::io;

use crate::common::IMPOSSIBLE_STATE;
use crate::parser::ASTNode;
use ASTNode::*;
use crate::import_file;
use crate::lexer::TokenType;
use crate::value::Value;

// Functies are burlap functions in rust (like print)
type Functie = fn(&mut Interpreter, Vec<Value>) -> Value;

// Sub struct for extentions/functies
pub struct Functies {
    builtin: HashMap<String, Functie>
}
impl fmt::Debug for Functies {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{{...}}")
    }
}

// Interpreter state
#[derive(Debug)]
pub struct Interpreter {
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
}

impl Interpreter {
    // Init
    pub fn new(
        is_repl: bool, import_path: PathBuf, extentions: Vec<String>
    ) -> Interpreter {
        // Builtin functions
        let functies = Functies{builtin: HashMap::from([
            ("print".to_string(), sk_print as Functie),
            ("input".to_string(), sk_input as Functie),
            // Casts
            ("int".to_string(), sk_int as Functie),
            ("float".to_string(), sk_float as Functie),
            ("string".to_string(), sk_string as Functie),
        ])};
        Interpreter {
            is_repl, has_err: false, in_func: false,
            is_global: true, globals: HashMap::new(),
            functions: HashMap::new(), functies,
            var_names: Vec::new(), var_vals: Vec::new(),
            var_min: 0, extentions, import_path
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
        if let ASTNode::FunctiStmt(_name, named_args, fbody) = function {
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
        let ret = exec(self, &body);
        // Raise scope
        self.in_func = old_func;
        self.raise_scope(scope_data);
        return ret;
    }
}

// Builtin Functions (prefixed with 'sk_')
// Print
fn sk_print(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
    if interpreter.extentions.contains(&"va-print".to_string()) {
        // VA print extention
        for i in args {
            print!("{} ", i.to_string());
        }
        println!("");
    } else if args.len() != 1 {
        // Invalid args
        return interpreter.bad_args(&"print".to_string(), args.len(), 1);
    } else {
        // Normal printing
        println!("{}", args[0].to_string());
    }
    return Value::None;
}

// Input
fn sk_input(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return interpreter.bad_args(&"input".to_string(), args.len(), 1);
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

// Casting
// Int
fn sk_int(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return interpreter.bad_args(&"int".to_string(), args.len(), 1);
    }
    return Value::Int(args[0].to_int());
}

// Float
fn sk_float(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return interpreter.bad_args(&"float".to_string(), args.len(), 1);
    }
    return Value::Float(args[0].to_float());
}

// String
fn sk_string(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        // Invalid args
        return interpreter.bad_args(&"string".to_string(), args.len(), 1);
    }
    return Value::Str(args[0].to_string());
}

// Eval (for expressions)
fn eval(interpreter: &mut Interpreter, node: &ASTNode) -> Value {
    return match node {
        // Normal boring values
        StringExpr(val) => Value::Str(val.clone()),
        NumberExpr(val) => Value::Int(val.clone()),
        DecimalExpr(val) => Value::Float(val.clone()),
        BoolExpr(val) => Value::Bool(val.clone()),
        NoneExpr => Value::None,
        // Vars/function
        VarExpr(val) => interpreter.get_var(val),
        CallExpr(name, args) => {
            // Eval args
            let mut vals: Vec<Value> = vec![];
            for arg in args {
                let new_arg = eval(interpreter, arg);
                if let Value::Error(_) = new_arg {
                    return new_arg;
                }
                vals.push(new_arg);
            }
            // Call
            interpreter.call(name, &vals)
        }
        // Unary
        UnaryExpr(unary, val) => {
            // "&**" is a tad cursed, but this is rust after all.
            let value = eval(interpreter, &**val);
            if let Value::Error(_) = value {
                return value;
            }
            // Do the op
            let ret = match unary {
                TokenType::Not => Value::Bool(!value.is_truthy()),
                TokenType::Minus => Value::Int(0) - value.clone(),
                // These ones are done separately
                TokenType::PlusPlus | TokenType::MinusMinus => Value::Null,
                _ => Value::Error(format!("unknown unary op ({:?})", unary))
            };
            if let TokenType::PlusPlus | TokenType::MinusMinus = unary {} else {
                return ret;
            }
            // Do ++/--
            let name: String;
            if let ASTNode::VarExpr(n) = &**val {
                name = n.clone();
            } else {
                return Value::Error("++/-- can only be used on variable".to_string());
            }
            let val: Value;
            if let TokenType::PlusPlus = unary {
                // Increment
                val = value + Value::Int(1);
                interpreter.change_var(&name, val.clone());
            } else {
                // Decrement
                val = value - Value::Int(1);
                interpreter.change_var(&name, val.clone());
            }
            // Return
            return val;
        },
        // Binops
        BinopExpr(lhs, op, rhs) => {
            // Var name for variable ops
            let name = if let ASTNode::VarExpr(n) = &**lhs {
                n.clone()
            } else { "".to_string() };
            // Get values
            let left = eval(interpreter, &**lhs);
            let right = eval(interpreter, &**rhs);
            if let Value::Error(_) = left {
                return left;
            }
            if let Value::Error(_) = right {
                return right;
            }
            // Do the binop
            match op {
                // Math
                TokenType::Plus => left + right,
                TokenType::Minus => left - right,
                TokenType::Times => left * right,
                TokenType::Div => left / right,
                TokenType::Modulo => left % right,
                // Eq/neq
                TokenType::EqualsEquals => Value::Bool(left.eq(right)),
                TokenType::NotEquals => Value::Bool(!left.eq(right)),
                // Gt/lt
                TokenType::Lt => Value::Bool(left.to_float() < right.to_float()),
                TokenType::Gt => Value::Bool(left.to_float() > right.to_float()),
                TokenType::LtEquals => Value::Bool(left.to_float() <= right.to_float()),
                TokenType::GtEquals => Value::Bool(left.to_float() >= right.to_float()),
                // Logical
                TokenType::And =>
                    Value::Bool(left.is_truthy() && right.is_truthy()),
                TokenType::Or =>
                    Value::Bool(left.is_truthy() || right.is_truthy()),
                TokenType::Xor =>
                    Value::Bool(left.is_truthy() != right.is_truthy()),
                // Variable
                TokenType::Equals => {
                    interpreter.set_var(&name, right, interpreter.is_global);
                    Value::None
                },
                TokenType::PlusEquals => {
                    let val = interpreter.get_var(&name);
                    if let Value::Error(_) = val {
                        return val;
                    }
                    interpreter.change_var(&name, val + right);
                    Value::None
                },
                TokenType::MinusEquals => {
                    let val = interpreter.get_var(&name);
                    if let Value::Error(_) = val {
                        return val;
                    }
                    interpreter.change_var(&name, val - right);
                    Value::None
                },
                TokenType::TimesEquals => {
                    let val = interpreter.get_var(&name);
                    if let Value::Error(_) = val {
                        return val;
                    }
                    interpreter.change_var(&name, val * right);
                    Value::None
                },
                TokenType::DivEquals => {
                    let val = interpreter.get_var(&name);
                    if let Value::Error(_) = val {
                        return val;
                    }
                    interpreter.change_var(&name, val / right);
                    Value::None
                },
                // Unknown
                _ => Value::Error(format!("unknown binop op ({:?})", op))
            }
        },
        // Default to none
        _ => Value::Error(format!("eval error (failed to evaluate {:?})", node)),
    };
}

fn exec(interpreter: &mut Interpreter, node: &ASTNode) -> Value {
    return match node {
        BodyStmt(body) => {
            // Lower scope
            let scope_data = interpreter.lower_scope(false);
            // Run each statement
            let mut ret: Value;
            for i in body {
                ret = exec(interpreter, i);
                if let Value::Error(_) = ret {
                    return ret;
                }
                if ret != Value::Null {
                    // Raise scope
                    interpreter.raise_scope(scope_data);
                    return ret;
                }
            }
            // Raise scope
            interpreter.raise_scope(scope_data);
            Value::Null
        },
        ReturnStmt(ret) => {
            // Return
            eval(interpreter, &**ret)
        },
        IfStmt(cond, body, elsestmt) => {
            // Execute either body or else body
            let condition = eval(interpreter, &**cond);
            if let Value::Error(_) = condition {
                return condition;
            }
            if condition.is_truthy() {
                exec(interpreter, &**body)
            } else {
                // If there isn't a else cond, this will run an empty body
                exec(interpreter, &**elsestmt)
            }
        },
        // Declaring vars
        LetStmt(name, val) => {
            let val = eval(interpreter, &**val);
            if let Value::Error(_) = val {
                return val;
            }
            if interpreter.make_var(name, val, interpreter.is_global, false) {
                Value::Null
            } else {
                Value::Error(format!("cannot redefine variable \"{}\"", name))
            }
        },
        // Declaring functions
        FunctiStmt(name, _args, _body) => {
            interpreter.functions.insert(name.clone(), node.clone());
            Value::Null
        },
        // Loops
        LoopStmt(var, iter, body) => {
            // Make the var if it doesn't exist
            if let Value::Error(_) = interpreter.get_var(var) {
                interpreter.make_var(var, Value::Int(0), interpreter.is_global, false);
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
                    return interpreter.bad_args(&name, args.len(), 2);
                }
                // Get args
                min = eval(interpreter, &args[0]).to_int();
                max = eval(interpreter, &args[1]).to_int();
            } else {
                return Value::Error(
                    "only range is currently supported for iterative loops".to_string()
                );
            }
            // Do the loop
            while min <= max {
                interpreter.set_var(var, Value::Int(min), interpreter.is_global);
                exec(interpreter, &*body.clone());
                min += 1;
            }
            Value::Null
        },
        // While loop
        WhileStmt(cond, body) => {
            // It's really easy, just check cond and loop
            while eval(interpreter, &**cond).is_truthy() {
                exec(interpreter, &*body.clone());
            }
            Value::Null
        },
        // Import
        ImportStmt(name) => {
            // Get file name
            let fname = eval(interpreter, &**name);
            let name = fname.to_string();
            // Get the file path
            let mut path = interpreter.import_path.clone();
            path.push(name.clone());
            // Import
            if import_file(interpreter, &mut path) {
                Value::Null
            } else {
                Value::Error(format!("failed to import {}", name))
            }
        },
        _ => {
            // Expression, not statement
            let val = eval(interpreter, node);
            if let Value::Error(_) = val {
                return val;
            }
            if interpreter.is_repl && val != Value::Null && val != Value::None {
                println!("{}", val.to_string());
            }
            Value::Null
        }
    }
}

pub fn run(interpreter: &mut Interpreter, ast: Vec<ASTNode>) -> bool {
    for node in ast {
        let val = exec(interpreter, &node);
        if let Value::Error(msg) = val {
            println!("RuntimeError: {}", msg);
            return false;
        }
    }
    return true;
}
