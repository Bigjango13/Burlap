use std::ops;

// Value enum for varibles
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Errors
    Error(String),
    // Normal values
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    None,
    // Null (the diffrence between, no return and returning none)
    Null
}
// Helper for ops
macro_rules! do_op {
    ($left:expr, $right:expr, $op:tt, $errval:expr) => {
        match $left {
            // Floats
            Value::Float(f) => {
                if let Value::Float(f_right) = $right {
                    // Two floats are easy!
                    Value::Float(f $op &f_right)
                } else if let Value::Int(i_right) = $right {
                    // A float and an int are easier
                    Value::Float(f $op &(i_right as f32))
                } else {
                    $errval
                }
            },
            // Ints
            Value::Int(i) => {
                if let Value::Float(f_right) = $right {
                    // Int and float -> float and float
                    Value::Float((i as f32) $op f_right)
                } else if let Value::Int(i_right) = $right {
                    // Two ints
                    Value::Int(i $op &i_right)
                } else {
                    $errval
                }
            },
            // Strings, bools, and nones aren't usable in ops
            // They must be handled separately
            _ => {
                $errval
            },
        }
    }
}

// Methods
impl Value {
    // To int conversion
    pub fn to_int(&self) -> i32 {
        return match self {
            Value::Str(s) => s.parse::<i32>().unwrap_or(0),
            Value::Int(i) => *i,
            Value::Float(f) => *f as i32,
            Value::Bool(b) => if *b { 1 } else { 0 },
            _ => 0,
        };
    }
    // To float conversion
    pub fn to_float(&self) -> f32 {
        return match self {
            Value::Str(s) => s.parse::<f32>().unwrap_or(0.0),
            Value::Int(i) => *i as f32,
            Value::Float(f) => *f,
            Value::Bool(b) => if *b { 1.0 } else { 0.0 },
            _ => 0.0,
        };
    }
    // String conversion
    pub fn to_string(&self) -> String {
        return match self {
            Value::Str(s) => s.clone(),
            Value::Int(i) => format!("{}", i),
            Value::Float(f) => format!("{}", f),
            Value::Bool(b) => format!("{}", b),
            Value::None => "none".to_string(),
            _ => format!("{:?}", self),
        };
    }
    // Truthy converstion
    pub fn is_truthy(&self) -> bool {
        return match self {
            Value::Str(s) => s != "",
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Bool(b) => *b,
            _ => false,
        };
    }
    pub fn eq(&self, right: Value) -> bool {
        return match self {
            // str == ?
            Value::Str(s) => {
                if let Value::Str(s_right) = right {
                    // str == str
                    s == &s_right
                } else { false }
            },
            // none == ?
            Value::None => {
                if let Value::None = right {
                    // none == none
                    true
                } else { false }
            },
            // bool == ?
            Value::Bool(b) => {
                // float == float
                if let Value::Bool(b_right) = right {
                    *b == b_right
                } else { false }
            },
            // Floats
            Value::Float(f) => {
                if let Value::Float(f_right) = right {
                    // Two floats are easy!
                    f == &f_right
                } else if let Value::Int(i_right) = right {
                    // A float and an int also easy
                    f == &(i_right as f32)
                } else {
                    false
                }
            },
            // Ints
            Value::Int(i) => {
                if let Value::Float(f_right) = right {
                    // Int and float -> float and float
                    (*i as f32) == f_right
                } else if let Value::Int(i_right) = right {
                    // Two ints
                    i == &i_right
                } else {
                    false
                }
            },
            // Anything else
            _ => false,
        };
    }
}

// Add
impl_op_ex!(+ |left: Value, right: Value| -> Value {
    return match left {
        // str + anything is a string
        Value::Str(s) => {
            Value::Str(s + &right.to_string())
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // bool and a bool is only false is both are false.
                Value::Bool(b || b_right)
            } else {
                // bool is converted to an int
                    Value::Int(b as i32) * right
            }
        },
        Value::None => Value::None,
        _ => do_op!(left, right, +, Value::Error("addition failed".to_string())),
    }
});

// Subtract
impl_op_ex!(- |left: Value, right: Value| -> Value {
    return match left {
        // str - anything is invalid
        Value::Str(_) => {
            Value::Error("cannot subtract from string".to_string())
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // Bools and a bool
                Value::Bool(b != b_right)
            } else {
                // bool is converted to an int
                Value::Int(b as i32) - right
            }
        },
        Value::None => Value::None,
        _ => do_op!(left, right, -, Value::None),
    }
});

// Multiply
impl_op_ex!(* |left: Value, right: Value| -> Value {
    return match left {
        // str * number is valid
        Value::Str(s) => {
            if let Value::Int(i_right) = right {
                if i_right > 0 {
                    Value::Str(s.repeat(i_right.try_into().unwrap()))
                } else {
                    Value::Str("".to_string())
                }
            } else {
                Value::Error("can only multiply string with number".to_string())
            }
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // bool and a bool is only false if both are false.
                Value::Bool(b || b_right)
            } else {
                // bool is converted to an int
                Value::Int(b as i32) * right
            }
        },
        Value::None => Value::None,
        _ => do_op!(left, right, *, Value::Error("multiplication failed".to_string())),
    }
});

// Div
impl_op_ex!(/ |left: Value, right: Value| -> Value {
    return match left {
        // str / anything is invalid
        Value::Str(_) => {
            Value::Error("cannot divide string".to_string())
        },
        // none / anything is none
        Value::Bool(b) => {
            // bool is converted to an int
            Value::Int(b as i32) / right
        },
        Value::None => Value::None,
        _ => do_op!(left, right, /, Value::Error("division failed".to_string())),
    }
});

// Modulo
impl_op_ex!(% |left: Value, right: Value| -> Value {
    return match left {
        // str % anything is invalid
        Value::Str(_) => {
            Value::Error("cannot modulo string".to_string())
        },
        Value::Bool(b) => {
            // bool is converted to an int
            Value::Int(b as i32) % right
        },
        Value::None => Value::None,
        _ => do_op!(left, right, %, Value::Error("modulo failed".to_string())),
    }
});
