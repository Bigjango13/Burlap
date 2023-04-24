use std::ops;
use indexmap::map::IndexMap;

// Value enum for variables
// TODO: Speed up (pointer tagging?)
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Normal values
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    List(IndexMap<String, Value>),
    None,

    // Iterator (used for iter based loops)
    Iter(Vec<Value>, i32),
    // RangeType (used for optimized ranges)
    RangeType(i32, i32, i32),
}
// Helper for ops
macro_rules! do_op {
    ($left:expr, $right:expr, $op:tt, $errval:expr) => {
        match $left {
            // Floats
            Value::Float(f) => {
                if let Value::Float(f_right) = $right {
                    // Two floats are easy!
                    Ok(Value::Float(f $op &f_right))
                } else if let Value::Int(i_right) = $right {
                    // A float and an int are easier
                    Ok(Value::Float(f $op &(i_right as f32)))
                } else {
                    $errval
                }
            },
            // Ints
            Value::Int(i) => {
                if let Value::Float(f_right) = $right {
                    // Int and float -> float and float
                    Ok(Value::Float((i as f32) $op f_right))
                } else if let Value::Int(i_right) = $right {
                    // Two ints
                    Ok(Value::Int(i $op &i_right))
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
            Value::Float(f) => format!("{:?}", f),
            Value::Bool(b) => format!("{}", b),
            Value::List(l) => {
                let mut ret = "[".to_string();
                // Add each element
                for val in l.iter() {
                    // The the index isn't a number, print the index
                    if !val.0.as_bytes()[0].is_ascii_digit() {
                        ret += val.0;
                        ret += ": ";
                    }
                    ret += &val.1.to_string();
                    ret += ", ";
                }
                // Remove trailing ", "
                if ret.len() != 1 {
                    ret.truncate(ret.len() - 2);
                }
                ret += "]";
                ret
            }
            Value::None => "none".to_string(),
            // Internal types
            Value::Iter(_, _) => "__burlap_iter".to_string(),
            Value::RangeType(_, _, _) => "__burlap_rangetype".to_string(),
        };
    }
    // Truthy conversion
    pub fn is_truthy(&self) -> bool {
        return match self {
            Value::Str(s) => s != "",
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Bool(b) => *b,
            Value::List(l) => !l.is_empty(),
            _ => false,
        };
    }
    // Type
    pub fn get_type(&self) -> String {
        return match self {
            // Normal types
            Value::Str(_) => "String",
            Value::Int(_) => "Number",
            Value::Float(_) => "Decimal",
            Value::Bool(_) => "Bool",
            Value::List(_) => "List",
            Value::None => "None",
            // Internal types
            Value::Iter(_, _) => "__burlap_iter",
            Value::RangeType(_, _, _) => "__burlap_rangetype",
        }.to_string();
    }
    // Iterators
    pub fn to_iter(&self) -> Result<Value, String> {
        if let Value::RangeType(_, _, _) | Value::Iter(_, _) = self {
            return Ok(self.clone());
        }
        let Value::List(list) = self else {
            return Err(format!("Cannot iterate over {}", self.get_type()));
        };
        return Ok(Value::Iter(list.values().map(|i| i.clone()).collect(), 0));
    }
    pub fn iter_next(&mut self) -> Result<Option<Value>, String> {
        // Must be an iter or rangetype
        if let Value::RangeType(ref mut at, max, step) = self {
            if *step == 0 {
                // End of loop
                return Ok(None);
            }
            if at == max {
                // Final part
                *step = 0;
                return Ok(Some(Value::Int(*at)));
            }
            let ret = Value::Int(*at);
            // Step
            *at += *step;
            return Ok(Some(ret));
        }
        // It's not rangetype, must be an iter
        let Value::Iter(list, ref mut at) = self else {
            return Err(format!(
                "Require __burlap_rangetype or __burlap_iter not {}",
                self.get_type()
            ));
        };
        // Get the value
        let ret = list.get(*at as usize);
        *at += 1;
        // Return it
        if let Some(ret) = ret {
            return Ok(Some(ret.clone()));
        } else {
            return Ok(None);
        }
    }
    // Indexing
    pub fn index(&self, index: Value) -> Option<&Value> {
        let Value::List(l) = self else {
            // Not a list
            return None;
        };
        // String indexing (keys)
        if let Value::Str(s) = index {
          return l.get(&s);
        }
        // Number indexing
        return match l.get_index(index.to_int() as usize) {
            // Remove the key
            Some((_, v)) => Some(v),
            None => None
        };
    }
    // ==
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
                } else {
                    false
                }
            },
            // bool == ?
            Value::Bool(b) => {
                // float == float
                if let Value::Bool(b_right) = right {
                    *b == b_right
                } else {
                    false
                }
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
            // Lists
            Value::List(lhs) => {
                if let Value::List(rhs) = right {
                    // This isn't spec defined, so may change in the future
                    if lhs.len() != rhs.len() {
                        return false;
                    }
                    // Compare values
                    for ab in lhs.values().zip(rhs.values()) {
                        let (a, b) = ab;
                        if !a.eq(b.clone()) {
                            return false;
                        }
                    }
                    true
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
impl_op_ex!(+ |left: Value, right: Value| -> Result<Value, String> {
    return match left {
        // str + anything is a string
        Value::Str(s) => {
            Ok(Value::Str(s + &right.to_string()))
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // bool and a bool is only false is both are false.
                Ok(Value::Bool(b || b_right))
            } else {
                // bool is converted to an int
                Value::Int(b as i32) * right
            }
        },
        Value::None => Ok(Value::None),
        _ => do_op!(left, right, +, Err(
            format!("Cannot add {} and {}", left.get_type(), right.get_type())
        )),
    }
});

// Subtract
impl_op_ex!(- |left: Value, right: Value| -> Result<Value, String> {
    return match left {
        // str - anything is invalid
        Value::Str(_) => {
            Err(format!("Cannot subtract {} and {}", left.get_type(), right.get_type()))
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // Bools and a bool
                Ok(Value::Bool(b != b_right))
            } else {
                // bool is converted to an int
                Value::Int(b as i32) - right
            }
        },
        Value::None => Ok(Value::None),
        _ => do_op!(left, right, -, Ok(Value::None)),
    }
});

// Multiply
impl_op_ex!(* |left: Value, right: Value| -> Result<Value, String> {
    return match left.clone() {
        // str * number is valid
        Value::Str(s) => {
            if let Value::Int(i_right) = right {
                Ok(if i_right > 0 {
                    Value::Str(s.repeat(i_right.try_into().unwrap()))
                } else {
                    Value::Str("".to_string())
                })
            } else {
                Err(format!("Cannot multiply {} and {}", left.get_type(), right.get_type()))
            }
        },
        Value::Bool(b) => {
            if let Value::Bool(b_right) = right {
                // bool and a bool is only false if both are false.
                Ok(Value::Bool(b || b_right))
            } else {
                // bool is converted to an int
                Value::Int(b as i32) * right
            }
        },
        Value::None => Ok(Value::None),
        _ => do_op!(left, right, *,
            Err(format!("Cannot multiply {} and {}", left.get_type(), right.get_type()))
        ),
    }
});

// Div
impl_op_ex!(/ |left: Value, right: Value| -> Result<Value, String> {
    return match left {
        // str / anything is invalid
        Value::Str(_) => {
            Err(format!("Cannot divide {} and {}", left.get_type(), right.get_type()))
        },
        // bool and int are converted to floats
        Value::Bool(b) => {
            Value::Float(b as i32 as f32) / right
        },
        Value::Int(i) => {
            Value::Float(i as f32) / right
        },
        // none / anything is none
        Value::None => Ok(Value::None),
        _ => do_op!(left, right, /,
            Err(format!("Cannot divide {} and {}", left.get_type(), right.get_type()))
        ),
    }
});

// Modulo
impl_op_ex!(% |left: Value, right: Value| -> Result<Value, String> {
    return match left {
        // str % anything is invalid
        Value::Str(_) => {
            Err(format!("Cannot modulo {} and {}", left.get_type(), right.get_type()))
        },
        Value::Bool(b) => {
            // bool is converted to an int
            Value::Int(b as i32) % right
        },
        Value::None => Ok(Value::None),
        _ => do_op!(left, right, %,
            Err(format!("Cannot divide {} and {}", left.get_type(), right.get_type()))
        )
    }
});
