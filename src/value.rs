use std::cell::RefCell;
use std::rc::Rc;
use std::fs::File;
use std::ops;

#[derive(Debug)]
pub struct FileInfo {
    pub name: String,
    pub mode: i8,
    pub closed: bool,
    pub file: Option<File>
}

impl PartialEq for FileInfo {
    fn eq(&self, _: &FileInfo) -> bool {
        return false;
    }
}

// Value enum for variables
// TODO: Speed up (pointer tagging?)
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Normal values
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Byte(u8),
    List(Vec<(String, Value)>),
    None,
    File(Rc<RefCell<FileInfo>>),

    // FastList (used for lists with only number keys)
    FastList(Vec<Value>),

    // Ptr, used for ffi
    #[cfg(feature = "cffi")]
    Ptr(usize),

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
                    Ok(Value::Float(*f $op *f_right))
                } else if let Value::Int(i_right) = $right {
                    // A float and an int are easier
                    Ok(Value::Float(*f $op (*i_right as f32)))
                } else {
                    $errval
                }
            },
            // Ints
            Value::Int(i) => {
                if let Value::Float(f_right) = $right {
                    // Int and float -> float and float
                    Ok(Value::Float((*i as f32) $op *f_right))
                } else if let Value::Int(i_right) = $right {
                    // Two ints
                    Ok(Value::Int(*i $op *i_right))
                } else {
                    $errval
                }
            },
            // Anything else
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
            Value::Byte(b) => *b as i32,
            #[cfg(feature = "cffi")]
            Value::Ptr(ptr) => *ptr as i32,
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
            Value::Byte(b) => *b as f32,
            _ => 0.0,
        };
    }
    // String conversion
    pub fn to_string(&self) -> Result<String, String> {
        Ok(match self {
            Value::Str(s) => s.clone(),
            Value::Int(i) => format!("{}", i),
            Value::Float(f) => format!("{:?}", f),
            Value::Bool(b) => format!("{}", b),
            Value::Byte(b) => {
                let mut ret = "0b".to_string();
                for mask in (0..8).rev() {
                    ret += if (*b & (1<<mask)) == 0 { "0" } else { "1" };
                }
                ret
            }
            Value::List(l) => {
                let mut ret = "[".to_string();
                // Add each element
                for val in l.iter() {
                    // The the index isn't a number, print the index
                    if !val.0.as_bytes()[0].is_ascii_digit() {
                        ret += &val.0;
                        ret += ": ";
                    }
                    ret += &val.1.to_string()?;
                    ret += ", ";
                }
                // Remove trailing ", "
                if ret.len() != 1 {
                    ret.truncate(ret.len() - 2);
                }
                ret += "]";
                ret
            }
            Value::FastList(l) => {
                let mut ret = "[".to_string();
                // Add each element
                for val in l {
                    ret += &(val.to_string()? + ", ");
                }
                // Remove trailing ", "
                if ret.len() != 1 {
                    ret.truncate(ret.len() - 2);
                }
                ret += "]";
                ret
            }
            Value::None => "none".to_string(),
            // Anything else
            _ => return Err(
                format!("Failed to convert {} to string", self.get_type())
            ),
        })
    }
    // Truthy conversion
    pub fn is_truthy(&self) -> bool {
        return match self {
            Value::Str(s) => !s.is_empty(),
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Bool(b) => *b,
            Value::List(l) => !l.is_empty(),
            Value::FastList(l) => !l.is_empty(),
            #[cfg(feature = "cffi")]
            Value::Ptr(ptr) => *ptr != 0,
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
            Value::Byte(_) => "Byte",
            Value::List(_) | Value::FastList(_) => "List",
            Value::None => "None",
            Value::File(..) => "File",
            // Internal types
            #[cfg(feature = "cffi")]
            Value::Ptr(_) => "__burlap_ptr",
            Value::Iter(..) => "__burlap_iter",
            Value::RangeType(..) => "__burlap_rangetype",
        }.to_string();
    }
    // Lists
    pub fn values(&self) -> Option<Vec<Value>> {
        if let Value::FastList(l) = self {
            return Some(l.clone());
        }
        if let Value::List(l) = self {
            return Some(l.iter().map(|i| i.1.clone()).collect());
        }
        None
    }
    // Iterators
    pub fn to_iter(&self) -> Result<Value, String> {
        if let Value::RangeType(..) | Value::Iter(..) = self {
            return Ok(self.clone());
        }
        if let Value::FastList(list) = self {
            return Ok(Value::Iter(list.clone(), 0));
        }
        if let Value::Str(str) = self {
            return Ok(Value::Iter(
                str.lines().map(|s| Value::Str(s.to_string())).collect(), 0
            ));
        }
        let Value::List(list) = self else {
            return Err(format!("Cannot iterate over {}", self.get_type()));
        };
        return Ok(Value::Iter(
            list.iter().map(|i| i.1.clone()).collect(), 0
        ));
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
    // Contains
    pub fn contains(&self, val: &Value) -> Option<bool> {
        if let Some(vals) = self.values() {
            return Some(vals.iter().any(|i| i.eq(val)));
        } else if let Value::Str(str) = self {
            if let Value::Str(vstr) = val {
                return Some(str.contains(vstr));
            } else if let Value::Byte(byte) = val {
                return Some(str.contains(
                    &(*byte as char).to_string()
                ));
            }
        }
        return None;
    }
    // Indexing
    pub fn index(&self, index: &Value) -> Option<Value> {
        if let Value::Str(str) = self {
            return if let Value::Int(i) = index {
                if *i >= 0 {
                    Some(Value::Str(str.chars().nth(*i as usize)?.to_string()))
                } else {
                    None
                }
            } else {
                None
            }
        } else if let Value::FastList(list) = self {
            // String indexing doesn't work
            return if let Value::Str(_) = index {
                None
            } else {
                list.get(index.to_int() as usize).cloned()
            }
        }
        let Value::List(l) = self else {
            // Not a list
            return None;
        };
        // String indexing (keys)
        if let Value::Str(s) = index {
            for i in l {
                if &i.0 == s {
                    return Some(i.1.clone());
                }
            }
            return None;
        }
        // Number indexing
        return match l.get(index.to_int() as usize) {
            // Extract the value and return
            Some((_, v)) => Some(v.clone()),
            None => None
        };
    }
    // ==
    pub fn eq(&self, right: &Value) -> bool {
        return match self {
            // str == ?
            Value::Str(s) => {
                if let Value::Str(s_right) = right {
                    // str == str
                    s == s_right
                } else {
                    false
                }
            },
            // none == ?
            Value::None => {
                right == &Value::None
            },
            // bool == ?
            Value::Bool(b) => {
                // bool == bool
                if let Value::Bool(b_right) = right {
                    b == b_right
                } else {
                    false
                }
            },
            // byte == ?
            Value::Byte(b) => {
                // byte == byte
                if let Value::Byte(b_right) = right {
                    b == b_right
                } else {
                    false
                }
            },
            // Floats
            Value::Float(f) => {
                if let Value::Float(f_right) = right {
                    // Two floats are easy!
                    f == f_right
                } else if let Value::Int(i_right) = right {
                    // A float and an int also easy
                    *f == *i_right as f32
                } else {
                    false
                }
            },
            // Ints
            Value::Int(i) => {
                if let Value::Float(f_right) = right {
                    // Int and float -> float and float
                    *i as f32 == *f_right
                } else if let Value::Int(i_right) = right {
                    // Two ints
                    i == i_right
                } else {
                    false
                }
            },
            // Pointers
            #[cfg(feature = "cffi")]
            Value::Ptr(p) => {
                if let Value::Ptr(p_right) = right {
                    *p == p_right
                } else {
                    false
                }
            },
            // Lists
            Value::List(_) | Value::FastList(_) => {
                // This isn't spec defined, so may change in the future
                let Some(rhs) = right.values() else {
                    return false;
                };
                let Some(lhs) = self.values() else {
                    return false;
                };
                if lhs.len() != rhs.len() {
                    return false;
                }
                // Compare values
                for ab in lhs.iter().zip(rhs.iter()) {
                    let (a, b) = ab;
                    if !a.eq(&b) {
                        return false;
                    }
                }
                true
            },
            // Anything else
            _ => false,
        };
    }
}

// Add
impl_op_ex!(+ |left: &Value, right: &Value| -> Result<Value, String> {
    // Lists
    if let Value::List(_) = left {
        let Value::List(mut list) = left.clone() else {
            panic!("impossible");
        };
        if let Some(vals) = right.values() {
            // Concat
            for val in vals.clone() {
                list.push((list.len().to_string(), val));
            }
        } else {
            // Append
            list.push((list.len().to_string(), right.clone()));
        }
        return Ok(Value::List(list));
    } else if let Value::FastList(_) = left {
        let Value::FastList(mut list) = left.clone() else {
            panic!("impossible");
        };
        if let Some(mut vals) = right.values() {
            // Concat
            list.append(&mut vals);
        } else {
            // Append
            list.push(right.clone());
        }
        return Ok(Value::FastList(list));
    };
    // Strings
    if let Value::Str(s) = right {
        return Ok(Value::Str(left.to_string()? + &s));
    } else if let Value::Str(s) = left {
        return Ok(Value::Str(s.to_owned() + &right.to_string()?));
    }
    // Anything else
    return do_op!(left, right, +, Err(
        format!("Cannot add {} and {}", left.get_type(), right.get_type())
    ))
});

// Subtract
impl_op_ex!(- |left: &Value, right: &Value| -> Result<Value, String> {
    do_op!(left, right, -,
        Err(format!("Cannot subtract {} and {}", left.get_type(), right.get_type()))
    )
});

// Multiply
impl_op_ex!(* |left: &Value, right: &Value| -> Result<Value, String> {
    return match left {
        // str * number is valid
        Value::Str(s) => {
            if let Value::Int(i_right) = right {
                Ok(if *i_right > 0 {
                    Value::Str((*s).repeat((*i_right).try_into().unwrap()))
                } else {
                    Value::Str("".to_string())
                })
            } else {
                Err(format!(
                    "Cannot multiply {} and {}", left.get_type(), right.get_type()
                ))
            }
        },
        _ => do_op!(left, right, *,
            Err(format!(
                "Cannot multiply {} and {}", left.get_type(), right.get_type()
            ))
        ),
    }
});

// Div
impl_op_ex!(/ |left: &Value, right: &Value| -> Result<Value, String> {
    if let Value::Int(i) = left {
        return &Value::Float(*i as f32) / right;
    }
    do_op!(
        left, right, /,
        Err(format!(
            "Cannot modulo {} and {}",
            left.get_type(),
            right.get_type()
        ))
    )
});

// Modulo
impl_op_ex!(% |left: &Value, right: &Value| -> Result<Value, String> {
    do_op!(
        left, right, %,
        Err(format!(
            "Cannot modulo {} and {}",
            left.get_type(),
            right.get_type()
        ))
    )
});
