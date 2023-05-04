use std::ffi::{CString, CStr};
use libc;

use libffi::middle::{Arg, Cif, CodePtr, Type};

use crate::common::IMPOSSIBLE_STATE;
use crate::value::Value;

unsafe fn ptr_to_string(input: *mut libc::c_char) -> String {
    CStr::from_ptr(input).to_str().unwrap_or("").to_string()
}

pub fn load_library(libname: String) -> Result<usize, String> {
    // Convert to CString
    let libname = CString::new(libname).unwrap();
    // Open
    let handle = unsafe{ dlopen(libname.as_ptr(), libc::RTLD_NOW) };
    if handle.is_null() {
        // Failed to open
        return unsafe{Err(ptr_to_string(dlerror()))};
    }
    Ok(handle as usize)
}

pub fn load_functi(handle: usize, symname: String) -> Result<usize, String> {
    // Convert to CString
    let symname = CString::new(symname).unwrap();
    // Load symbol
    let sym = unsafe { dlsym(handle as *mut libc::c_void, symname.as_ptr()) };
    if sym.is_null() {
        // Failed to find
        return unsafe{Err(ptr_to_string(dlerror()))};
    }
    Ok(sym as usize)
}

fn get_val_c_type(val: &Value) -> Option<Type> {
    Some(match val {
        // char* and void*
        Value::Str(_) | Value::Ptr(_) => Type::pointer(),
        // Int is i32
        Value::Int(_) => Type::i32(),
        // Float is i32
        Value::Float(_) => Type::f32(),
        // Bool and byte are u8
        Value::Bool(_) | Value::Byte(_) => Type::u8(),
        // Anything else doesn't map
        _ => return None,
    })
}

fn get_str_c_type(val: String) -> Option<Type> {
    Some(match val.as_str() {
        // char* and void*
        "String" | "__burlap_ptr" => Type::pointer(),
        // Int is i32
        "Number" => Type::i32(),
        // Float is f32
        "Decimal" => Type::f32(),
        // Bool and byte are u8
        "Bool" | "Byte" => Type::u8(),
        // Void
        "" | "None" => Type::void(),
        // Anything else doesn't map
        _ => return None,
    })
}

fn val_to_c(val: &Value) -> Result<Arg, CString> {
    return Ok(match val {
        // char* and void*
        Value::Str(ref s) => return Err(CString::new(s.clone()).unwrap()),
        Value::Ptr(ref p) => Arg::new(p),
        // Int is i32
        Value::Int(ref i) => Arg::new(i),
        // Float is f32
        Value::Float(ref f) => Arg::new(f),
        // Bool and byte are u8
        Value::Bool(ref b) => Arg::new(b),
        Value::Byte(ref b) => Arg::new(b),
        // Anything else doesn't map
        _ => panic!("{}", IMPOSSIBLE_STATE),
    });
}

pub fn call(
    ptr: usize, args: Vec<Value>, ret: String
) -> Result<Value, String> {
    // Get the args
    let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());
    let mut c_args: Vec<Arg> = Vec::with_capacity(args.len());
    let mut strings: Vec<CString> = vec![];
    for at in 0..args.len() {
        let arg = args.get(at).unwrap();
        // Get type
        let Some(atype) = get_val_c_type(arg) else {
            return Err(
                format!("Cannot convert {} to C argument", arg.get_type())
            );
        };
        arg_types.push(atype);
        // Convert arg to C equivalent
        match val_to_c(arg) {
            Ok(a) => c_args.push(a),
            Err(cstr) => {
                strings.push(cstr);
                c_args.push(Arg::new(&strings.last().unwrap().as_ptr()));
            }
        }
    }
    // Get the return type
    let Some(ret_t) = get_str_c_type(ret.clone()) else {
        return Err(format!("Invalid C type: {}", ret));
    };
    // Get signature
    let cif = Cif::new(arg_types.into_iter(), ret_t);
    // Call and return
    return Ok(unsafe { match ret.as_str() {
        "String" => {Value::Str(
            ptr_to_string(cif.call(CodePtr(ptr as *mut _), c_args.as_slice()))
        )},
        "__burlap_ptr" => {Value::Ptr(
            cif.call(CodePtr(ptr as *mut _), c_args.as_slice())
        )},
        "Number" => Value::Int(
            cif.call(CodePtr(ptr as *mut _), c_args.as_slice())
        ),
        "Decimal" => Value::Float(
            cif.call(CodePtr(ptr as *mut _), c_args.as_slice())
        ),
        "Bool" => Value::Bool(
            cif.call::<u8>(CodePtr(ptr as *mut _), c_args.as_slice()) == 0
        ),
        "Byte" => Value::Byte(
            cif.call(CodePtr(ptr as *mut _), c_args.as_slice())
        ),
        _ => {
            cif.call::<()>(CodePtr(ptr as *mut _), c_args.as_slice());
            Value::None
        }
    }});
}

extern "C" {
    // dlfcn functions
    fn dlopen(filename: *const libc::c_char, flag: libc::c_int) -> *mut libc::c_void;
    fn dlsym(handle: *mut libc::c_void, symbol: *const libc::c_char) -> *mut libc::c_void;    fn dlerror() -> *mut libc::c_char;
}
