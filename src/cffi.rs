use std::ffi;
use libc;

use libffi::middle::{Arg, Cif, CodePtr, Type};

use crate::common::IMPOSSIBLE_STATE;
use crate::value::Value;

unsafe fn ptr_to_string(input: *mut libc::c_char) -> String {
    ffi::CStr::from_ptr(input).to_str().unwrap_or("").to_string()
}

pub fn load_library(libname: String) -> Result<usize, String> {
    // Convert to CString
    let libname = ffi::CString::new(libname).unwrap();
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
    let symname = ffi::CString::new(symname).unwrap();
    // Load symbol
    let sym = unsafe { dlsym(handle as *mut libc::c_void, symname.as_ptr()) };
    if sym.is_null() {
        // Failed to find
        return unsafe{Err(ptr_to_string(dlerror()))};
    }
    Ok(sym as usize)
}

fn get_c_type(val: &Value) -> Option<Type> {
    Some(match val {
        // char* and void*
        // Value::Str(_) |
        Value::Ptr(_) => Type::pointer(),
        // Int is i32
        Value::Int(_) => Type::i32(),
        // Bool and byte are u8
        Value::Bool(_) | Value::Byte(_) => Type::u8(),
        // Anything else doesn't map
        _ => return None,
    })
}

fn val_to_c(val: &Value) -> Arg {
    match val {
        // char* and void*
        //Value::Str(s) => Arg::new(&s.as_str()),
        Value::Ptr(p) => Arg::new(p),
        // Int is i32
        Value::Int(i) => Arg::new(i),
        // Bool and byte are u8
        Value::Bool(b) => Arg::new(b),
        Value::Byte(b) => Arg::new(b),
        // Anything else doesn't map
        _ => panic!("{}", IMPOSSIBLE_STATE),
    }
}

pub fn call(ptr: usize, args: Vec<Value>) -> Result<(), String> {
    // Get the args
    let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());
    let mut c_args: Vec<Arg> = Vec::with_capacity(args.len());
    for arg in args {
        // Get type
        let Some(atype) = get_c_type(&arg) else {
            return Err(
                format!("Cannot convert {} to C argument", arg.get_type())
            );
        };
        arg_types.push(atype);
        // Convert args to C equivalent
        c_args.push(val_to_c(&arg))
    }
    // Get signature
    let cif = Cif::new(arg_types.into_iter(), Type::void());
    unsafe {
       cif.call::<()>(CodePtr(ptr as *mut _), c_args.as_slice());
    }
    Ok(())
}

extern "C" {
    // dlfcn functions
    fn dlopen(filename: *const libc::c_char, flag: libc::c_int) -> *mut libc::c_void;
    fn dlsym(handle: *mut libc::c_void, symbol: *const libc::c_char) -> *mut libc::c_void;    fn dlerror() -> *mut libc::c_char;
}
