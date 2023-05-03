use std::ffi;
use libc;

use libffi::middle::{Type, Cif, CodePtr};

//use crate::value::Value;

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

/*pub fn get_c_type(val: &Value) -> Option<Type> {
    Some(match val {
        // char* and void*
        Value::Str(_) | Value::Ptr(_) => Type::pointer(),
        // Int is i32
        Value::Int(_) => Type::i32(),
        // Bool and byte are u8
        Value::Bool(_) | Value::Byte(_) => Type::u8(),
        // Anything else doesn't map
        _ => return None,
    })
}*/

pub fn call(ptr: usize) {
    let cif = Cif::new(vec![].into_iter(), Type::void());
    unsafe {
        cif.call::<()>(CodePtr(ptr as *mut _), &[])
    };
}

extern "C" {
    // dlfcn functions
    fn dlopen(filename: *const libc::c_char, flag: libc::c_int) -> *mut libc::c_void;
    fn dlsym(handle: *mut libc::c_void, symbol: *const libc::c_char) -> *mut libc::c_void;
    fn dlerror() -> *mut libc::c_char;
}
