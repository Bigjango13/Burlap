use std::ffi;
use libc;

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

extern "C" {
    // dlfcn functions
    fn dlopen(filename: *const libc::c_char, flag: libc::c_int) -> *mut libc::c_void;
    fn dlsym(handle: *mut libc::c_void, symbol: *const libc::c_char) -> *mut libc::c_void;
    fn dlerror() -> *mut libc::c_char;
}
