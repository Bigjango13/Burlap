// This disassembles the VM's bytecode into a human readable form
use crate::backend::vm::vm::{Opcode, shift2, shift3};
use crate::backend::vm::compiler::Program;
use Opcode::*;

fn decode_op(op: u32) -> (Opcode, u8, u8, u8) {
    return (
        unsafe {
            std::mem::transmute::<u8, Opcode>(((op & 0xFF000000) >> 24).try_into().unwrap())
        },
        ((op & 0xFF0000) >> 16).try_into().unwrap(),
        ((op & 0xFF00) >> 8).try_into().unwrap(),
        (op & 0xFF).try_into().unwrap(),
    );
}

#[inline]
fn dis_binop(name: &str, a: u8, b: u8, c: u8) -> String {
    if a == c {
        format!("{name} r{a}, r{b}")
    } else {
        format!("{name} r{a}, r{b}, r{c}")
    }
}

pub fn dis_single(program: &Program, at: usize) -> String {
    let op = program.ops[at];
    let (op, a, b, c) = decode_op(op);
    match op {
        NOP => "NOP".to_string(),
        POP => "POP".to_string(),
        RET => "RET".to_string(),

        CP => format!("CP r{a}, r{b}"),
        SARG => format!("SARG {a}"),
        CARG => format!("CARG r{a}"),
        CALL => format!("CALL @{}", shift3(a, b, c)),
        VCALL => format!("VCALL r{a}, {b}"),
        RCALL => format!("RCALL @{}", shift3(a, b, c)),
        LV_L => format!("LV(L) {}, r{c}", shift2(a, b)),
        LV_G => format!("LV(G) {}, r{c}", shift2(a, b)),
        SV_L => format!("SV(L) {}, r{c}", shift2(a, b)),
        SV_G => format!("SV(G) {}, r{c}", shift2(a, b)),
        ALO => format!("ALO +{}, r{c}", shift2(a, b)),
        PLC => format!("PLC {}, {c}", shift2(a, b)),
        PGB => format!("PGB {}", shift2(a, b)),
        LFL => format!("LFL r{a}, {}", shift2(a, b)),
        LL => format!("LL r{a}, {}", shift2(a, b)),
        INX => format!("INX r{a}, r{b}, r{c}"),
        ITER => format!("ITER r{a}, r{b}"),
        NXT => format!("NXT r{a}, r{b}, @{}", at + c as usize),
        SKY => format!("SKY r{a}, r{b}, r{c}"),
        NOT => format!("NOT r{a}, r{b}"),
        JMP => format!("JMP @{}", at + shift3(a, b, c)),
        JMPB => format!("JMPB @{}", at - shift3(a, b, c)),
        JMPNT => format!("JMPNT r{a}, @{}", at + shift2(b, c)),

        ADD => dis_binop("ADD", a, b, c),
        SUB => dis_binop("SUB", a, b, c),
        MUL => dis_binop("MUL", a, b, c),
        DIV => dis_binop("DIV", a, b, c),
        MOD => dis_binop("MOD", a, b, c),
        AND => dis_binop("AND", a, b, c),
        XOR => dis_binop("XOR", a, b, c),
        OR => dis_binop("OR", a, b, c),
        EQ => dis_binop("EQ", a, b, c),
        GT => dis_binop("GT", a, b, c),
        LT => dis_binop("LT", a, b, c),
        IN => dis_binop("IN", a, b, c),

        LD => {
            let nsrc = shift2(a, b);
            let src = &program.consts[nsrc];
            format!("LD {:?} (#{nsrc}), r{c}", src)
        },
        LDL => {
            let nsrc = shift3(a, b, c);
            let src = &program.consts[nsrc];
            format!("LDL {:?} (#{nsrc})", src)
        },
    }
}

pub fn dis(program: &Program, start: usize) {
    for i in start..program.ops.len() {
        println!("{i}: {}", dis_single(program, i));
    }
}
