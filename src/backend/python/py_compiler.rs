// This is Burlap's Sack to Python compiler, it does *not* compile to Python bytecode
use crate::common::IMPOSSIBLE_STATE;
use crate::lexer::TokenType;
use crate::parser::{ASTNode, ASTNode::*, StmtNode, AST, FunctiData, FunctiNode};
use crate::backend::vm::Value;
use crate::backend::vm::vm::Opcode;

use std::io::BufWriter;

pub fn compile_py(
    ast: &mut AST, filename: &Option<String>, output: &mut BufWriter<File>
) -> bool {
    // Header, all python files have this
    output.write("# From: {filename}");
    output.write("# Compiled with Burlap {}", env!("CARGO_PKG_VERSION"));

    // Done!
    output.flush();
    return true;
}
