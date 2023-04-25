use std::fs::File;
use std::io::Write;
use std::io;

use crate::lexer::TokenType;
use crate::common::IMPOSSIBLE_STATE;
use crate::parser::{ASTNode, ASTNode::*};

struct Formatter {
    file: File,
    indent: usize,
}

#[inline]
fn indent(fmt: &mut Formatter) -> Result<(), io::Error> {
    write!(fmt.file, "{}", "    ".repeat(fmt.indent))?;
    Ok(())
}

fn format_body(fmt: &mut Formatter, node: &ASTNode) -> Result<(), io::Error> {
    let BodyStmt(nodes) = node else {
        if let Nop = node {
            write!(fmt.file, "{{}}")?;
            return Ok(());
        } else {
            panic!("format_body got non-body node!");
        }
    };
    write!(fmt.file, "{{\n")?;
    fmt.indent += 1;
    for node in nodes {
        indent(fmt)?;
        format_stmt(fmt, &node)?;
        write!(fmt.file, "\n")?;
    }
    fmt.indent -= 1;
    indent(fmt)?;
    write!(fmt.file, "}}")?;
    Ok(())
}

fn format_list(
    fmt: &mut Formatter, keys: &Vec<String>, vals: &Vec<ASTNode>
) -> Result<(), io::Error> {
    write!(fmt.file, "[")?;
    if keys.len() == 0 {
        write!(fmt.file, "]")?;
        return Ok(());
    }
    let mut at = 0;
    for key in keys {
        // Print the key
        if key.parse::<i32>().is_err() {
            if let VarExpr(s) = &vals[at] {
                if s != key {
                    write!(fmt.file, "{}: ", key)?;
                }
            } else {
                write!(fmt.file, "{}: ", key)?;
            }
        }
        // Print val
        format_expr(fmt, &vals[at])?;
        // Comma
        at += 1;
        if at != keys.len() {
            write!(fmt.file, ", ")?;
        }
    }
    write!(fmt.file, "]")?;
    Ok(())
}

fn get_prec(op: &TokenType) -> u8 {
    use TokenType::*;
    match op {
        Plus | Minus | Times | Div | Modulo => 1,
        EqualsEquals | NotEquals | Lt | Gt | LtEquals | GtEquals => 2,
        And | Or | Xor => 3,
        Equals | PlusEquals | MinusEquals | TimesEquals | DivEquals => 4,
        _ => 0,
    }
}

fn format_binop(
    fmt: &mut Formatter, rhs: ASTNode, op: TokenType, lhs: ASTNode
) -> Result<(), io::Error> {
    // TODO: Remove uneeded parens
    if let BinopExpr(_,ref rop,_) = rhs {
        if get_prec(&op) < get_prec(rop) {
            write!(fmt.file, "(")?;
            format_expr(fmt, &rhs)?;
            write!(fmt.file, ")")?;
        } else {
            format_expr(fmt, &rhs)?;
        }
    } else {
        format_expr(fmt, &rhs)?;
    }
    write!(fmt.file, " {} ", match op {
        TokenType::Equals => "=",
        TokenType::Plus   => "+",
        TokenType::Minus  => "-",
        TokenType::Times  => "*",
        TokenType::Div    => "/",
        TokenType::Modulo => "%",
        TokenType::PlusEquals  => "+=",
        TokenType::MinusEquals => "-=",
        TokenType::TimesEquals => "*=",
        TokenType::DivEquals   => "/=",
        TokenType::And => "&&",
        TokenType::Or  => "||",
        TokenType::Xor => "^^",
        TokenType::EqualsEquals => "==",
        TokenType::NotEquals    => "!=",
        TokenType::Lt           => "<",
        TokenType::Gt           => ">",
        TokenType::LtEquals     => "<=",
        TokenType::GtEquals     => ">=",
        _ => panic!("{}", IMPOSSIBLE_STATE),
    })?;
    if let BinopExpr(_,ref lop,_) = lhs {
        if get_prec(&op) >= get_prec(lop) {
            write!(fmt.file, "(")?;
            format_expr(fmt, &lhs)?;
            write!(fmt.file, ")")?;
        } else {
            format_expr(fmt, &lhs)?;
        }
    } else {
        format_expr(fmt, &lhs)?;
    }
    Ok(())
}

fn format_expr(fmt: &mut Formatter, node: &ASTNode) -> Result<(), io::Error> {
    match node {
        // Really easy
        NumberExpr(n) => write!(fmt.file, "{}", n)?,
        DecimalExpr(f) => write!(fmt.file, "{:?}", f)?,
        BoolExpr(b) => write!(fmt.file, "{}", b)?,
        NoneExpr => write!(fmt.file, "none")?,
        VarExpr(v) => write!(fmt.file, "{}", v)?,
        BinopExpr(rhs, op, lhs) =>
            format_binop(fmt, *rhs.clone(), op.clone(), *lhs.clone())?,
        ListExpr(keys, vals) => format_list(fmt, keys, vals)?,
        // Bit harder
        StringExpr(str) => {
            if str.contains("\"") {
                write!(fmt.file, "'{}'", str)?;
            } else {
                write!(fmt.file, "\"{}\"", str)?;
            }
        },
        CallExpr(func, args) => {
            write!(fmt.file, "{}(", func)?;
            if args.len() != 0 {
                for arg in &args[..args.len()-1] {
                    format_expr(fmt, &arg)?;
                    write!(fmt.file, ", ")?;
                }
                format_expr(fmt, &args[args.len()-1])?;
            }
            write!(fmt.file, ")")?;
        },
        IndexExpr(lst, index) => {
            format_expr(fmt, &lst)?;
            write!(fmt.file, "[")?;
            format_expr(fmt, &index)?;
            write!(fmt.file, "]")?;
        },
        UnaryExpr(op, node) => {
            write!(fmt.file, "{}", match op {
                TokenType::Minus => "-",
                TokenType::Not => "!",
                TokenType::PlusPlus => "++",
                TokenType::MinusMinus => "--",
                _ => panic!("{}", IMPOSSIBLE_STATE),
            })?;
            // Don't turn - -- into --- because maximal munch
            if let UnaryExpr(TokenType::MinusMinus, _) = *node.clone() {
                if let TokenType::Minus = op {
                    write!(fmt.file, "(")?;
                    format_expr(fmt, &node)?;
                    write!(fmt.file, ")")?;
                } else {
                    format_expr(fmt, &node)?;
                }
            } else {
                format_expr(fmt, &node)?;
            }
        },
        _ => todo!(),
    }
    Ok(())
}

fn format_stmt(fmt: &mut Formatter, node: &ASTNode) -> Result<(), io::Error> {
    match node {
        Nop | BodyStmt(_) => {
            format_body(fmt, node)?;
        },
        FunctiStmt(name, args, body) => {
            write!(fmt.file, "functi {}(", name)?;
            if args.len() != 0 {
                for arg in &args[..args.len()-1] {
                    write!(fmt.file, "{}, ", arg)?;
                }
                write!(fmt.file, "{}) ", args[args.len()-1])?;
            } else {
                write!(fmt.file, ") ")?;
            }
            format_body(fmt, body)?;
            write!(fmt.file, "\n")?;
        },
        IfStmt(cond, yes, no) => {
            write!(fmt.file, "if ")?;
            format_expr(fmt, cond)?;
            write!(fmt.file, " ")?;
            format_body(fmt, yes)?;
            // The else
            if *no.clone() != Nop {
                write!(fmt.file, " else ")?;
                format_stmt(fmt, no)?;
            }
        },
        LetStmt(name, val) => {
            write!(fmt.file, "let {} = ", name)?;
            format_expr(fmt, &val)?;
            write!(fmt.file, ";")?;
        },
        ReturnStmt(val) => {
            write!(fmt.file, "return ")?;
            format_expr(fmt, &val)?;
            write!(fmt.file, ";")?;
        },
        LoopStmt(name, iter, body) => {
            // Copy
            let mut iter = *iter.clone();
            write!(fmt.file, "loop ({} in ", name)?;
            // Unoptimize loops
            if let CallExpr(ref mut name, _) = iter {
                if name == "__burlap_range" {
                    *name = "range".to_string();
                }
            }
            // Iter
            format_expr(fmt, &iter)?;
            write!(fmt.file, ") ")?;
            format_body(fmt, &body)?;
        },
        WhileStmt(cond, body) => {
            write!(fmt.file, "loop (while ")?;
            format_expr(fmt, &cond)?;
            write!(fmt.file, ") ")?;
            format_body(fmt, &body)?;
        },
        ImportStmt(file) => {
            if file.contains("\"") {
                write!(fmt.file, "import ('{}');", file)?;
            } else {
                write!(fmt.file, "import (\"{}\");", file)?;
            }
        },
        _ => {
            format_expr(fmt, node)?;
            write!(fmt.file, ";")?;
        },
    }
    Ok(())
}

#[allow(unused)]
pub fn format(ast: Vec<ASTNode>, filename: String) -> Result<(), io::Error> {
    // Open the file
    let mut fmt = Formatter{
        file: File::create(filename)?,
        indent: 0,
    };
    for node in ast {
        format_stmt(&mut fmt, &node)?;
        write!(fmt.file, "\n")?;
    }
    Ok(())
}
