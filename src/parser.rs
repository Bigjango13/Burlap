use std::fs::read_to_string;
use crate::{Arguments, to_ast};
use crate::common::{err, ErrType, IMPOSSIBLE_STATE, get_builtins};
use crate::lexer::{Token, TokenType};
use TokenType::*;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    // Expressions
    // String, ("Example")
    StringExpr(String),
    // Number, (47)
    NumberExpr(i32),
    // Float, (3.14)
    DecimalExpr(f32),
    // Bool, (false)
    BoolExpr(bool),
    // None
    NoneExpr,
    // Byte, (0b00000000)
    ByteExpr(u8),
    // Var, (x)
    VarExpr(String),
    // Call, (print, [Number(7)])
    CallExpr(Box<ASTNode>, Vec<ASTNode>),
    // Index, (mylist, Number(7))
    IndexExpr(Box<ASTNode>, Box<ASTNode>),
    // Unary, (Minus, Number(1))
    UnaryExpr(TokenType, Box<ASTNode>),
    // Binop, (Number(2), "+", Number(2))
    BinopExpr(Box<ASTNode>, TokenType, Box<ASTNode>),
    // List (keys["1", "e"], values[node, node], fast = false)
    ListExpr(Vec<String>, Vec<ASTNode>, bool),

    // Statements
    // Body, ([Call(Var(print), [String("Hello World")])])
    BodyStmt(Vec<ASTNode>),
    // Function, (foobar, [a, b, c], Body(...))
    FunctiStmt(String, Vec<String>, Box<ASTNode>),
    // If/else if, (Binop(x == 1), Body(trueBody), Body(falseBody or nop))
    IfStmt(Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    // Let, (x, 47)
    LetStmt(String, Box<ASTNode>),
    // Return, ("Return Val")
    ReturnStmt(Box<ASTNode>),
    // Iter loop, (i, range(0, 100), Body(...))
    LoopStmt(String, Box<ASTNode>, Box<ASTNode>),
    // While loop, (6 > i, Body(...))
    WhileStmt(Box<ASTNode>, Box<ASTNode>),
    // ImportStmt, used for the file table as the parser handles imports
    ImportStmt(),
    // EndImportStmt, (filename), used for marking the end of the import
    EndImportStmt(String),

    // Special
    // Nop, does nothing
    Nop,
}

// Parser state
pub type VecFunctis = Vec<(String, i32)>;
struct Parser {
    tokens: Vec<Token>,
    ast: Vec<ASTNode>,
    functis: VecFunctis,
    vars: Vec<String>,
    args: Arguments,
    at: usize,
    in_func: bool,
    has_err: bool,
    name: String
}

impl Parser {
    // Current token
    fn current(&mut self) -> TokenType {
        self.tokens[self.at].token.clone()
    }

    // Advance and return token
    fn next(&mut self) -> TokenType {
        self.at += 1;
        self.current()
    }
}

// Error macro
macro_rules! error {
    ($parser:expr, $msg:expr) => (
        $parser.has_err = true;
        err(
            &$parser.tokens[$parser.at].stream, $msg, ErrType::Err,
            $parser.args.extensions.contains(&"color".to_string())
        );
    );
    // DO NOT USE WITH ErrType::Err
    ($parser:expr, $msg:expr, $err_type:expr) => (
        err(
            &$parser.tokens[$parser.at].stream, $msg, $err_type,
            $parser.args.extensions.contains(&"color".to_string())
        );
    )
}
// Eat the token or error
macro_rules! eat {
    ($parser:expr, $tok:pat, $msg:expr) => (
        if let $tok = $parser.current() {
            $parser.next();
            Some(())
         } else {
            error!($parser, $msg);
            Option::None
         }
    );
}
// Semicolons eating macro
macro_rules! eat_semicolon {
    ($parser:expr) => (
        eat!($parser, Semicolon, "expected semicolon")
    );
}

// Var/func checking
enum SymLookupRes {
    TakenByVar,
    TakenByFuncti,
    TakenByBuiltin,
    Free,
}

fn get_sym(parser: &mut Parser, name: &String, arg_num: i32) -> SymLookupRes {
    // -1 arg_num means name is a variable
    // Check variables
    if parser.vars.iter().any(|n| n == name) {
        return SymLookupRes::TakenByVar;
    }
    // Check functions
    if parser.functis.iter()
        .any(|(n, a)| n == name && (arg_num == -1 || arg_num == *a)) {
        return SymLookupRes::TakenByFuncti;
    }
    // Check builtins
    let extended = parser.args.extensions.contains(&"burlap-extensions".to_string());
    if get_builtins(extended).iter()
        .any(|(n, a)| n == name && (arg_num == -1 || arg_num == *a))
    {
        return SymLookupRes::TakenByBuiltin;
    }
    return SymLookupRes::Free;
}

fn _check_unique(parser: &mut Parser, name: &String, arg_num: i32) -> bool {
    let name = &name.clone().split("::").nth(1).unwrap_or(name.as_str()).to_string();
    match get_sym(parser, name, arg_num) {
        SymLookupRes::TakenByVar => return false,
        SymLookupRes::TakenByFuncti => {error!(
            parser,
            if arg_num == -1 {
                format!("the name \"{}\" is already taken by a function", *name)
            } else {
                format!("cannot overload \"{}\" with the same number of args", *name)
            }.as_str()
        );},
        SymLookupRes::TakenByBuiltin => {error!(
            parser,
            if arg_num == -1 {
                format!("the name \"{}\" is already taken by a builtin function", *name)
            } else {
                format!("cannot overload \"{}\" as it is a builtin function", *name)
            }.as_str()
        );},
        SymLookupRes::Free => if arg_num != -1 {
            parser.functis.push((name.clone(), arg_num));
        } else {
            parser.vars.push(name.clone());
        }
    };
    return true;
}

fn check_unique(parser: &mut Parser, name: &String, arg_num: i32) {
    if !_check_unique(parser, name, arg_num) {
        error!(
            parser,
            format!("the name \"{}\" is already taken by a variable", *name).as_str()
        );
    }
}

fn check_name(parser: &mut Parser, name: &String) {
    let name = &name.clone().split("::").nth(1).unwrap_or(name.as_str()).to_string();
    match get_sym(parser, name, -1) {
        SymLookupRes::TakenByVar => (),
        SymLookupRes::TakenByFuncti => (),
        SymLookupRes::TakenByBuiltin => (),
        SymLookupRes::Free => {error!(
            parser,
            format!("\"{}\" is not defined", *name).as_str()
        );}
    };
}

fn check_call(parser: &mut Parser, name: &String, arg_num: i32) {
    let name = &name.clone().split("::").nth(1).unwrap_or(name.as_str()).to_string();
    let mut wrong_args = false;
    // Functions
    for (n, a) in &parser.functis {
        if n == name {
            if *a == arg_num {
                // A correct call was found
                return;
            }
            wrong_args = true;
        }
    }
    // Builtins
    let extended = parser.args.extensions.contains(&"burlap-extensions".to_string());
    for (n, a) in get_builtins(extended) {
        if n == name {
            if *a == arg_num {
                // A correct call was found
                return;
            }
            wrong_args = true;
        }
    }
    // Variables
    if parser.vars.iter().any(|n| n == name) {
        // No way to check
        return;
    }
    // No correct call
    // "not defined" errors are reported earlier
    if wrong_args {
        error!(
            parser,
            format!("incorrect number of arguments for \"{}\"", *name).as_str()
        );
    }
}

// Expressions
// Calls or indexes
fn parse_callindex_from(parser: &mut Parser, mut ret: ASTNode) -> Option<ASTNode> {
    // Parse call
    let is_call = parser.current() == Lparan;
    let (Lparan | Lbracket) = parser.current() else {
        // Not a call or index
        return Some(ret);
    };
    // Eat '('/'['
    parser.next();
    // Get args/index
    if is_call {
        let mut args: Vec<ASTNode> = vec![];
        loop {
            if let Rparan = parser.current() {
                break;
            }
            args.push(parse_expr(parser)?);
            if let Rparan = parser.current() {
                break;
            }
            eat!(parser, Comma, "expected ')' or ',' in argument list")?;
        }
        if let ASTNode::VarExpr(ref name) = ret {
            check_call(parser, name, args.len().try_into().unwrap());
        }
        parser.next();
        ret = ASTNode::CallExpr(Box::new(ret), args);
    } else {
        let expr = parse_expr(parser)?;
        eat!(parser, Rbracket, "expected ']' at end of index")?;
        ret = ASTNode::IndexExpr(Box::new(ret), Box::new(expr));
    }
    // Functions and indexes can return functions and lists, so loop
    return parse_callindex_from(parser, ret);
}

fn parse_call_or_index(parser: &mut Parser) -> Option<ASTNode> {
    // Parse base function
    let ret = parse_base_expr(parser)?;
    return parse_callindex_from(parser, ret);
}

// Unary
fn parse_unary(parser: &mut Parser) -> Option<ASTNode> {
    if vec![Minus, Not].contains(&parser.current()) {
        let op = parser.current();
        parser.next();
        return Some(ASTNode::UnaryExpr(op, Box::new(parse_unary(parser)?)));
    }
    if vec![PlusPlus, MinusMinus].contains(&parser.current()) {
        let op = parser.current();
        if let Identifier(mut v) = parser.next() {
            check_name(parser, &v);
            parser.next();
            v = parser.name.clone() + "::" + &v;
            return Some(ASTNode::UnaryExpr(op, Box::new(ASTNode::VarExpr(v))));
        } else {
            error!(parser, "++/-- require identifiers");
            return Option::None;
        }
    }
    return parse_call_or_index(parser);
}

// Binops
// Helper for binops so I don't write the same code more than once
fn parse_binop_helper(
    parser: &mut Parser,
    tokens: Vec<TokenType>,
    callback: &dyn Fn(&mut Parser) -> Option<ASTNode>,
    can_repeat: bool
) -> Option<ASTNode> {
    // Lower precedence op
    let mut expr = callback(parser)?;
    while tokens.contains(&parser.current()) {
        // Left arg
        // Op in the middle
        let op = parser.current();
        parser.next();
        // Right arg
        let right = callback(parser)?;
        // Make binop
        expr = ASTNode::BinopExpr(Box::new(expr), op, Box::new(right));
        if !can_repeat {
            return Some(expr);
        }
    }
    return Some(expr);
}

fn parse_binop_math2(parser: &mut Parser) -> Option<ASTNode> {
    // Math 2 binops, *, /, %
    parse_binop_helper(parser, vec![
        Times, Div, Modulo
    ], &parse_unary, true)
}
fn parse_binop_math1(parser: &mut Parser) -> Option<ASTNode> {
    // Math 1 binops, + and -
    parse_binop_helper(parser, vec![
        Plus, Minus,
    ], &parse_binop_math2, true)
}
fn parse_binop_cmp(parser: &mut Parser) -> Option<ASTNode> {
    // Compare binops, ==, !=, <, >, <=, >=, in
    parse_binop_helper(parser, vec![
        EqualsEquals, NotEquals, Lt, Gt, LtEquals, GtEquals, In
    ], &parse_binop_math1, true)
}
fn parse_binop_logic(parser: &mut Parser) -> Option<ASTNode> {
    // Logic binops, &&, ||, ^^
    parse_binop_helper(parser, vec![
        And, Or, Xor
    ], &parse_binop_cmp, true)
}

// Simply a wrapper to the highest expression parser
fn parse_expr(parser: &mut Parser) -> Option<ASTNode> {
    parse_binop_logic(parser)
}

// Special non-nesting binop
fn parse_binop_set(parser: &mut Parser) -> Option<ASTNode> {
    // Setter binops, =, +=, -=, *=, /=
    // Left must start with identifier
    if let Identifier(..) = parser.current() {
        let ret = parse_binop_helper(parser, vec![
            Equals, PlusEquals, MinusEquals, TimesEquals, DivEquals, ModEquals
        ], &parse_binop_logic, false)?;
        let ASTNode::BinopExpr(rhs, op, _) = ret.clone() else {
            return Some(ret);
        };
        // Check that the binop is a setter
        if let Equals | PlusEquals | MinusEquals | TimesEquals
            | DivEquals | ModEquals = op {} else {
            return Some(ret);
        }
        // Check that arms are either Var or Index
        if let ASTNode::VarExpr(..) | ASTNode::IndexExpr(..) = *rhs {
            Some(ret)
        } else {
            error!(parser, "setters must be used on indexes or variables");
            Option::None
        }
    } else {
        parse_binop_logic(parser)
    }
}

// Lists
fn parse_list_item(parser: &mut Parser, at: i32) -> (String, Option<ASTNode>) {
    // Parses a single item in a list
    let mut name: String;
    // Get the key name
    if let Identifier(n) = parser.current() {
        // Use identifier name
        name = n;
        parser.next();
        if let Colon = parser.current() {
            parser.next();
        } else if let Comma | Rbracket = parser.current() {
            // Named indexes don't need values
            return (name.clone(), Some(ASTNode::VarExpr(
                parser.name.clone() + "::" + &name
            )));
        } else {
            // It's not a named index (`[myvar + 1]`)
            name = at.to_string();
            parser.at -= 1;
        }
    } else {
        // Use number index
        name = at.to_string();
    }
    // Parse value
    let val = parse_expr(parser);
    return (name, val);
}
fn parse_list(parser: &mut Parser) -> Option<ASTNode> {
    // Parses a list
    eat!(parser, Lbracket, "expecting [")?;
    // Parse elements
    let mut at: i32 = 0;
    let mut names: Vec<String> = vec![];
    let mut vals: Vec<ASTNode> = vec![];
    let mut fastlist = true;
    while parser.current() != Rbracket {
        let (name, val) = parse_list_item(parser, at);
        // Invalid element
        if val.is_none() {
            // Parse until the end of the list so there aren't trailing errors
            while parser.next() != Semicolon {}
            return Option::None;
        }
        // Valid element
        names.push(name.clone());
        if fastlist {
            fastlist = name.as_bytes()[0].is_ascii_digit();
        }
        vals.push(val?);
        // Eat comma
        if parser.current() == Comma {
            if parser.next() == Rbracket {
                error!(parser, "trailing comma", ErrType::Warn);
            }
        } else if parser.current() == Rbracket {
        } else {
            error!(parser, "expected comma or ']'");
            if parser.current() == Colon {
                error!(parser, "identifiers are used as keys", ErrType::Hint);
            }
            while parser.next() != Semicolon {}
            return Option::None;
        }
        // Increment index
        at += 1;
    }
    eat!(parser, Rbracket, "expecting ]")?;
    return Some(ASTNode::ListExpr(names, vals, fastlist));
}

// Normal expressions
fn parse_base_expr(parser: &mut Parser) -> Option<ASTNode> {
    return match parser.current() {
        // Inbuilt type
        Identifier(v) => {
            check_name(parser, &v);
            parser.next();
            Some(ASTNode::VarExpr(
                parser.name.clone() + "::" + &v
            ))
        },
        Str(s)        => { parser.next(); Some(ASTNode::StringExpr(s))  },
        Int(i)        => { parser.next(); Some(ASTNode::NumberExpr(i))  },
        Float(f)      => { parser.next(); Some(ASTNode::DecimalExpr(f)) },
        Bool(b)       => { parser.next(); Some(ASTNode::BoolExpr(b))    },
        None          => { parser.next(); Some(ASTNode::NoneExpr)       },
        Byte(b)       => { parser.next(); Some(ASTNode::ByteExpr(b))    },
        // Lists
        Lbracket => parse_list(parser),
        // Nested expressions
        Lparan => {
            parser.next();
            let ret = parse_expr(parser)?;
            eat!(parser, Rparan, "expecting )")?;
            Some(ret)
        },
        // Operators in the wrong spot
        EqualsEquals | NotEquals | Lt | Gt | LtEquals | GtEquals
        | PlusEquals | MinusEquals | TimesEquals | DivEquals
        | Plus | Minus | Times | Div | Modulo | And | Or | Xor
        | PlusPlus | MinusMinus | Not | ModEquals => {
            error!(
                parser,
                "invalid use of operator, did you forgot something before/after?"
            );
            parser.next();
            Option::None
        },
        // Unknown
        _ => {
            error!(parser, "expected expression");
            if let Eof = parser.current() {} else {
                parser.next();
            }
            Option::None
        }
    };
}

// Statements
fn parse_statement(parser: &mut Parser) -> Option<ASTNode> {
    return match parser.current() {
        // Bodies
        Lbrace => parse_body(parser),
        Rbrace => {
            error!(parser, "unmatched '}'");
            parser.next();
            Option::None
        },
        // Functions
        Func(_) => parse_functi(parser),
        // Var def
        Let => parse_let(parser),
        // Loop
        Loop => parse_loop(parser),
        While => {
            error!(parser, "invalid while loop syntax");
            error!(
                parser, "try changing it to `loop (while ...) { ... }`", ErrType::Hint
            );
            parser.next();
            Option::None
        },
        // Return
        Return => parse_return(parser),
        // If
        If => parse_if(parser),
        Else => {
            error!(parser, "missing previous if statement");
            parser.next();
            Option::None
        },
        // Imports
        Import => {
            error!(parser, "imports must be at highest level");
            parser.next();
            Option::None
        },
        // Semicolons
        Semicolon => {
            if parser.next() == Eof {
                return Option::None;
            };
            parse_statement(parser)
        }
        // EOF
        Eof => {
            error!(parser, "reached EOF too soon!");
            Option::None
        },
        // Expressions
        _ => {
            let ret = parse_binop_set(parser)?;
            eat_semicolon!(parser)?;
            Some(ret)
        }
    }
}

// Bodies
fn parse_body(parser: &mut Parser) -> Option<ASTNode> {
    // Start
    eat!(parser, Lbrace, "expected { to start body")?;
    // Middle
    let mut body: Vec<ASTNode> = vec![];
    let mut err = false;
    let old_len = parser.vars.len();
    loop {
        // Exit loop on } or EOF
        if let Rbrace | Eof = parser.current() {
            break;
        }
        // Add statement
        let Some(stmt) = parse_statement(parser) else {
            if Eof == parser.current() && !parser.has_err {
                break;
            }
            err = true;
            continue;
        };
        if stmt != ASTNode::Nop {
            body.push(stmt);
        }
    }
    // End
    parser.vars.truncate(old_len);
    eat!(parser, Rbrace, "expected } to end body, not EOF")?;
    if err {
        return Option::None;
    }
    if body.is_empty() {
        // {} is a nop
        return Some(ASTNode::Nop);
    }
    return Some(ASTNode::BodyStmt(body));
}

// If/else if/else
fn parse_if(parser: &mut Parser) -> Option<ASTNode> {
    // Eat if
    parser.next();
    // Condition
    let cond = parse_expr(parser)?;
    // Body
    let body = parse_body(parser)?;
    // Else
    let else_body = if let Else = parser.current() {
        match parser.next() {
            If => parse_if(parser),
            _ => parse_body(parser)
        }?
    } else {
        ASTNode::Nop
    };
    // Nops
    if body == ASTNode::Nop && else_body == ASTNode::Nop {
        return Some(cond);
    }
    // Return
    return Some(ASTNode::IfStmt(
        Box::new(cond), Box::new(body), Box::new(else_body)
    ));
}

// Loops
fn parse_loop_iter(parser: &mut Parser) -> Option<ASTNode> {
    // Name
    let name: String;
    if let Identifier(n) = parser.current() {
        name = parser.name.clone() + "::" + &n;
    } else {
        error!(parser, "expected variable name");
        return Option::None;
    }
    let _ = _check_unique(parser, &name, -1);
    parser.next();
    // Obligatory 'in'
    eat!(parser, In, "missing 'in' keyword in loop")?;
    // Iterator
    let mut iter = parse_expr(parser)?;
    // Range optimization
    if let ASTNode::CallExpr(expr, args) = iter.clone() {
        let name = if let ASTNode::VarExpr(n) = *expr {
            n
        } else {
            "".to_string()
        };
        if name == *"range" {
            if args.len() != 2 {
                // Arg check
                error!(
                    parser,
                    format!("range takes 2 args, not {}", args.len()).as_str()
                );
                return Option::None;
            }
            // Use the faster range
            iter = ASTNode::CallExpr(
                Box::new(ASTNode::VarExpr("__burlap_range".to_string())),
                args
            );
        }
    }
    // End parens
    eat!(parser, Rparan, "missing ')' in loop")?;
    // Body
    let body = parse_body(parser)?;
    // Return
    return Some(ASTNode::LoopStmt(name, Box::new(iter), Box::new(body)));
}

fn parse_loop_while(parser: &mut Parser) -> Option<ASTNode> {
    // Eat 'while'
    eat!(parser, While, IMPOSSIBLE_STATE)?;
    // Condition
    let cond = parse_expr(parser)?;
    // End parens
    eat!(parser, Rparan, "missing ')' in loop")?;
    // Body
    let body = parse_body(parser)?;
    // Return
    return Some(ASTNode::WhileStmt(Box::new(cond), Box::new(body)));
}

fn parse_loop(parser: &mut Parser) -> Option<ASTNode> {
    // Eat loop
    parser.next();
    // Start parens
    eat!(parser, Lparan, "missing '(' in loop")?;
    // Get the loop type and call the helper
    return if parser.current() == While {
        parse_loop_while(parser)
    } else {
        parse_loop_iter(parser)
    };
}

// Imports
fn parse_import(parser: &mut Parser) -> Option<(String, Vec<ASTNode>)> {
    // Eat import
    parser.next();
    // The parens part 1
    eat!(parser, Lparan, "missing '(' in import")?;
    // Import name
    let Str(file) = parser.current() else {
        error!(parser, "import filename must be a constant string");
        return Option::None;
    };
    parser.next();
    // The closing parens
    eat!(parser, Rparan, "missing ')' in import")?;

    // Everything parsed well, now for the tricky part; importing
    let old_name = parser.args.name.clone();
    let old_path = parser.args.path.clone();
    let path = &mut parser.args.path;
    path.pop();
    path.push(file.clone());
    // Try x.sk
    path.set_extension("sk");
    if let Ok(src) = read_to_string(path.to_str().unwrap()) {
        parser.args.source = src;
        parser.args.name = path.clone().into_os_string().into_string().unwrap();
    } else {
        // Try x.sack
        path.set_extension("sack");
        if let Ok(src) = read_to_string(path.to_str().unwrap()) {
            parser.args.source = src;
            parser.args.name = path.clone().into_os_string().into_string().unwrap();
        } else {
            // No such file
            error!(parser, format!("cannot import {}", file).as_str());
            return Option::None;
        }
    }

    // Return
    let ret = to_ast(&mut parser.args, Some(&mut parser.functis))?;
    let name = parser.args.name.clone();
    parser.args.name = old_name;
    parser.args.path = old_path;

    // Semicolon
    eat_semicolon!(parser)?;
    return Some((name, ret));
}

// Variable definition
fn parse_let(parser: &mut Parser) -> Option<ASTNode> {
    // Eat let
    parser.next();
    // Get var name
    let name: String;
    if let Identifier(n) = parser.current() {
        name = parser.name.clone() + "::" + &n;
    } else {
        error!(parser, "expected variable name");
        return Option::None;
    }
    check_unique(parser, &name, -1);
    parser.next();
    // Let without value (non-standard)
    if let Semicolon = parser.current() {
        parser.next();
        if parser.args.extensions.contains(&"auto-none".to_string()) {
            return Some(ASTNode::LetStmt(name, Box::new(ASTNode::NoneExpr)));
        } else {
            error!(parser, "let must have value");
            error!(
                parser,
                "this can be disabled by the auto none extension (`-use-auto-none`)",
                ErrType::Hint
            );
            return Option::None;
        }
    }
    // Equals symbol
    eat!(parser, Equals, "expected '=' in variable declaration")?;
    // Let with value
    let value = parse_expr(parser)?;
    // Semicolon
    eat_semicolon!(parser)?;
    // Return
    return Some(ASTNode::LetStmt(name, Box::new(value)));
}

// Returning
fn parse_return(parser: &mut Parser) -> Option<ASTNode> {
    // Function check
    if !parser.in_func {
        error!(parser, "return outside of functions");
        parser.next();
        return Option::None;
    }
    // Eat return
    parser.next();
    // Return without value (non-standard)
    if let Semicolon = parser.current() {
        parser.next();
        if parser.args.extensions.contains(&"auto-none".to_string()) {
            return Some(ASTNode::ReturnStmt(Box::new(ASTNode::NoneExpr)));
        } else {
            error!(parser, "return must have value");
            error!(
                parser,
                "this can be disabled by the auto none extension (`-use-auto-none`)",
                ErrType::Hint
            );
            return Option::None;
        }
    }
    // Returns with a value
    let ret_val = parse_expr(parser)?;
    // Semicolon
    eat_semicolon!(parser)?;
    // Return return
    return Some(ASTNode::ReturnStmt(Box::new(ret_val)));
}

// Functions
fn parse_functi(parser: &mut Parser) -> Option<ASTNode> {
    // Disallow functions in functions
    if parser.in_func {
        parser.next();
        error!(parser, "cannot create function in function");
        return Option::None;
    }
    // Eat functi
    parser.next();
    // Name
    let name: String;
    if let Identifier(n) = parser.current() {
        name = n;
    } else {
        error!(parser, "expected function name");
        return Option::None;
    }
    parser.next();
    // Args
    eat!(parser, Lparan, "expected '(' at start of argument list")?;
    let mut args: Vec<String> = vec![];
    loop {
        if let Rparan = parser.current() {
            break;
        }
        // Arg name
        if let Identifier(n) = parser.current() {
            check_unique(parser, &n, -1);
            args.push(parser.name.clone() + "::" + &n);
            parser.next();
        } else {
            error!(parser, "expected argument name");
            return Option::None;
        }
        // Comma for new args or rparan for end of args
        if let Rparan | Comma = parser.current() {} else {
            error!(parser, "expected comma or ')' in argument list");
            return Option::None;
        }
        // Eat comma
        if let Comma = parser.current() {
            parser.next();
        }
    }
    parser.next();
    check_unique(parser, &name, args.len().try_into().unwrap());
    // Body
    if let Lbrace = parser.current() {} else {
        error!(parser, "expected '{' to start function body");
        err(
            &parser.tokens[parser.at].stream,
            "forward declaration isn't supported", ErrType::Hint,
            parser.args.extensions.contains(&"color".to_string())
        );
        return Option::None;
    }
    parser.in_func = true;
    let body = parse_body(parser);
    parser.in_func = false;
    parser.vars.truncate(parser.vars.len() - args.len());
    // Return
    return Some(ASTNode::FunctiStmt(name, args, Box::new(body?)));
}

// Main parsing
pub fn parse(
    tokens: Vec<Token>,
    args: &Arguments
) -> Option<(Vec<ASTNode>, VecFunctis)> {
    let res = _parse(tokens, args, vec![], vec![])?;
    return Some((res.0, res.1));
}

pub fn _parse(
    tokens: Vec<Token>,
    args: &Arguments,
    functis: VecFunctis,
    vars: Vec<String>
) -> Option<(Vec<ASTNode>, VecFunctis, Vec<String>)> {
    if tokens.is_empty() {
        return Some((vec![], vec![], vec![]));
    }
    // Set up
    // TODO: Line numbers
    let mut parser = Parser{
        tokens, args: args.clone(), functis, vars,
        at: 0, has_err: false, in_func: false,
        ast: vec![], name: args.name.clone(),
    };
    // Parse
    while parser.current() != Eof {
        // Import must be highest scope
        if parser.current() == Import {
            if let Some((path, mut imported_ast)) = parse_import(&mut parser) {
                parser.ast.push(ASTNode::ImportStmt());
                parser.ast.append(&mut imported_ast);
                parser.ast.push(ASTNode::EndImportStmt(path));
            } else {
                parser.has_err = true;
            };
            continue;
        }
        if let Some(stmt) = parse_statement(&mut parser) {
            parser.ast.push(stmt);
            continue;
        };
        // Skip along until EOF/; so the errors don't go crazy
        loop {
            if let Eof | Semicolon = parser.current() {
                break;
            }
            parser.next();
        }
    }
    // Return
    if parser.has_err {
        return Option::None;
    }
    return Some((parser.ast, parser.functis, parser.vars));
}
