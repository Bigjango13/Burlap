use std::fs::read_to_string;
use std::mem::swap;

use crate::{Arguments, to_ast};
use crate::common::{err, ErrType, IMPOSSIBLE_STATE, get_builtins};
use crate::lexer::{Token, TokenType};
use crate::folding::fold_expr;
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
    BodyStmt(Vec<StmtNode>),
    // Function, (foobar, [a, b, c], Body(...))
    FunctiStmt(FunctiNode),
    // If/else if, (Binop(x == 1), Body(trueBody), Body(falseBody or nop))
    IfStmt(Box<ASTNode>, Box<StmtNode>, Box<StmtNode>),
    // Let, (x, 47)
    LetStmt(Vec<String>, Vec<ASTNode>),
    // Return, ("Return Val")
    ReturnStmt(Box<ASTNode>),
    // Infinite loop
    LoopStmt(Box<StmtNode>),
    // Iter loop, (i, range(0, 100), Body(...), already_defined)
    IterLoopStmt(String, Box<ASTNode>, Box<StmtNode>, bool),
    // While loop, (6 > i, Body(...))
    WhileStmt(Box<ASTNode>, Box<StmtNode>),
    // Break
    BreakStmt,
    // Continue
    ContinueStmt,
    // ImportStmt, used for the file table as the parser handles imports
    ImportStmt,
    // EndImportStmt, (filename), used for marking the end of the import
    EndImportStmt(String),

    // Special
    // Nop, does nothing
    Nop,
}

#[derive(PartialEq, Clone)]
pub struct Variable {
    // Number of times it's been used
    pub name: String,
    pub count: u32,
}

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Var({}, count: {})", self.name, self.count)
    }
}

#[derive(PartialEq, Clone)]
pub struct ExprNode {
    pub node: ASTNode,
    //pub can_fold: bool,
    pub lvalue: bool,
}

impl std::fmt::Debug for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.node)
    }
}

#[derive(PartialEq, Clone)]
pub struct FunctiNode {
    pub name: String,
    pub body: Box<StmtNode>,
}

impl std::fmt::Debug for FunctiNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // FunctiNode is always inside a FunctiStmt, so it doesn't need the wrapper
        write!(f, "{:?}, body: {:?}", self.name, self.body)
    }
}

#[derive(PartialEq, Clone)]
pub struct StmtNode {
    pub node: ASTNode,
    pub line: usize
}

impl std::fmt::Debug for StmtNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.node)
    }
}

fn into_stmt(call: fn(&mut Parser) -> Option<ASTNode>, parser: &mut Parser) -> Option<StmtNode> {
    let line = parser.tokens[parser.at].stream.line;
    Some(StmtNode {
        line, node: call(parser)?
    })
}

#[derive(PartialEq, Clone, Default)]
pub struct FunctiData {
    pub name: String,
    pub arg_num: i32,
    pub locals: Vec<Variable>,
    pub count: i32,
}

impl std::fmt::Debug for FunctiData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{{:?}@{:?}, count: {:?}, locals: {:?}}}", self.name, self.arg_num, self.count, self.locals)
    }
}

pub type VecFunctis = Vec<FunctiData>;

// The AST itself
#[derive(Debug, PartialEq, Clone, Default)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
    pub functis: VecFunctis,
    pub nodes: Vec<StmtNode>,
    pub cur_vars: Vec<Variable>,
    pub all_vars: Vec<Variable>,
}

impl AST {
    pub fn new() -> AST {
        AST {
            functis: vec![],
            cur_vars: vec![],
            all_vars: vec![],
            nodes: vec![]
        }
    }

    pub fn get_functi(&self, name: String) -> Option<&FunctiData> {
        self.functis.iter().find(|&i| i.name == name)
    }

    pub fn get_var_offset(&self, name: String, functi: Option<&FunctiData>) -> Option<u16> {
        let name = name.split("::").nth(1).unwrap_or(&name);
        if let Some(functi) = functi {
            // Local level, find fn first
            &functi.locals
        } else {
            // Global level
            &self.all_vars
        }.iter().position(|i| i.name == name).and_then(|i| u16::try_from(i).ok())
    }

    pub fn add_var(&mut self, var: Variable) {
        self.cur_vars.push(var.clone());
        self.all_vars.push(var);
    }
}

// Parser state
struct Parser {
    tokens: Vec<Token>,
    args: Arguments,
    at: usize,
    in_func: bool,
    in_loop: bool,
    has_err: bool,
    name: String,
    ast: AST,
    functi_locals: Vec<Variable>
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
            $parser.args.extension_color
        );
    );
    // DO NOT USE WITH ErrType::Err
    ($parser:expr, $msg:expr, $err_type:expr) => (
        err(
            &$parser.tokens[$parser.at].stream, $msg, $err_type,
            $parser.args.extension_color
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

fn get_sym(parser: &mut Parser, name: &str, arg_num: i32) -> SymLookupRes {
    // -1 arg_num means name is a variable
    // Check variables
    if let Some(v) = parser.ast.cur_vars.iter_mut().find(|i| i.name == name) {
        // Used again
        v.count += 1;
        return SymLookupRes::TakenByVar;
    }
    // Check functions
    if let Some(f) = parser.ast.functis.iter_mut()
        .find(|i| i.name == name && (arg_num == -1 || arg_num == i.arg_num)) {
        f.count += 1;
        return SymLookupRes::TakenByFuncti;
    }
    // Check builtins
    if get_builtins(&parser.args).iter()
        .any(|(n, a)| n == name && (arg_num == -1 || arg_num == *a))
    {
        return SymLookupRes::TakenByBuiltin;
    }
    return SymLookupRes::Free;
}

fn _check_unique(parser: &mut Parser, name: &str, arg_num: i32) -> Result<bool, Variable> {
    let name = name.split("::").nth(1).unwrap_or(name);
    match get_sym(parser, name, arg_num) {
        SymLookupRes::TakenByVar => return Ok(false),
        SymLookupRes::TakenByFuncti => {error!(
            parser,
            if arg_num == -1 {
                format!("the name \"{}\" is already taken by a function", name)
            } else {
                format!("cannot overload \"{}\" with the same number of args", name)
            }.as_str()
        );},
        SymLookupRes::TakenByBuiltin => {error!(
            parser,
            if arg_num == -1 {
                format!("the name \"{}\" is already taken by a builtin function", name)
            } else {
                format!("cannot overload \"{}\" as it is a builtin function", name)
            }.as_str()
        );},
        SymLookupRes::Free => if arg_num != -1 {
            parser.ast.functis.push(FunctiData {
                name: name.to_string(), arg_num, count: 0, locals: vec![]
            });
        } else {
            return Err(Variable {
                name: name.to_string(),
                count: 0
            });
        }
    };
    return Ok(true);
}

fn check_unique(parser: &mut Parser, name: &str, arg_num: i32) -> Result<bool, Variable>{
    let ret = _check_unique(parser, name, arg_num);
    if let Ok(false) = ret {
        error!(
            parser,
            format!("the name \"{}\" is already taken by a variable", name).as_str()
        );
    }
    ret
}

fn check_name(parser: &mut Parser, name: &str) -> bool {
    let name = name.split("::").nth(1).unwrap_or(name);
    match get_sym(parser, name, -1) {
        SymLookupRes::TakenByVar => true,
        SymLookupRes::TakenByFuncti => false,
        SymLookupRes::TakenByBuiltin => false,
        SymLookupRes::Free => {error!(
            parser,
            format!("\"{}\" is not defined", name).as_str()
        );
        true
        }
    }
}

fn check_call(parser: &mut Parser, name: &str, arg_num: i32) {
    let name = name.split("::").nth(1).unwrap_or(name);
    let mut wrong_args = false;
    // Functions
    for i in &parser.ast.functis {
        if i.name == name {
            if i.arg_num == arg_num {
                // A correct call was found
                return;
            }
            wrong_args = true;
        }
    }
    // Builtins
    for (n, a) in get_builtins(&parser.args) {
        if n == name {
            if *a == arg_num {
                // A correct call was found
                return;
            }
            wrong_args = true;
        }
    }
    // Variables
    if parser.ast.cur_vars.iter().any(|i| i.name == name) {
        // No way to check
        return;
    }
    // No correct call
    // "not defined" errors are reported earlier
    if wrong_args {
        error!(
            parser,
            format!("incorrect number of arguments for \"{}\"", name).as_str()
        );
    }
}

// Expressions
// Calls or indexes
fn parse_callindex_from(parser: &mut Parser, mut ret: ExprNode) -> Option<ExprNode> {
    // Parse call or index
    let is_call = parser.current() == Lparan;
    let is_colon = parser.current() == Colon;
    let (Lparan | Lbracket | Colon) = parser.current() else {
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
            args.push(parse_expr(parser)?.node);
            if let Rparan = parser.current() {
                break;
            }
            eat!(parser, Comma, "expected ')' or ',' in argument list")?;
        }
        if let ASTNode::VarExpr(ref name) = ret.node {
            check_call(parser, name, args.len().try_into().unwrap());
        }
        parser.next();
        ret.node = ASTNode::CallExpr(Box::new(ret.node), args);
        ret.lvalue = false;
    } else if is_colon {
        let Identifier(ident) = parser.current() else {
            error!(parser, "The colon operator requires a identifier!");
            return Option::None;
        };
        parser.next();
        ret.node = ASTNode::IndexExpr(
            Box::new(ret.node), Box::new(ASTNode::StringExpr(ident))
        );
    } else {
        let expr = parse_expr(parser)?.node;
        eat!(parser, Rbracket, "expected ']' at end of index")?;
        ret.node = ASTNode::IndexExpr(Box::new(ret.node), Box::new(expr));
    }
    // Functions and indexes can return functions and lists, so loop
    return parse_callindex_from(parser, ret);
}

fn parse_call_or_index(parser: &mut Parser) -> Option<ExprNode> {
    // Parse base function
    let ret = parse_base_expr(parser)?;
    return parse_callindex_from(parser, ret);
}

// Unary
fn parse_unary(parser: &mut Parser) -> Option<ExprNode> {
    let node = if vec![Minus, Not].contains(&parser.current()) {
        let op = parser.current();
        parser.next();
        ASTNode::UnaryExpr(op, Box::new(parse_unary(parser)?.node))
    } else if vec![PlusPlus, MinusMinus].contains(&parser.current()) {
        let op = parser.current();
        if let Identifier(mut v) = parser.next() {
            check_name(parser, &v);
            parser.next();
            v = parser.name.clone() + "::" + &v;
            ASTNode::UnaryExpr(op, Box::new(ASTNode::VarExpr(v)))
        } else {
            error!(parser, "++/-- require identifiers");
            return Option::None;
        }
    } else {
        return parse_call_or_index(parser);
    };
    return Some(ExprNode { node, lvalue: false });
}

// Binops
// Helper for binops so I don't write the same code more than once
fn parse_binop_helper(
    parser: &mut Parser,
    tokens: Vec<TokenType>,
    callback: &dyn Fn(&mut Parser) -> Option<ExprNode>,
    can_repeat: bool
) -> Option<ExprNode> {
    // Lower precedence op
    let mut expr = callback(parser)?;
    while tokens.contains(&parser.current()) {
        // Left arg
        // Op in the middle
        let op = parser.current();
        parser.next();
        // Right arg
        let right = callback(parser)?.node;
        // Make binop
        expr = ExprNode {
            node: ASTNode::BinopExpr(Box::new(expr.node), op, Box::new(right)),
            lvalue: false
        };
        if !can_repeat {
            return Some(expr);
        }
    }
    return Some(expr);
}

fn parse_binop_math2(parser: &mut Parser) -> Option<ExprNode> {
    // Math 2 binops, *, /, %
    parse_binop_helper(parser, vec![
        Times, Div, Modulo
    ], &parse_unary, true)
}
fn parse_binop_math1(parser: &mut Parser) -> Option<ExprNode> {
    // Math 1 binops, + and -
    parse_binop_helper(parser, vec![
        Plus, Minus,
    ], &parse_binop_math2, true)
}
fn parse_binop_cmp(parser: &mut Parser) -> Option<ExprNode> {
    // Compare binops, ==, !=, <, >, <=, >=, in
    parse_binop_helper(parser, vec![
        EqualsEquals, NotEquals, Lt, Gt, LtEquals, GtEquals, In
    ], &parse_binop_math1, true)
}
fn parse_binop_logic(parser: &mut Parser) -> Option<ExprNode> {
    // Logic binops, &&, ||, ^^
    parse_binop_helper(parser, vec![
        And, Or, Xor
    ], &parse_binop_cmp, true)
}

// Simply a wrapper to the highest expression parser and fold
fn parse_expr(parser: &mut Parser) -> Option<ExprNode> {
    let ret = parse_binop_logic(parser)?;
    Some(if let Some(folded) = fold_expr(&ret) {
        //println!("Folded {:?} into {:?}!", ret, folded);
        folded
    } else {
        ret
    })
}

// Special non-nesting binop
fn parse_binop_set(parser: &mut Parser) -> Option<ASTNode> {
    // Setter binops, =, +=, -=, *=, /=
    let ret = parse_expr(parser)?;
    let (Equals | PlusEquals | MinusEquals
        | TimesEquals | DivEquals | ModEquals) = parser.current()
    else {
        // The next token isn't a setter op
        return Some(ret.node);
    };
    // Check that the value is an lvalue
    if ret.lvalue {
        let op = parser.current();
        parser.next();
        Some(ASTNode::BinopExpr(
            Box::new(ret.node), op,
            Box::new(parse_expr(parser)?.node)
        ))
    } else {
        error!(parser, "expected lvalue on left hand side of setter");
        parser.next();
        Option::None
    }
}

// Lists
fn parse_list_item(parser: &mut Parser) -> (String, Option<ASTNode>) {
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
            let var_name = parser.name.clone() + "::" + &name;
            check_name(parser, &var_name);
            return (name, Some(ASTNode::VarExpr(var_name)));
        } else {
            // It's not a named index (`[myvar + 1]`)
            name = "".to_string();
            parser.at -= 1;
        }
    } else if let Func(_) = parser.current() {
        // Method
        let Some((method, method_name)) = parse_functi(parser, true) else {
            return ("".to_string(), Option::None);
        };
        return (method_name.to_string(), Some(method.clone()));
    } else {
        // Use number index
        if parser.current() == Colon {
            parser.next();
        }
        name = "".to_string();
    }
    // Parse value
    let val = parse_expr(parser).map(|x| x.node);
    return (name, val);
}
fn parse_list(parser: &mut Parser) -> Option<ASTNode> {
    // Parses a list
    eat!(parser, Lbracket, "expecting [")?;
    // Parse elements
    let mut names: Vec<String> = vec![];
    let mut vals: Vec<ASTNode> = vec![];
    let mut fastlist = true;
    while parser.current() != Rbracket {
        let (name, val) = parse_list_item(parser);
        // Invalid element
        if val.is_none() {
            // Parse until the end of the list so there aren't trailing errors
            while parser.next() != Semicolon {}
            return Option::None;
        }
        // Valid element
        names.push(name.clone());
        if fastlist {
            fastlist = name == "";
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
    }
    eat!(parser, Rbracket, "expecting ]")?;
    return Some(ASTNode::ListExpr(names, vals, fastlist));
}

// Normal expressions
fn parse_base_expr(parser: &mut Parser) -> Option<ExprNode> {
    let mut lvalue = false;
    let node = match parser.current() {
        // Inbuilt type
        Identifier(v) => {
            lvalue = check_name(parser, &v);
            parser.next();
            ASTNode::VarExpr(
                parser.name.clone() + "::" + &v
            )
        },
        Str(s)        => { parser.next(); ASTNode::StringExpr(s)  },
        Int(i)        => { parser.next(); ASTNode::NumberExpr(i)  },
        Float(f)      => { parser.next(); ASTNode::DecimalExpr(f) },
        Bool(b)       => { parser.next(); ASTNode::BoolExpr(b)    },
        None          => { parser.next(); ASTNode::NoneExpr       },
        Byte(b)       => { parser.next(); ASTNode::ByteExpr(b)    },
        // Lists
        Lbracket => parse_list(parser)?,
        // Nested expressions
        Lparan => {
            parser.next();
            let ret = parse_expr(parser)?;
            eat!(parser, Rparan, "expecting )")?;
            return Some(ret);
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
            return Option::None;
        },
        // Unknown
        _ => {
            error!(parser, "expected expression");
            if let Eof = parser.current() {} else {
                parser.next();
            }
            return Option::None;
        }
    };
    return Some(ExprNode {
        lvalue, node
    });
}

// Statements
fn parse_statement(parser: &mut Parser) -> Option<StmtNode> {
    let line = parser.tokens[parser.at].stream.line;
    let node = match parser.current() {
        // Bodies
        Lbrace => parse_body(parser),
        Rbrace => {
            error!(parser, "unmatched '}'");
            parser.next();
            Option::None
        },
        // Functions
        Func(_) => parse_functi(parser, false).map(|i| i.0),
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
        Break => {
            if !parser.in_loop {
                error!(parser, "break outside of loop");
                parser.next();
                Option::None
            } else {
                parser.next();
                eat_semicolon!(parser)?;
                Some(ASTNode::BreakStmt)
            }
        },
        Continue => {
            if !parser.in_loop {
                error!(parser, "continue outside of loop");
                parser.next();
                Option::None
            } else {
                parser.next();
                eat_semicolon!(parser)?;
                Some(ASTNode::ContinueStmt)
            }
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
                Option::None
            } else {
                Some(parse_statement(parser)?.node)
            }
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
    }?;
    // Put the node and line together
    return Some(StmtNode {
        node, line
    });
}

// Bodies
fn parse_body(parser: &mut Parser) -> Option<ASTNode> {
    // Start
    eat!(parser, Lbrace, "expected { to start body")?;
    // Middle
    let mut body: Vec<StmtNode> = vec![];
    let mut err = false;
    let old_len = parser.ast.cur_vars.len();
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
        if stmt.node != ASTNode::Nop {
            body.push(stmt);
        }
    }
    // End
    parser.functi_locals.append(&mut parser.ast.cur_vars.split_off(old_len));
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
    let cond = parse_expr(parser)?.node;
    // Body
    let body = into_stmt(parse_body, parser)?;
    // Else
    let else_body = if let Else = parser.current() {
        match parser.next() {
            If => into_stmt(parse_if, parser),
            _ => into_stmt(parse_body, parser)
        }?
    } else {
        StmtNode { line: parser.tokens[parser.at].stream.line, node: ASTNode::Nop }
    };
    // Nops
    if body.node == ASTNode::Nop && else_body.node == ASTNode::Nop {
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
    let old_len = parser.ast.cur_vars.len();
    let uniq_var = _check_unique(parser, &name, -1);
    parser.next();
    // Obligatory 'in'
    eat!(parser, In, "missing 'in' keyword in loop")?;
    // Iterator
    let mut iter = parse_expr(parser)?.node;
    if let Err(ref var) = uniq_var {
        // Var is now valid
        parser.ast.add_var(var.clone());
    }
    let already_defined = !uniq_var.unwrap_or(true);
    // Range optimization
    if let ASTNode::CallExpr(expr, args) = iter.clone() {
        let name = if let ASTNode::VarExpr(n) = *expr {
            n.clone().split("::").nth(1).unwrap_or(n.as_str()).to_string()
        } else {
            "".to_string()
        };
        if name == *"range" {
            // Arg check
            if args.len() != 2 {
                error!(
                    parser,
                    format!("range takes 2 args, not {}", args.len()).as_str()
                );
                return Option::None;
            }
            // Use the faster range
            iter = ASTNode::CallExpr(
                Box::new(ASTNode::VarExpr("::__burlap_range".to_string())),
                args
            );
        }
    }
    // End parens
    eat!(parser, Rparan, "missing ')' in loop")?;
    // Body
    let body = into_stmt(parse_body, parser)?;
    parser.functi_locals.append(&mut parser.ast.cur_vars.split_off(old_len));
    // Return
    Some(ASTNode::IterLoopStmt(name, Box::new(iter), Box::new(body), already_defined))
}

fn parse_loop_while(parser: &mut Parser) -> Option<ASTNode> {
    // Eat 'while'
    eat!(parser, While, IMPOSSIBLE_STATE)?;
    // Condition
    let cond = parse_expr(parser)?.node;
    // End parens
    eat!(parser, Rparan, "missing ')' in loop")?;
    // Body
    let body = into_stmt(parse_body, parser)?;
    // Return
    return Some(ASTNode::WhileStmt(Box::new(cond), Box::new(body)));
}

fn parse_loop(parser: &mut Parser) -> Option<ASTNode> {
    // Eat loop
    parser.next();
    // No condition
    if parser.current() == Lbrace {
        let old_in_loop = parser.in_loop;
        parser.in_loop = true;
        let body = Box::new(into_stmt(parse_body, parser)?);
        parser.in_loop = old_in_loop;
        return Some(ASTNode::LoopStmt(body));
    }
    // Start parens
    eat!(parser, Lparan, "missing '(' in loop")?;
    // Get the loop type and call the helper
    let old_in_loop = parser.in_loop;
    parser.in_loop = true;
    let ret = if parser.current() == While {
        parse_loop_while(parser)
    } else {
        parse_loop_iter(parser)
    };
    parser.in_loop = old_in_loop;
    return ret;
}

// Imports
fn parse_import(parser: &mut Parser) -> Option<(String, Vec<StmtNode>)> {
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
    let mut new_ast = to_ast(&mut parser.args)?;
    parser.ast.functis.append(&mut new_ast.functis);
    parser.ast.cur_vars.append(&mut new_ast.cur_vars);
    parser.ast.all_vars.append(&mut new_ast.all_vars);
    let name = parser.args.name.clone();
    parser.args.name = old_name;
    parser.args.path = old_path;

    // Semicolon
    eat_semicolon!(parser)?;
    return Some((name, new_ast.nodes));
}

// Variable definition
fn parse_let(parser: &mut Parser) -> Option<ASTNode> {
    let mut names = vec![];
    let mut values = vec![];
    loop {
        if let Semicolon = parser.current() {
            break;
        }
        let (name, value) = _parse_let(parser)?;
        names.push(name);
        values.push(value);
    }
    // Semicolon
    eat_semicolon!(parser)?;
    return Some(ASTNode::LetStmt(names, values));
}

fn _parse_let(parser: &mut Parser) -> Option<(String, ASTNode)> {
    // Eat let or comma
    parser.next();
    // Get var name
    let name: String;
    if let Identifier(n) = parser.current() {
        name = parser.name.clone() + "::" + &n;
    } else {
        error!(parser, "expected variable name");
        return Option::None;
    }
    let uniq_var = check_unique(parser, &name, -1);
    parser.next();
    // Let without value (non-standard)
    if let Semicolon = parser.current() {
        if let Err(var) = uniq_var {
            // Var is now valid
            parser.ast.add_var(var.clone());
        }
        if parser.args.extension_auto_none {
            return Some((name, ASTNode::NoneExpr));
        } else {
            error!(parser, "let must have value");
            error!(
                parser,
                "this can be disabled by the auto none extension (`-use-auto-none`)",
                ErrType::Hint
            );
            parser.next();
            return Option::None;
        }
    }
    // Equals symbol
    eat!(parser, Equals, "expected '=' in variable declaration")?;
    // Let with value
    let value = parse_expr(parser)?.node;
    if let Err(var) = uniq_var {
        // Var is now valid
        parser.ast.add_var(var.clone());
    }
    // Return
    return Some((name, value));
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
        if parser.args.extension_auto_none {
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
    let ret_val = parse_expr(parser)?.node;
    // Semicolon
    eat_semicolon!(parser)?;
    // Return return
    return Some(ASTNode::ReturnStmt(Box::new(ret_val)));
}

// Functions
fn parse_functi(parser: &mut Parser, anon: bool) -> Option<(ASTNode, String)> {
    // Disallow named functions in functions
    if parser.in_func && !anon {
        parser.next();
        error!(parser, "cannot create function in function");
        return Option::None;
    }
    // Eat functi
    parser.next();
    // Name
    let mut name: String;
    if let Identifier(n) = parser.current() {
        name = n;
    } else {
        error!(parser, "expected function name");
        return Option::None;
    }
    parser.next();
    parser.functi_locals = vec![];
    // Args
    eat!(parser, Lparan, "expected '(' at start of argument list")?;
    let mut arg_names: Vec<String> = vec![];
    loop {
        if let Rparan = parser.current() {
            break;
        }
        // Arg name
        if let Identifier(n) = parser.current() {
            if let Err(var) = check_unique(parser, &n, -1) {
                parser.ast.add_var(var.clone());
            }
            arg_names.push(parser.name.clone() + "::" + &n);
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
    // Anonymise the name
    let old_name = name.clone();
    if anon {
        name = format!("__anon_functi${}", parser.ast.functis.len());
    }
    if let Err(var) = check_unique(parser, &name, arg_names.len().try_into().unwrap()) {
        parser.ast.add_var(var.clone());
    }
    let fn_index = parser.ast.functis.len() - 1;
    // Body
    if let Lbrace = parser.current() {} else {
        error!(parser, "expected '{' to start function body");
        err(
            &parser.tokens[parser.at].stream,
            "forward declaration isn't supported", ErrType::Hint,
            parser.args.extension_color
        );
        return Option::None;
    }
    let old_in = parser.in_func;
    parser.in_func = true;
    let body = into_stmt(parse_body, parser);
    parser.in_func = old_in;
    parser.functi_locals.append(&mut parser.ast.cur_vars.split_off(parser.ast.cur_vars.len() - arg_names.len()));
    swap(
        &mut parser.ast.functis[fn_index].locals,
        &mut parser.functi_locals
    );
    // Return
    return Some((ASTNode::FunctiStmt(FunctiNode {
        name,
        body: Box::new(body?),
    }), old_name));
}

// Main parsing
pub fn parse(ast: AST, tokens: Vec<Token>, args: &Arguments) -> Option<AST> {
    if tokens.is_empty() {
        return Some(ast);
    }
    // Set up
    let mut parser = Parser {
        tokens, args: args.clone(),
        at: 0, has_err: false, ast,
        in_loop: false, in_func: false,
        name: args.name.clone(),
        functi_locals: vec![],
    };
    // Parse
    while parser.current() != Eof {
        // Import must be highest scope
        if parser.current() == Import {
            let line = parser.tokens[parser.at].stream.line;
            if let Some((path, mut imported_ast)) = parse_import(&mut parser) {
                parser.ast.nodes.push(StmtNode{node: ASTNode::ImportStmt, line});
                parser.ast.nodes.append(&mut imported_ast);
                parser.ast.nodes.push(StmtNode{node: ASTNode::EndImportStmt(path), line});
            } else {
                parser.has_err = true;
            };
            continue;
        }
        if let Some(stmt) = parse_statement(&mut parser) {
            parser.ast.nodes.push(stmt);
            continue;
        };
        // Skip along until EOF/;/{ so the errors don't go crazy
        loop {
            if let Eof | Semicolon | Lbrace = parser.current() {
                break;
            }
            parser.next();
        }
    }
    // Return
    if parser.has_err {
        return Option::None;
    }
    return Some(parser.ast);
}
