use crate::common::{err, ErrType, get_len, IMPOSSIBLE_STATE};
use crate::lexer::{Token, TokenType};
use TokenType::*;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    // None is the result of a parser error
    // not to be confused with NoneExpr, which is the none type
    None,
    // Expressions
    // String, ("Example")
    StringExpr(String),
    // Number, (42)
    NumberExpr(i32),
    // Float, (3.14)
    DecimalExpr(f32),
    // Bool, (false)
    BoolExpr(bool),
    // None
    NoneExpr,
    // Var, (x)
    VarExpr(String),
    // Call, (print, [Number(7)])
    CallExpr(String, Vec<ASTNode>),
    // Index, (mylist, Number(7))
    IndexExpr(Box<ASTNode>, Box<ASTNode>),
    // Unary, (Minus, Number(1))
    UnaryExpr(TokenType, Box<ASTNode>),
    // Binop, (Number(2), "+", Number(2))
    BinopExpr(Box<ASTNode>, TokenType, Box<ASTNode>),
    // List (keys["1", "e"], values[node, node])
    ListExpr(Vec<String>, Vec<ASTNode>),

    // Statements
    // Body, ([Call(Var(print), [String("Hello Worls")])])
    BodyStmt(Vec<ASTNode>),
    // Function, (foobar, [a, b, c], Body(...))
    FunctiStmt(String, Vec<String>, Box<ASTNode>),
    // If/else if, (Binop(x == 1), Body(trueBody), Body(falseBody or none))
    IfStmt(Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>),
    // Let, (a, 42)
    LetStmt(String, Box<ASTNode>),
    // Return, ("Return Val")
    ReturnStmt(Box<ASTNode>),
    // Iter loop, (i, range(5, 6), Body(...))
    LoopStmt(String, Box<ASTNode>, Box<ASTNode>),
    // While loop, (6 > i, Body(...))
    WhileStmt(Box<ASTNode>, Box<ASTNode>),
    // Import, ("math")
    ImportStmt(Box<ASTNode>)
}

// Parser state
struct ParserState {
    tokens: Vec<Token>,
    extentions: Vec<String>,
    at: usize,
    has_err: bool,
    in_func: bool,
}
// Token helpers
// Current token
fn cur_tok(parser: &ParserState) -> TokenType {
    parser.tokens[parser.at].token.clone()
}
// Advance and return token
fn next_tok(parser: &mut ParserState) -> TokenType {
    parser.at += 1;
    cur_tok(parser)
}
// Error macro
macro_rules! error {
    ($parser:expr, $msg:expr) => (
        $parser.has_err = true;
        err(
            &$parser.tokens[$parser.at].stream, $msg,
            get_len(&$parser.tokens[$parser.at].token), ErrType::Err
        );
    );
    // DO NOT USE WITH ErrType::Err!!!
    ($parser:expr, $msg:expr, $err_type:expr) => (
        err(
            &$parser.tokens[$parser.at].stream, $msg,
            get_len(&$parser.tokens[$parser.at].token), $err_type
        );
    )
}
// Gobble up the yummy token and spit out an error if it tastes bad
macro_rules! eat {
    ($parser:expr, $tok:pat, $msg:expr) => (
        if let $tok = cur_tok(&$parser) {
            next_tok($parser);
            true
         } else {
            error!($parser, $msg);
            false
         }
    );
}
// Semicolons eating macro
macro_rules! eat_semicolon {
    ($parser:expr) => (
        eat!($parser, Semicolon, "expected semicolon")
    )
}

// Expressions
// Call
fn parse_call(parser: &mut ParserState) -> ASTNode {
    // A function can't return a function so no need for a loop
    if let Identifier(_) = cur_tok(parser) {} else {
        // Can't call anything but identifiers
        return parse_expr(parser);
    }
    // Eat
    let mut ret = parse_expr(parser);
    if let ASTNode::None = ret {
        return ASTNode::None;
    }
    let name: String;
    if let ASTNode::VarExpr(n) = &ret {
        name = n.clone();
    } else {
        return ret;
    }
    // Parse call
    let mut args: Vec<ASTNode> = vec![];
    if let Lparan = cur_tok(parser) {
        next_tok(parser);
        loop {
            if let Rparan = cur_tok(parser) {
                break;
            }
            args.push(parse_binop_logic(parser));
            if let Rparan = cur_tok(parser) {
                break;
            }
            if !eat!(parser, Comma, "expected ')' or ',' in argument list") {
                return ASTNode::None;
            }
        }
        next_tok(parser);
        ret = ASTNode::CallExpr(name, args);
    }
    return ret;
}

// Index
fn parse_index(parser: &mut ParserState) -> ASTNode {
    // Get expr
    let mut ret = parse_call(parser);
    if let ASTNode::None = ret {
        return ASTNode::None;
    }
    // Parse index
    while let Lbracket = cur_tok(parser) {
        next_tok(parser);
        let index = parse_binop_logic(parser);
        ret = ASTNode::IndexExpr(Box::new(ret), Box::new(index));
        // Eat ']'
        if !eat!(parser, Rbracket, "expected ']' at end of index") {
            return ASTNode::None;
        }
    }
    return ret;
}

// Unary
fn parse_unary(parser: &mut ParserState) -> ASTNode {
    if vec![Minus, Not].contains(&cur_tok(parser)) {
        let op = cur_tok(parser);
        next_tok(parser);
        return ASTNode::UnaryExpr(op, Box::new(parse_unary(parser)));
    }
    if vec![PlusPlus, MinusMinus].contains(&cur_tok(parser)) {
        let op = cur_tok(parser);
        if let Identifier(v) = next_tok(parser) {
            next_tok(parser);
            return ASTNode::UnaryExpr(op, Box::new(ASTNode::VarExpr(v)));
        } else {
            error!(parser, "++/-- require identifiers", ErrType::Err);
            return ASTNode::None;
        }
    }
    return parse_index(parser);
}

// Binops
// Helper for binops so I don't write the same code more than once
fn parse_binop_helper(
    parser: &mut ParserState,
    tokens: Vec<TokenType>,
    callback: &dyn Fn(&mut ParserState) -> ASTNode,
    can_repeat: bool
) -> ASTNode {
    // Lower precedence op
    let mut expr = callback(parser);
    while tokens.contains(&cur_tok(parser)) {
        // Left arg
        if let ASTNode::None = expr {
            parser.has_err = true;
            return ASTNode::None;
        }
        // Op in the middle
        let op = cur_tok(parser);
        next_tok(parser);
        // Right arg
        let right = callback(parser);
        if let ASTNode::None = right {
            parser.has_err = true;
            return ASTNode::None;
        }
        // Make binop
        expr = ASTNode::BinopExpr(Box::new(expr), op, Box::new(right));
        if !can_repeat {
            return expr;
        }
    }
    return expr;
}

fn parse_binop_math(parser: &mut ParserState) -> ASTNode {
    // Math binops, +, -, *, /, %
    parse_binop_helper(parser, vec![
        Plus, Minus, Times, Div, Modulo
    ], &parse_unary, true)
}
fn parse_binop_cmp(parser: &mut ParserState) -> ASTNode {
    // Compare binops, ==, !=, <, >, <=, >=
    parse_binop_helper(parser, vec![
        EqualsEquals, NotEquals, Lt, Gt, LtEquals, GtEquals
    ], &parse_binop_math, true)
}
fn parse_binop_logic(parser: &mut ParserState) -> ASTNode {
    // Logic binops, &&, ||, ^^
    parse_binop_helper(parser, vec![
        And, Or, Xor
    ], &parse_binop_cmp, true)
}
fn parse_binop_set(parser: &mut ParserState) -> ASTNode {
    // Setter binops, =, +=, -=, *=, /=
    // Left must be identifier
    if let Identifier(_) = cur_tok(parser) {
        parse_binop_helper(parser, vec![
            Equals, PlusEquals, MinusEquals, TimesEquals, DivEquals
        ], &parse_binop_logic, false)
    } else {
        parse_binop_logic(parser)
    }
}

// Lists
fn parse_list_item(parser: &mut ParserState, at: i32) -> (String, ASTNode) {
    // Parses a single item in a list
    let mut name: String;
    // Get the key name
    if let Identifier(n) = cur_tok(parser) {
        // Use identifier name
        name = n;
        next_tok(parser);
        if let Colon = cur_tok(parser) {
            next_tok(parser);
        } else if let Comma | Rbracket = cur_tok(parser) {
            // Named indexes don't need values
            return (name.clone(), ASTNode::VarExpr(name));
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
    let val: ASTNode = parse_binop_logic(parser);
    return (name, val);
}
fn parse_list(parser: &mut ParserState) -> ASTNode {
    // Parses a list
    if !eat!(parser, Lbracket, "expecting [") {
        return ASTNode::None;
    }
    // Parse elements
    let mut at: i32 = 0;
    let mut names: Vec<String> = vec![];
    let mut vals: Vec<ASTNode> = vec![];
    while cur_tok(parser) != Rbracket {
        let (name, val) = parse_list_item(parser, at);
        // Invalid element
        if val == ASTNode::None {
            // Parse until the end of the list so there aren't trailing errors
            while next_tok(parser) != Semicolon {}
            return ASTNode::None;
        }
        // Valid element
        names.push(name);
        vals.push(val);
        // Eat comma
        if cur_tok(parser) == Comma {
            if next_tok(parser) == Rbracket {
                error!(parser, "trailing comma", ErrType::Warn);
            }
        } else if cur_tok(parser) == Rbracket {
        } else {
            error!(parser, "expected comma or ']'");
            if cur_tok(parser) == Colon {
                error!(parser, "identifiers are used as keys", ErrType::Hint);
            }
            while next_tok(parser) != Semicolon {}
            return ASTNode::None;
        }
        // Increment index
        at += 1;
    }
    if !eat!(parser, Rbracket, "expecting ]") {
        return ASTNode::None;
    }
    return ASTNode::ListExpr(names, vals);
}

// Normal expressions
fn parse_expr(parser: &mut ParserState) -> ASTNode {
    return match cur_tok(parser) {
        // Inbuilt type
        Identifier(v) => { next_tok(parser); ASTNode::VarExpr(v)     },
        Str(s)        => { next_tok(parser); ASTNode::StringExpr(s)  },
        Int(i)        => { next_tok(parser); ASTNode::NumberExpr(i)  },
        Float(f)      => { next_tok(parser); ASTNode::DecimalExpr(f) },
        Bool(b)       => { next_tok(parser); ASTNode::BoolExpr(b)    },
        None          => { next_tok(parser); ASTNode::NoneExpr       },
        // Lists
        Lbracket => parse_list(parser),
        // Nested expressions
        Lparan => {
            next_tok(parser);
            let ret = parse_binop_logic(parser);
            if let ASTNode::None = ret {
                return ASTNode::None;
            }
            if eat!(parser, Rparan, "expecting )") {
                ret
            } else {
                ASTNode::None
            }
        },
        // Operators in the wrong spot
        EqualsEquals | NotEquals | Lt | Gt | LtEquals | GtEquals |
        PlusEquals | MinusEquals | TimesEquals | DivEquals |
        Plus | Minus | Times | Div | Modulo |
        PlusPlus | MinusMinus | Not => {
            error!(
                parser,
                "invalid use of operator, did you forgot something before/after?"
            );
            next_tok(parser);
            ASTNode::None
        },
        // Unknown
        _ => {
            error!(parser, "expected expression");
            if let Eof = cur_tok(parser) {} else {
                next_tok(parser);
            }
            ASTNode::None
        }
    };
}

// Statements
fn parse_statement(parser: &mut ParserState) -> ASTNode {
    return match cur_tok(parser) {
        // Bodies
        Lbrace => parse_body(parser),
        Rbrace => {
            error!(parser, "unmatched '}'");
            next_tok(parser);
            ASTNode::None
        },
        // Functions (functi forever!)
        Func => parse_functi(parser),
        // Var def
        Let => parse_let(parser),
        // Loop
        Loop => parse_loop(parser),
        While => {
            error!(parser, "invalid while loop syntax");
            error!(
                parser, "try changing it to `loop (while ...) { ... }`", ErrType::Hint
            );
            next_tok(parser);
            ASTNode::None
        },
        // Import
        Import => parse_import(parser),
        // Return
        Return => parse_return(parser),
        // If
        If => parse_if(parser),
        Else => {
            error!(parser, "missing previous if statement");
            next_tok(parser);
            ASTNode::None
        },
        // Semicolons
        Semicolon => {
            next_tok(parser);
            parse_statement(parser)
        }
        // EOF
        Eof => ASTNode::None,
        // Expressions
        _ => {
            let ret = parse_binop_set(parser);
            if let ASTNode::None = ret {
                return ASTNode::None;
            }
            if !eat_semicolon!(parser) {
                return ASTNode::None;
            }
            ret
        }
    }
}

// Bodies
fn parse_body(parser: &mut ParserState) -> ASTNode {
    // Start
    if !eat!(parser, Lbrace, "expected { to start body") {
        return ASTNode::None;
    }
    // Middle
    let mut body: Vec<ASTNode> = vec![];
    loop {
        // Exit loop on } or EOF
        if let Rbrace | Eof = cur_tok(parser) {
            break;
        }
        // Add statment
        let stmt = parse_statement(parser);
        body.push(stmt);
    }
    // End
    if !eat!(parser, Rbrace, "expected } to end body, not EOF") {
        return ASTNode::None;
    }
    return ASTNode::BodyStmt(body);
}

// If/else if/else
fn parse_if(parser: &mut ParserState) -> ASTNode {
    // Eat if
    next_tok(parser);
    // Condition
    let cond = parse_binop_logic(parser);
    if let ASTNode::None = cond {
        return ASTNode::None;
    }
    // Body
    let body = parse_body(parser);
    if let ASTNode::None = body {
        return ASTNode::None;
    }
    // Else
    let mut else_stmt = ASTNode::BodyStmt(vec![]);
    if let Else = cur_tok(parser) {
        else_stmt = match next_tok(parser) {
            If => parse_if(parser),
            _ => parse_body(parser)
        };
    }
    // Invalid else
    if let ASTNode::None = else_stmt {
        return ASTNode::None;
    }
    // No else
    if let ASTNode::BodyStmt(ref nodes) = else_stmt {
        if nodes.len() == 0 {
            return ASTNode::IfStmt(Box::new(cond), Box::new(body), Option::None);
        }
    }
    // Return
    return ASTNode::IfStmt(
        Box::new(cond), Box::new(body), Some(Box::new(else_stmt))
    );
}

// Loops
fn parse_loop_iter(parser: &mut ParserState) -> ASTNode {
    // Name
    let name: String;
    if let Identifier(n) = cur_tok(parser) {
        name = n;
    } else {
        error!(parser, "expected varible name");
        return ASTNode::None;
    }
    next_tok(parser);
    // Obligatory 'in'
    if !eat!(parser, In, "missing 'in' keyword in loop") {
        return ASTNode::None;
    }
    // Iterator
    let iter = parse_binop_logic(parser);
    if iter == ASTNode::None {
        return ASTNode::None;
    }
    // End parens
    if !eat!(parser, Rparan, "missing ')' in loop") {
        return ASTNode::None;
    }
    // Body
    let body = parse_body(parser);
    if body == ASTNode::None {
        return ASTNode::None;
    }
    // Return
    return ASTNode::LoopStmt(name, Box::new(iter), Box::new(body));
}

fn parse_loop_while(parser: &mut ParserState) -> ASTNode {
    // Eat 'while'
    if !eat!(parser, While, IMPOSSIBLE_STATE) {
        return ASTNode::None;
    }
    // Condition
    let cond = parse_binop_logic(parser);
    if cond == ASTNode::None {
        return ASTNode::None;
    }
    // End parens
    if !eat!(parser, Rparan, "missing ')' in loop") {
        return ASTNode::None;
    }
    // Body
    let body = parse_body(parser);
    if body == ASTNode::None {
        return ASTNode::None;
    }
    // Return
    return ASTNode::WhileStmt(Box::new(cond), Box::new(body));
}

fn parse_loop(parser: &mut ParserState) -> ASTNode {
    // Eat loop
    next_tok(parser);
    // Start parens
    if !eat!(parser, Lparan, "missing '(' in loop") {
        return ASTNode::None;
    }
    // Get the loop type and call the helper
    return if cur_tok(parser) == While {
        parse_loop_while(parser)
    } else {
        parse_loop_iter(parser)
    };
}

// Imports
fn parse_import(parser: &mut ParserState) -> ASTNode {
    // Eat import
    next_tok(parser);
    // The parens part 1
    if !eat!(parser, Lparan, "missing '(' in import") {
        return ASTNode::None;
    }
    // Import name
    let value = parse_binop_logic(parser);
    if value == ASTNode::None {
        return ASTNode::None;
    }
    // The parens part 2: electric boogaloo
    if !eat!(parser, Rparan, "missing ')' in import") {
        return ASTNode::None;
    }
    // Semicolon
    if !eat_semicolon!(parser) {
        return ASTNode::None;
    }
    // Return
    return ASTNode::ImportStmt(Box::new(value));
}

// Varible definition
fn parse_let(parser: &mut ParserState) -> ASTNode {
    // Eat let
    next_tok(parser);
    // Get var nane
    let name: String;
    if let Identifier(n) = cur_tok(parser) {
        name = n;
    } else {
        error!(parser, "expected varible name");
        return ASTNode::None;
    }
    next_tok(parser);
    // Let without value (non-standard)
    if let Semicolon = cur_tok(parser) {
        next_tok(parser);
        if parser.extentions.contains(&"auto-none".to_string()) {
            return ASTNode::LetStmt(name, Box::new(ASTNode::NoneExpr));
        } else {
            error!(parser, "let must have value");
            error!(
                parser,
                "this can be disabled by the auto none extention (`-use-auto-none`)",
                ErrType::Hint
            );
            return ASTNode::None;
        }
    }
    // Equals symbol
    if !eat!(parser, Equals, "expected '=' in variable declaration") {
        return ASTNode::None;
    }
    // Let with value
    let value = parse_binop_logic(parser);
    if value == ASTNode::None {
        return ASTNode::None;
    }
    // Semicolon
    if !eat_semicolon!(parser) {
        return ASTNode::None;
    }
    // Return
    return ASTNode::LetStmt(name, Box::new(value));
}

// Returning
fn parse_return(parser: &mut ParserState) -> ASTNode {
    // Function check
    if !parser.in_func {
        error!(parser, "return outside of functions");
        next_tok(parser);
        return ASTNode::None;
    }
    // Eat return
    next_tok(parser);
    // Return without value (non-standard)
    if let Semicolon = cur_tok(parser) {
        next_tok(parser);
        if parser.extentions.contains(&"auto-none".to_string()) {
            return ASTNode::ReturnStmt(Box::new(ASTNode::NoneExpr));
        } else {
            error!(parser, "return must have value");
            error!(
                parser,
                "this can be disabled by the auto none extention (`-use-auto-none`)",
                ErrType::Hint
            );
            return ASTNode::None;
        }
    }
    // Returns with a value
    let ret_val = parse_binop_logic(parser);
    if ret_val == ASTNode::None {
        return ASTNode::None;
    }
    // Semicolon
    if !eat_semicolon!(parser) {
        return ASTNode::None;
    }
    // Return return
    return ASTNode::ReturnStmt(Box::new(ret_val));
}

// Functions
fn parse_functi(parser: &mut ParserState) -> ASTNode {
    // Disallow functions in functions
    if parser.in_func {
        next_tok(parser);
        error!(parser, "cannot create function in function");
        return ASTNode::None;
    }
    // Eat functi
    next_tok(parser);
    // Name
    let name: String;
    if let Identifier(n) = cur_tok(parser) {
        name = n;
    } else {
        error!(parser, "expected function name");
        return ASTNode::None;
    }
    next_tok(parser);
    // Args
    if !eat!(parser, Lparan, "expected '(' at start of argument list") {
        return ASTNode::None;
    }
    let mut args: Vec<String> = vec![];
    loop {
        if let Rparan = cur_tok(parser) {
            break;
        }
        // Arg name
        if let Identifier(n) = cur_tok(parser) {
            args.push(n);
            next_tok(parser);
        } else {
            error!(parser, "expected argument name");
            return ASTNode::None;
        }
        // Comma for new args or rparan for end of args
        if let Rparan | Comma = cur_tok(parser) {} else {
            error!(parser, "expected comma or ')' in argument list");
            return ASTNode::None;
        }
        // Eat comma
        if let Comma = cur_tok(parser) {
            next_tok(parser);
        }
    }
    next_tok(parser);
    // Body
    if let Lbrace = cur_tok(parser) {} else {
        error!(parser, "expected '{' to start function body");
        err(
            &parser.tokens[parser.at].stream,
            "forward declaration isn't supported",
            1, ErrType::Hint
        );
        return ASTNode::None;
    }
    parser.in_func = true;
    let body = parse_body(parser);
    parser.in_func = false;
    if let ASTNode::None = body {
        return ASTNode::None;
    }
    // Return
    return ASTNode::FunctiStmt(name, args, Box::new(body));
}

// Main parsing
pub fn parse(tokens: Vec<Token>, extentions: Vec<String>) -> Vec<ASTNode> {
    // Set up
    let mut parser = ParserState{
        tokens, extentions, at: 0,
        has_err: false, in_func: false
    };
    let mut ret: Vec<ASTNode> = vec![];
    // Parse
    while cur_tok(&parser) != Eof {
        let stmt = parse_statement(&mut parser);
        if stmt != ASTNode::None {
            ret.push(stmt);
        }
    }
    // Return
    if parser.has_err {
        return vec![];
    }
    return ret;
}
