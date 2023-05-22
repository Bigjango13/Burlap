use crate::Arguments;
use crate::common::{err, ErrType, IMPOSSIBLE_STATE};
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
    CallExpr(String, Vec<ASTNode>),
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
    // Import, ("math")
    ImportStmt(String),

    // Special
    // Nop, does nothing
    Nop,
}

// Parser state
struct Parser {
    tokens: Vec<Token>,
    ast: Vec<ASTNode>,
    extensions: Vec<String>,
    at: usize,
    in_func: bool,
    has_err: bool,
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
            $parser.extensions.contains(&"color".to_string())
        );
    );
    // DO NOT USE WITH ErrType::Err
    ($parser:expr, $msg:expr, $err_type:expr) => (
        err(
            &$parser.tokens[$parser.at].stream, $msg, $err_type,
            $parser.extensions.contains(&"color".to_string())
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

// Expressions
// Call
fn parse_call(parser: &mut Parser) -> Option<ASTNode> {
    // A function can't return a function so no need for a loop
    if let Identifier(_) = parser.current() {} else {
        // Can't call anything but identifiers
        return parse_base_expr(parser);
    }
    // Eat
    let mut ret = parse_base_expr(parser)?;
    let name: String;
    if let ASTNode::VarExpr(n) = &ret {
        name = n.clone();
    } else {
        return Some(ret);
    }
    // Parse call
    let mut args: Vec<ASTNode> = vec![];
    if let Lparan = parser.current() {
        parser.next();
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
        parser.next();
        ret = ASTNode::CallExpr(name, args);
    }
    return Some(ret);
}

// Index
fn parse_index(parser: &mut Parser) -> Option<ASTNode> {
    // Get expr
    let mut ret = parse_call(parser)?;
    // Parse index
    while let Lbracket = parser.current() {
        parser.next();
        let index = parse_expr(parser);
        ret = ASTNode::IndexExpr(Box::new(ret), Box::new(index?));
        // Eat ']'
        eat!(parser, Rbracket, "expected ']' at end of index")?;
    }
    return Some(ret);
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
        if let Identifier(v) = parser.next() {
            parser.next();
            return Some(ASTNode::UnaryExpr(op, Box::new(ASTNode::VarExpr(v))));
        } else {
            error!(parser, "++/-- require identifiers", ErrType::Err);
            return Option::None;
        }
    }
    return parse_index(parser);
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

fn parse_binop_math(parser: &mut Parser) -> Option<ASTNode> {
    // Math binops, +, -, *, /, %, in
    parse_binop_helper(parser, vec![
        Plus, Minus, Times, Div, Modulo, In
    ], &parse_unary, true)
}
fn parse_binop_cmp(parser: &mut Parser) -> Option<ASTNode> {
    // Compare binops, ==, !=, <, >, <=, >=
    parse_binop_helper(parser, vec![
        EqualsEquals, NotEquals, Lt, Gt, LtEquals, GtEquals
    ], &parse_binop_math, true)
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
            Equals, PlusEquals, MinusEquals, TimesEquals, DivEquals
        ], &parse_binop_logic, false)?;
        let ASTNode::BinopExpr(rhs, op, _) = ret.clone() else {
            return Some(ret);
        };
        // Check that the binop is a setter
        if let Equals | PlusEquals | MinusEquals | TimesEquals | DivEquals =
            op {} else {
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
            return (name.clone(), Some(ASTNode::VarExpr(name)));
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
        Identifier(v) => { parser.next(); Some(ASTNode::VarExpr(v))     },
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
        | PlusPlus | MinusMinus | Not => {
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
        name = n;
    } else {
        error!(parser, "expected variable name");
        return Option::None;
    }
    parser.next();
    // Obligatory 'in'
    eat!(parser, In, "missing 'in' keyword in loop")?;
    // Iterator
    let mut iter = parse_expr(parser)?;
    // Range optimization
    if let ASTNode::CallExpr(name, args) = iter.clone() {
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
            iter = ASTNode::CallExpr("__burlap_range".to_string(), args);
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
fn parse_import(parser: &mut Parser) -> Option<ASTNode> {
    // Eat import
    parser.next();
    // The parens part 1
    eat!(parser, Lparan, "missing '(' in import")?;
    // Import name
    let Str(value) = parser.current() else {
        error!(parser, "import filename must be a constant string");
        return Option::None;
    };
    parser.next();
    // The parens part 2: electric boogaloo
    eat!(parser, Rparan, "missing ')' in import")?;
    // Semicolon
    eat_semicolon!(parser)?;
    // Return
    return Some(ASTNode::ImportStmt(value));
}

// Variable definition
fn parse_let(parser: &mut Parser) -> Option<ASTNode> {
    // Eat let
    parser.next();
    // Get var name
    let name: String;
    if let Identifier(n) = parser.current() {
        name = n;
    } else {
        error!(parser, "expected variable name");
        return Option::None;
    }
    parser.next();
    // Let without value (non-standard)
    if let Semicolon = parser.current() {
        parser.next();
        if parser.extensions.contains(&"auto-none".to_string()) {
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
        if parser.extensions.contains(&"auto-none".to_string()) {
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
            args.push(n);
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
    // Body
    if let Lbrace = parser.current() {} else {
        error!(parser, "expected '{' to start function body");
        err(
            &parser.tokens[parser.at].stream,
            "forward declaration isn't supported", ErrType::Hint,
            parser.extensions.contains(&"color".to_string())
        );
        return Option::None;
    }
    parser.in_func = true;
    let body = parse_body(parser);
    parser.in_func = false;
    // Return
    return Some(ASTNode::FunctiStmt(name, args, Box::new(body?)));
}

// Main parsing
pub fn parse(tokens: Vec<Token>, args: &Arguments) -> Option<Vec<ASTNode>> {
    if tokens.is_empty() {
        return Some(vec![]);
    }
    // Set up
    // TODO: Line numbers
    let mut parser = Parser{
        tokens, extensions: args.extensions.clone(),
        at: 0, has_err: false, in_func: false,
        ast: vec![]
    };
    // Parse
    while parser.current() != Eof {
        // Import must be highest scope
        let stmt = if parser.current() == Import {
            parse_import(&mut parser)
        } else {
            parse_statement(&mut parser)
        };
        if let Some(stmt) = stmt {
            parser.ast.push(stmt);
            continue;
        }
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
    return Some(parser.ast);
}
