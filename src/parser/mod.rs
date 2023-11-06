use crate::lexer;
use crate::symbols::{Numeric, Ttype};
use lexer::{Lexer, Token};
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Pipe,       // ->
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Index,      // []
    Primary,
}

pub type Identifier = String;
#[derive(Debug, Clone)]
pub enum Ast {
    Let(
        Identifier,
        Option<Box<Ast>>, // optional explicit type parameter
        //
        Option<Box<Ast>>, // optional immediate assignment expression
    ),
    FnDeclaration(Identifier, Box<Ast>),
    TypeDeclaration(Identifier, Box<Ast>),

    //expressions
    Id(Identifier, Ttype),
    Binop(Token, Box<Ast>, Box<Ast>, Ttype),
    Unop(Token, Box<Ast>, Ttype),
    Tuple(Vec<Ast>, Ttype),
    Index(Box<Ast>, Box<Ast>, Ttype),
    Assignment(Box<Ast>, Box<Ast>, Ttype),
    Fn(Vec<Ast>, Option<Ttype>, Vec<Ast>, Ttype),
    Call(Box<Ast>, Vec<Ast>, Ttype),
    Body(Vec<Ast>, Ttype),
    If(Box<Ast>, Vec<Ast>, Option<Vec<Ast>>, Ttype),
    VarArg,

    // literals
    Int8(i8),
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}
pub static mut TVAR_COUNT: usize = 0;

fn tvar() -> Ttype {
    let tv;
    unsafe {
        tv = Ttype::Var(format!("t{}", TVAR_COUNT));
        TVAR_COUNT += 1;
    };
    tv
}
// Ast node match template
// match ast {
//     Ast::Let(
//         id,
//         t,     // optional explicit type parameter
//         value, // optional immediate assignment expression
//     ) => {}
//     Ast::FnDeclaration(id, fn_expr) => {}
//     Ast::TypeDeclaration(id, type_expr) => {}
//
//     Ast::Id(id, ttype) => {}
//     Ast::Binop(token, left, right, ttype) => {}
//     Ast::Unop(token, operand, ttype) => {}
//     Ast::Tuple(exprs, ttype) => {}
//     Ast::Index(obj, idx, ttype) => {}
//     Ast::Assignment(assignee, val, ttype) => {}
//     Ast::Fn(params, ret_type, body, ttype) => {}
//     Ast::Call(callable, args, ttype) => {}
//     Ast::Body(stmts, ttype) => {}
//     Ast::If(cond, then, elze, ttype) => {}
//
//     Ast::Int8(i) => {}
//     Ast::Integer(i) => {}
//     Ast::Number(f) => {}
//     Ast::Bool(b) => {}
//     Ast::String(s) => {}
// }

impl Ast {
    pub fn get_ttype(&self) -> Option<Ttype> {
        match self {
            Ast::Id(_, t)
            | Ast::Binop(_, _, _, t)
            | Ast::Unop(_, _, t)
            | Ast::Tuple(_, t)
            | Ast::Index(_, _, t)
            | Ast::Assignment(_, _, t)
            | Ast::Fn(_, _, _, t)
            | Ast::Body(_, t)
            | Ast::Call(_, _, t)
            | Ast::If(_, _, _, t) => Some(t.clone()),

            // literals
            Ast::Int8(_) => Some(Ttype::Numeric(Numeric::Int8)),
            Ast::Integer(_) => Some(Ttype::Numeric(Numeric::Int)),
            Ast::Number(_) => Some(Ttype::Numeric(Numeric::Num)),
            Ast::Bool(_) => Some(Ttype::Bool),
            Ast::String(_) => Some(Ttype::Str),
            _ => None,
        }
    }

    pub fn set_ttype(&mut self, t: Ttype) {
        println!("set ttype {:?}", t);
        match self {
            Ast::Id(_, ref mut ttype)
            | Ast::Binop(_, _, _, ref mut ttype)
            | Ast::Unop(_, _, ref mut ttype)
            | Ast::Tuple(_, ref mut ttype)
            | Ast::Index(_, _, ref mut ttype)
            | Ast::Assignment(_, _, ref mut ttype)
            | Ast::Fn(_, _, _, ref mut ttype)
            | Ast::Body(_, ref mut ttype)
            | Ast::Call(_, _, ref mut ttype)
            | Ast::If(_, _, _, ref mut ttype) => {
                if let Ttype::Var(_n) = ttype {
                    // if n.is_empty() {
                    *ttype = t
                    // }
                }
            }
            _ => {}
        }
    }
}

/// implement PartialEq for Ast to ignore ttype Tvar names
impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Let(id1, t1, e1), Ast::Let(id2, t2, e2)) => id1 == id2 && t1 == t2 && e1 == e2,
            (Ast::FnDeclaration(id1, e1), Ast::FnDeclaration(id2, e2)) => id1 == id2 && e1 == e2,
            (Ast::TypeDeclaration(id1, e1), Ast::TypeDeclaration(id2, e2)) => {
                id1 == id2 && e1 == e2
            }
            (Ast::Id(id1, _), Ast::Id(id2, _)) => id1 == id2,
            (Ast::Binop(t1, e1, e2, _), Ast::Binop(t2, e3, e4, _)) => {
                t1 == t2 && e1 == e3 && e2 == e4
            }
            (Ast::Unop(t1, e1, _), Ast::Unop(t2, e2, _)) => t1 == t2 && e1 == e2,
            (Ast::Tuple(v1, _), Ast::Tuple(v2, _)) => v1 == v2,
            (Ast::Index(e1, e2, _), Ast::Index(e3, e4, _)) => e1 == e3 && e2 == e4,
            (Ast::Assignment(e1, e2, _), Ast::Assignment(e3, e4, _)) => e1 == e3 && e2 == e4,
            (Ast::Fn(v1, t1, v2, _), Ast::Fn(v3, t2, v4, _)) => v1 == v3 && t1 == t2 && v2 == v4,
            (Ast::Call(e1, v1, _), Ast::Call(e2, v2, _)) => e1 == e2 && v1 == v2,
            (Ast::Body(v1, _), Ast::Body(v2, _)) => v1 == v2,
            (Ast::If(e1, v1, t1, _), Ast::If(e2, v2, t2, _)) => e1 == e2 && v1 == v2 && t1 == t2,
            (Ast::Int8(i1), Ast::Int8(i2)) => i1 == i2,
            (Ast::Integer(i1), Ast::Integer(i2)) => i1 == i2,
            (Ast::Number(n1), Ast::Number(n2)) => n1 == n2,
            (Ast::Bool(b1), Ast::Bool(b2)) => b1 == b2,
            (Ast::String(s1), Ast::String(s2)) => s1 == s2,
            _ => false,
        }
    }
}
pub fn print_ast(ast: Ast, indent: usize) {
    // TODO: finish this crappy function
    let ident = format!("{: ^1$}", "", indent);
    print!("{}", ident);
    match ast {
        Ast::Let(
            _id,
            _type_param, // optional explicit type parameter
            _assignment, // optional immediate assignment expression
        ) => {}
        Ast::FnDeclaration(id, fn_expr) => {
            print!("fn {} ", id);
            print_ast(*fn_expr, indent);
        }
        Ast::TypeDeclaration(_id, _type_expr) => {}

        //expressions
        // Ast::Id(id, Some(t), _ttype) => {
        //     print!("{} {:?}, ", id, *t);
        // }
        Ast::Id(id, _ttype) => {
            print!("{}, ", id);
        }
        Ast::Binop(_token, _l, _r, _ttype) => {}
        Ast::Unop(_token, _operand, _ttype) => {}
        Ast::Tuple(_exprs, _ttype) => {}
        Ast::Index(_obj, _idx, _ttype) => {}
        Ast::Assignment(_id, _val, _ttype) => {}
        Ast::Fn(params, ret, body, _ttype) => {
            for p in params {
                print_ast(p, 0);
            }
            if let Some(ret) = ret {
                print!("{:?}", ret);
            }
            println!();
            for s in body {
                print_ast(s, indent + 1);
            }
        }
        Ast::Call(_callable, _args, _ttype) => {}
        Ast::Body(_stmts, _ttype) => {}
        Ast::If(_cond, _then, _elze, _ttype) => {}

        // literals
        Ast::Int8(_v) => {}
        Ast::Integer(_v) => {}
        Ast::Number(_v) => {}
        Ast::Bool(_v) => {}
        Ast::String(_v) => {}
        Ast::VarArg => {}
    }
}

pub type Block = Vec<Ast>;
pub type Program = Block;

#[macro_export]
macro_rules! int_expr {
    ($value:expr) => {
        Ast::Integer($value)
    };
}

macro_rules! num_expr {
    ($value:expr) => {
        Ast::Number($value)
    };
}

macro_rules! str_expr {
    ($value:expr) => {
        Ast::String($value)
    };
}

#[macro_export]
macro_rules! bool_expr {
    ($value:expr) => {
        Ast::Bool($value)
    };
}
#[macro_export]
macro_rules! id_expr {
    ($value:expr) => {
        Ast::Id($value.into(), tvar())
    };

    ($value:expr, $var: expr) => {
        Ast::Id($value.into(), $var)
    };
}

#[macro_export]
macro_rules! tuple_expr {
    ($values:expr) => {
        Ast::Tuple($values, tvar())
    };

    ($values:expr, $var:expr) => {
        Ast::Tuple($values, $var)
    };
}
#[macro_export]
macro_rules! binop_expr {
    ($op:expr,$l:expr,$r:expr) => {
        Ast::Binop($op, Box::new($l), Box::new($r), tvar())
    };

    ($op:expr, $l:expr, $r:expr, $var:expr) => {
        Ast::Binop($op, Box::new($l), Box::new($r), $var)
    };
}

macro_rules! unop_expr {
    ($op:expr,$operand:expr) => {
        Ast::Unop($op, Box::new($operand), tvar())
    };

    ($op:expr,$operand:expr, $var: expr) => {
        Ast::Unop($op, Box::new($operand), $var)
    };
}

macro_rules! assignment_expr {
    ($assignee:expr,$value:expr) => {
        Ast::Assignment(Box::new($assignee), Box::new($value), tvar())
    };

    ($assignee:expr,$value:expr, $var:expr) => {
        Ast::Assignment(Box::new($assignee), Box::new($value), $var)
    };
}

#[macro_export]
macro_rules! if_expr {
    ($cond: expr, $then: expr, $else: expr) => {
        Ast::If(Box::new($cond), $then, $else, tvar())
    };

    ($cond: expr, $then: expr, $else: expr, $var: expr) => {
        Ast::If(Box::new($cond), $then, $else, $var)
    };
}

#[macro_export]
macro_rules! call_expr {
    ($callable: expr, $args: expr) => {
        Ast::Call(Box::new($callable), $args, tvar())
    };

    ($callable: expr, $args: expr, $var: expr) => {
        Ast::Call(Box::new($callable), $args, $var)
    };
}

pub struct Parser {
    lexer: Lexer,
    previous: Token,
    current: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current = lexer.scan_token();
        Self {
            lexer,
            previous: Token::Start,
            current,
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = vec![];
        while self.current != Token::Eof {
            match self.parse_statement() {
                Some(stmt) => program.push(stmt),
                None => self.advance(),
            }
        }

        program
    }
    fn advance(&mut self) {
        self.previous = self.current.clone();
        self.current = self.lexer.scan_token()
    }

    fn parse_statement(&mut self) -> Option<Ast> {
        match &self.current {
            Token::Let => self.parse_let_statement(),
            // Token::Identifier(id) => self.parse_assignment_statement(id.clone()),
            Token::Type => self.parse_type_declaration(),
            // Token::Nl => None,
            _ => self.parse_expression(Precedence::None),
        }
    }

    fn parse_assignment_expression(&mut self, id: Option<Ast>) -> Option<Ast> {
        self.advance();

        id.as_ref()?;

        self.parse_expression(Precedence::None)
            .map(|e| assignment_expr!(id.unwrap(), e))
    }

    fn parse_typed_identifier(&mut self) -> Option<(Identifier, Option<Identifier>)> {
        let id = self.parse_identifier();
        let type_param = if self.expect_token(Token::Colon) {
            let tok = self.current.clone();
            match &tok {
                Token::Identifier(t) => {
                    self.advance();
                    Some(t.clone())
                }
                _ => None,
            }
        } else {
            None
        };

        println!("id {:?} {:?}", id, type_param);
        id.map(|id| (id, type_param))
    }

    fn parse_let_statement(&mut self) -> Option<Ast> {
        self.advance();
        let id = self.parse_identifier();
        let type_param = if self.expect_token(Token::Colon) {
            self.parse_type_expression()
        } else {
            None
        };

        if self.expect_token(Token::Assignment) {
            match self.current {
                Token::Extern => {
                    self.advance();
                    if self.expect_token(Token::Fn) {
                        Some(Ast::FnDeclaration(
                            id.unwrap(),
                            Box::new(self.parse_fn_expression()?),
                        ))
                    } else {
                        None
                    }
                }
                Token::Fn => Some(Ast::FnDeclaration(
                    id.unwrap(),
                    Box::new(self.parse_fn_expression().unwrap()),
                )),

                _ => Some(Ast::Let(
                    id.unwrap(),
                    type_param.map(|t| Box::new(t)),
                    Some(Box::new(self.parse_expression(Precedence::None).unwrap())),
                )),
            }
        } else {
            Some(Ast::Let(id.unwrap(), type_param.map(|t| Box::new(t)), None))
        }
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        let id = match self.current {
            Token::Identifier(ref mut ident) => Some(ident.clone()),
            _ => return None,
        };
        self.advance();
        id
    }

    fn parse_type_declaration(&mut self) -> Option<Ast> {
        None
    }
    fn token_to_precedence(&self, tok: &Token) -> Precedence {
        match tok {
            Token::Assignment => Precedence::Assignment,
            Token::Equality | Token::NotEqual => Precedence::Equality,
            Token::Lt | Token::Lte | Token::Gt | Token::Gte => Precedence::Comparison,
            Token::Plus | Token::Minus => Precedence::Term,
            Token::Slash | Token::Star | Token::Modulo => Precedence::Factor,
            Token::LeftSq => Precedence::Index,
            Token::Lp | Token::Rp | Token::Dot => Precedence::Call,
            Token::Bang => Precedence::Unary,
            Token::LogicalOr => Precedence::Or,
            Token::LogicalAnd => Precedence::And,
            _ => Precedence::None,
        }
    }

    fn parse_infix_expr(&mut self, left: Option<Ast>) -> Option<Ast> {
        let tok = self.current.clone();
        // println!("parse infix {:?} {:?}", left, tok);
        let precedence = self.token_to_precedence(&tok);

        self.advance();
        left.as_ref()?;

        self.parse_expression(precedence)
            .map(|expr| binop_expr!(tok, left.unwrap(), expr))
    }

    fn parse_prefix_expr(&mut self) -> Option<Ast> {
        let tok = self.current.clone();

        let precedence = self.token_to_precedence(&tok);
        self.advance();
        self.parse_expression(precedence)
            .map(|expr| unop_expr!(tok, expr))
    }
    fn parse_tuple(&mut self, first: Option<Ast>) -> Option<Ast> {
        first.as_ref()?;

        let mut exprs = vec![first.unwrap()];
        self.advance();

        while self.current != Token::Rp {
            self.skip_token(Token::Comma);
            if let Some(expr) = self.parse_expression(Precedence::None) {
                exprs.push(expr);
            }
        }

        Some(tuple_expr!(exprs))
    }

    fn parse_index_expr(&mut self, obj: Option<Ast>) -> Option<Ast> {
        self.advance();
        self.parse_expression(Precedence::None)
            .map(|idx| Ast::Index(Box::new(obj.unwrap()), Box::new(idx), tvar()))
    }

    fn parse_grouping(&mut self) -> Option<Ast> {
        self.advance();
        let expr = self.parse_expression(Precedence::None);

        match self.current {
            Token::Rp => {
                self.advance();
                expr
            }
            Token::Comma => self.parse_tuple(expr),
            _ => None,
        }
    }
    fn skip_token(&mut self, tok: Token) {
        if self.current == tok {
            self.advance();
        }
    }
    fn parse_type_expression(&mut self) -> Option<Ast> {
        let tok = self.current.clone();
        self.advance();
        match &tok {
            Token::Identifier(id) => Some(id_expr!(id)),
            _ => None,
        }
    }

    fn parse_fn_args(&mut self) -> Vec<Ast> {
        let mut args = vec![];
        self.advance();

        while self.current != Token::Rp {
            self.skip_token(Token::Comma);
            if let Some((arg, arg_type)) = self.parse_typed_identifier() {
                println!("arg {:?}, {:?}", arg, arg_type);
                let t = match arg_type.as_deref() {
                    Some("int8") => Ttype::Numeric(Numeric::Int8),
                    Some("int") => Ttype::Numeric(Numeric::Int),
                    Some("double") => Ttype::Numeric(Numeric::Num),
                    Some("bool") => Ttype::Bool,
                    Some("str") => Ttype::Str,
                    Some(s) => Ttype::Var(s.into()),
                    _ => tvar(),
                };
                args.push(Ast::Id(arg, t));
            } else if self.current == Token::TripleDot {
                println!("var arg?   {:?}", self.current);
                self.advance();
                args.push(Ast::VarArg)
            }
        }
        self.advance(); // move past Rp

        args
    }

    fn parse_fn_expression(&mut self) -> Option<Ast> {
        self.advance();
        if self.current != Token::Lp {
            return None;
        };
        let args = self.parse_fn_args();
        println!("parsed args {:?}", args);

        let return_type = if self.expect_token(Token::Colon) {
            self.parse_identifier().map(|t| match t.as_str() {
                "int8" => Ttype::Numeric(Numeric::Int8),
                "int" => Ttype::Numeric(Numeric::Int),
                "double" => Ttype::Numeric(Numeric::Num),
                "bool" => Ttype::Bool,
                "str" => Ttype::Str,
                s => Ttype::Var(s.into()),
                _ => tvar(),
            })
        } else {
            None
        };

        let body = if self.current == Token::LeftBrace {
            self.parse_body()
        } else {
            vec![]
        };

        Some(Ast::Fn(args, return_type, body, tvar()))
    }

    fn parse_body(&mut self) -> Vec<Ast> {
        let mut body = vec![];
        while self.current != Token::RightBrace {
            match self.parse_statement() {
                Some(stmt) => body.push(stmt),
                None => self.advance(),
            }
        }
        body
    }
    fn parse_conditional_expr(&mut self) -> Option<Ast> {
        self.advance();

        let condition = self.parse_expression(Precedence::None)?;
        // println!("condition: {:?}", condition);

        let then = if self.expect_token(Token::LeftBrace) {
            let body = self.parse_body();

            self.advance();
            body
        } else {
            return None;
        };

        let elze = if self.expect_token(Token::Else) {
            if !self.expect_token(Token::LeftBrace) {
                return None;
            }

            let body = self.parse_body();

            self.advance();
            Some(body)
        } else {
            None
        };

        Some(if_expr!(condition, then, elze))
    }
    fn parse_call(&mut self, callee: Option<Ast>) -> Option<Ast> {
        match callee {
            Some(c) => {
                let mut call_params = vec![];
                self.advance();

                while self.current != Token::Rp {
                    self.skip_token(Token::Comma);
                    if let Some(expr) = self.parse_expression(Precedence::None) {
                        println!("expr {:?}", expr);
                        self.print_current();
                        call_params.push(expr);
                    }
                }
                Some(call_expr!(c, call_params))
            }
            _ => None,
        }
    }
    fn parse_int(&mut self, i: i64) -> Option<Ast> {
        self.advance();
        Some(int_expr!(i))
    }

    fn parse_num(&mut self, n: f64) -> Option<Ast> {
        self.advance();
        Some(num_expr!(n))
    }

    fn parse_str(&mut self, s: String) -> Option<Ast> {
        self.advance();
        Some(str_expr!(s))
    }
    fn parse_bool(&mut self, b: bool) -> Option<Ast> {
        self.advance();
        Some(bool_expr!(b))
    }
    fn parse_id(&mut self, id: String) -> Option<Ast> {
        self.advance();
        Some(id_expr!(id))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Ast> {
        // prefix
        let mut tok = self.current.clone();
        let mut left = match tok {
            Token::Integer(i) => self.parse_int(i),
            Token::Number(i) => self.parse_num(i),
            Token::String(s) => self.parse_str(s.clone()),
            Token::True => self.parse_bool(true),
            Token::False => self.parse_bool(false),
            Token::Identifier(id) => self.parse_id(id.clone()),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expr(),
            Token::Lp => self.parse_grouping(),
            Token::If => self.parse_conditional_expr(),

            _ => {
                // self.error_no_prefix_parser();
                return None;
            }
        };

        tok = self.current.clone();

        while precedence < self.token_to_precedence(&tok) {
            match self.current {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Star
                | Token::Modulo
                | Token::Equality
                | Token::NotEqual
                | Token::Lt
                | Token::Lte
                | Token::Gt
                | Token::Gte => {
                    left = self.parse_infix_expr(left);
                }
                Token::LeftSq => {
                    left = self.parse_index_expr(left);
                }
                Token::Assignment => {
                    left = self.parse_assignment_expression(left);
                }
                Token::Lp => {
                    left = self.parse_call(left);
                }
                _ => return left,
            }
        }

        left
    }

    fn print_current(&self) {
        println!("{:?}", self.current);
    }
    fn expect_token(&mut self, tok: Token) -> bool {
        if self.current == tok {
            self.advance();
            true
        } else {
            false
        }
    }
}

pub fn parse(input: String) -> Program {
    let lexer = Lexer::new(input);
    let mut p = Parser::new(lexer);
    p.parse_program()
}

#[cfg(test)]
mod tests {
    use crate::symbols::{tbool, tint, tnum};

    use super::*;

    use pretty_assertions::assert_eq;

    fn tvar() -> Ttype {
        Ttype::tvar("")
    }

    #[test]
    fn let_stmt() {
        let input = r#"
        let a = 1 
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Let("a".into(), None, Some(Box::new(int_expr!(1))))],
            program
        )
    }

    #[test]
    fn let_typed() {
        let input = r#"
        let a: int = 1 
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Let(
                "a".into(),
                Some(Box::new(id_expr!("int"))),
                Some(Box::new(int_expr!(1)))
            )],
            program
        )
    }

    #[test]
    fn assignment() {
        let input = r#"a = 1 + 1"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Assignment(
                Box::new(id_expr!("a")),
                Box::new(binop_expr!(Token::Plus, int_expr!(1), int_expr!(1))),
                tvar()
            )],
            program
        )
    }

    #[test]
    fn math_exprs() {
        let tests = vec![
            (
                r#"1 + 7.0"#,
                binop_expr!(Token::Plus, int_expr!(1), num_expr!(7.0)),
            ),
            (
                r#"1 * (7.0 + 200)"#,
                binop_expr!(
                    Token::Star,
                    int_expr!(1),
                    binop_expr!(Token::Plus, num_expr!(7.0), int_expr!(200))
                ),
            ),
            (
                r#"13 * (2 + 2) + 100.0"#,
                binop_expr!(
                    Token::Plus,
                    binop_expr!(
                        Token::Star,
                        int_expr!(13),
                        binop_expr!(Token::Plus, int_expr!(2), int_expr!(2))
                    ),
                    num_expr!(100.0)
                ),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input.into()));
            let program = parser.parse_program();
            assert_eq!(vec![expect], program);
        }
    }

    #[test]

    fn grouping() {
        let input = r#"(1 + 1)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![binop_expr!(Token::Plus, int_expr!(1), int_expr!(1))],
            program
        )
    }

    #[test]
    fn tuple() {
        let input = r#"(1, 1)"#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![tuple_expr!(vec![int_expr!(1), int_expr!(1)])], program)
    }

    #[test]
    fn tuple_len_one() {
        let input = r#"(1,)"#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![tuple_expr!(vec![int_expr!(1)])], program)
    }

    #[test]
    fn tuple_ids() {
        let input = r#"(a, b)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![tuple_expr!(vec![id_expr!("a"), id_expr!("b"),])],
            program
        )
    }

    #[test]
    fn unop() {
        let input = r#"
        -7.0
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![unop_expr!(Token::Minus, num_expr!(7.0))], program)
    }

    #[test]
    fn unop_bang() {
        let input = r#"
        !1
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn if_else() {
        let input = r#"if true {1} else {2}"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![if_expr!(
                bool_expr!(true),
                vec![int_expr!(1)],
                Some(vec![int_expr!(2)])
            )],
            program
        );

        // assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn if_else_tuple() {
        let input = r#"if (true) {(1, 2)} else {(3, 4)}"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![if_expr!(
                bool_expr!(true),
                vec![tuple_expr!(vec![int_expr!(1), int_expr!(2)])],
                Some(vec![tuple_expr!(vec![int_expr!(3), int_expr!(4)])])
            )],
            program
        );

        // assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn fn_declaration() {
        let input = r#"
        let f = fn (a, b, c) { a + b + c }
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::FnDeclaration(
                "f".into(),
                Box::new(Ast::Fn(
                    vec![id_expr!("a"), id_expr!("b"), id_expr!("c"),],
                    None,
                    vec![
                        (binop_expr!(
                            Token::Plus,
                            binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
                            id_expr!("c")
                        ))
                    ],
                    tvar(),
                ))
            )],
            program
        )
    }

    #[test]
    fn typed_fn_declaration() {
        let input = r#"
        let f = fn (a: int, b: double, c: bool): int { a + b + c }
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::FnDeclaration(
                "f".into(),
                Box::new(Ast::Fn(
                    vec![
                        id_expr!("a", tint()),
                        id_expr!("b", tnum()),
                        id_expr!("c", tbool()),
                    ],
                    Some(Ttype::Numeric(Numeric::Int)),
                    vec![
                        (binop_expr!(
                            Token::Plus,
                            binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
                            id_expr!("c")
                        ))
                    ],
                    tvar(),
                ))
            )],
            program
        )
    }

    #[test]
    fn extern_fn_declaration() {
        let input = r#"
        let printf = fn (): int
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::FnDeclaration(
                "printf".into(),
                Box::new(Ast::Fn(
                    vec![],
                    Some(Ttype::Numeric(Numeric::Int)),
                    vec![],
                    tvar(),
                ))
            )],
            program
        )
    }
    #[test]
    fn fn_with_if() {
        let input = r#"
        let x = fn (a, b, c) {
          if a == b {
            2 * a
          } else {
            b + c
          }
        }"#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::FnDeclaration(
                "x".into(),
                Box::new(Ast::Fn(
                    vec![id_expr!("a"), id_expr!("b"), id_expr!("c"),],
                    None,
                    vec![if_expr!(
                        binop_expr!(Token::Equality, id_expr!("a"), id_expr!("b")),
                        vec![binop_expr!(Token::Star, int_expr!(2), id_expr!("a"))],
                        Some(vec![binop_expr!(Token::Plus, id_expr!("b"), id_expr!("c"))]),
                        tvar()
                    )],
                    tvar(),
                ))
            )],
            program
        )
    }
    // vec![binop_expr!(
    //     Token::Plus,
    //     binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
    //     id_expr!("c")
    // )],

    #[test]
    fn call_expr() {
        let input = r#"f()"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(vec![(call_expr!(id_expr!("f"), vec![]))], program,)
    }

    #[test]
    fn call_expr_neg() {
        let input = r#"f(-1)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![(call_expr!(id_expr!("f"), vec![unop_expr!(Token::Minus, int_expr!(1))]))],
            program,
        )
    }
    #[test]
    fn call_exprs() {
        let mut input = r#"f(1,2,3)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        assert_eq!(
            vec![
                (call_expr!(
                    id_expr!("f"),
                    vec![int_expr!(1), int_expr!(2), int_expr!(3),]
                ))
            ],
            program,
        );

        input = r#"f(1)"#;
        parser = Parser::new(Lexer::new(input.into()));
        program = parser.parse_program();
        assert_eq!(
            vec![(call_expr!(id_expr!("f"), vec![int_expr!(1)]))],
            program,
        )
    }
}
