use crate::{
    lexer::{Lexer, Token},
    symbols::{Numeric, Ttype},
};
use serde::Serialize;

pub static mut TVAR_COUNT: usize = 0;

fn tvar() -> Ttype {
    let tv;
    unsafe {
        tv = Ttype::Var(format!("t{}", TVAR_COUNT));
        TVAR_COUNT += 1;
    };
    tv
}

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
macro_rules! array_expr {
    ($values:expr) => {
        Ast::List($values, tvar())
    };

    ($values:expr, $var:expr) => {
        Ast::List($values, $var)
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
    ($op:expr, $operand:expr) => {
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

#[macro_export]
macro_rules! match_expr {
    ($predicate: expr, $arms: expr) => {
        Ast::Match(Box::new($predicate), $arms, tvar())
    };

    ($predicate: expr, $arms: expr, %var: expr) => {
        Ast::Match(Box::new($predicate), $arms, $var)
    };
}

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
#[derive(Debug, Clone, Serialize)]
pub enum Ast {
    Let(
        Identifier,
        Option<Ttype>,    // optional explicit type parameter
        Option<Box<Ast>>, // optional immediate assignment expression
    ),
    // Destructure(),
    FnDeclaration(Identifier, Box<Ast>),
    TypeDeclaration(Identifier, Box<Ast>),

    //expressions
    Id(Identifier, Ttype),
    Binop(Token, Box<Ast>, Box<Ast>, Ttype),
    Unop(Token, Box<Ast>, Ttype),
    Tuple(Vec<Ast>, Ttype),
    List(Vec<Ast>, Ttype),
    Index(Box<Ast>, Box<Ast>, Ttype),
    Assignment(Box<Ast>, Box<Ast>, Ttype),
    Fn(Vec<Ast>, Vec<Ast>, Ttype),
    Call(Box<Ast>, Vec<Ast>, Ttype),
    If(Box<Ast>, Vec<Ast>, Option<Vec<Ast>>, Ttype),
    Match(Box<Ast>, Vec<(Ast, Vec<Ast>)>, Ttype),
    VarArg,

    // literals
    Int8(i8),
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}
pub type MatchArm = (Ast, Vec<Ast>);

impl Ast {
    pub fn ttype(&self) -> Ttype {
        match self {
            Ast::Let(
                _id,
                _t,          // optional explicit type parameter
                Some(value), // optional immediate assignment expression
            ) => value.ttype(),

            Ast::Let(
                _,
                t,    // optional explicit type parameter
                None, // optional immediate assignment expression
            ) => t.clone().unwrap(),

            Ast::FnDeclaration(_id, fn_expr) => fn_expr.ttype(),
            Ast::TypeDeclaration(_id, type_expr) => type_expr.ttype(),

            Ast::Id(_, t)
            | Ast::Binop(_, _, _, t)
            | Ast::Unop(_, _, t)
            | Ast::Tuple(_, t)
            | Ast::List(_, t)
            | Ast::Index(_, _, t)
            | Ast::Assignment(_, _, t)
            | Ast::Fn(_, _, t)
            | Ast::Call(_, _, t)
            | Ast::If(_, _, _, t)
            | Ast::Match(_, _, t) => t.clone(),

            Ast::Int8(_i) => Ttype::Numeric(Numeric::Int8),
            Ast::Integer(_i) => Ttype::Numeric(Numeric::Int),
            Ast::Number(_f) => Ttype::Numeric(Numeric::Num),
            Ast::Bool(_b) => Ttype::Bool,
            Ast::String(_s) => Ttype::Str,
            Ast::VarArg => Ttype::Tuple(vec![]),
            // Ast::Destructure() =>
            // Ast::MatchArm(predicate, expr) => expr.ttype(),
        }
    }
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Let(id1, _t1, e1), Ast::Let(id2, _t2, e2)) => id1 == id2 && e1 == e2,
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
            (Ast::List(v1, _), Ast::List(v2, _)) => v1 == v2,
            (Ast::Index(e1, e2, _), Ast::Index(e3, e4, _)) => e1 == e3 && e2 == e4,
            (Ast::Assignment(e1, e2, _), Ast::Assignment(e3, e4, _)) => e1 == e3 && e2 == e4,
            (Ast::Fn(v1, v2, _), Ast::Fn(v3, v4, _)) => v1 == v3 && v2 == v4,
            (Ast::Call(e1, v1, _), Ast::Call(e2, v2, _)) => e1 == e2 && v1 == v2,
            // (Ast::Body(v1, _), Ast::Body(v2, _)) => v1 == v2,
            (Ast::If(e1, v1, t1, _), Ast::If(e2, v2, t2, _)) => e1 == e2 && v1 == v2 && t1 == t2,
            (Ast::Int8(i1), Ast::Int8(i2)) => i1 == i2,
            (Ast::Integer(i1), Ast::Integer(i2)) => i1 == i2,
            (Ast::Number(n1), Ast::Number(n2)) => n1 == n2,
            (Ast::Bool(b1), Ast::Bool(b2)) => b1 == b2,
            (Ast::String(s1), Ast::String(s2)) => s1 == s2,
            (Ast::Match(x1, arms1, _), Ast::Match(x2, arms2, _)) => x1 == x2 && arms1 == arms2,
            _ => false,
        }
    }
}

fn identifier_to_type(id: Identifier) -> Ttype {
    match id.as_str() {
        "int8" => Ttype::Numeric(Numeric::Int8),
        "int" => Ttype::Numeric(Numeric::Int),
        "double" => Ttype::Numeric(Numeric::Num),
        "bool" => Ttype::Bool,
        "str" => Ttype::Str,
        s => Ttype::Var(s.into()),
        // _ => tvar(),
    }
}
fn token_to_precedence(tok: &Token) -> Precedence {
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

pub type Program = Vec<Ast>;
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

    fn advance(&mut self) {
        self.previous = self.current.clone();
        self.current = self.lexer.scan_token()
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

    fn parse_lambda_body(&mut self) -> Vec<Ast> {
        let mut body = vec![];
        match self.parse_statement() {
            Some(stmt) => body.push(stmt),
            None => self.advance(),
        }
        if self.current == Token::Nl {
            self.advance();
            body
        } else {
            body
        }
    }

    fn parse_match_body(&mut self) -> Vec<Ast> {
        let mut body = vec![];
        if self.current != Token::LeftBrace {
            // if match body expr doesn't start with a { then it's a single expression and the {}
            // can be omitted
            let expr = self.parse_expression(Precedence::None);
            body.push(expr.unwrap());
        } else {
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }
            self.advance();
        }
        body
    }
    fn parse_statement(&mut self) -> Option<Ast> {
        match &self.current {
            Token::Let => self.parse_let(),
            _ => self.parse_expression(Precedence::None),
        }
    }
    fn parse_identifier(&mut self) -> Identifier {
        let id = match &self.current {
            Token::Identifier(id) => id.clone(),
            _ => panic!(
                "expected identifier at {line}:{col}",
                line = self.lexer.line,
                col = self.lexer.col
            ),
        };
        self.advance();
        id
    }
    fn parse_type_expression(&mut self) -> Option<Ttype> {
        let _tok = self.current.clone();
        self.advance();
        // match &tok {
        //     Token::Identifier(id) => Some(id_expr!(id)),
        //     _ => None,
        // }
        None
    }
    fn parse_let(&mut self) -> Option<Ast> {
        self.advance();
        let id = self.parse_identifier();
        // TODO: allow syntax like
        // let a0, a1, a2 = (1, 2, 3)
        // ie instead of Identifier, allow for Ast::Let(Vec<Ast::Id>, None, TupleExpr)
        // could work to implement let as always
        // Ast::Let(
        //   Vec<Ast>, (- Ast here must be Ast::Id)
        //   None, (- no type necessary, each Ast::Id will have its own type corresponding to Nth of
        //          the TupleExpr)
        //   Box<Ast>
        // )
        //
        let type_param = if self.expect_token(Token::Colon) {
            self.parse_type_expression()
        } else {
            None
        };
        if self.expect_token(Token::Assignment) {
            match self.current {
                Token::Fn => Some(Ast::FnDeclaration(
                    id,
                    Box::new(self.parse_fn_expression()?),
                )),
                _ => Some(Ast::Let(
                    id,
                    type_param,
                    Some(Box::new(self.parse_expression(Precedence::None).unwrap())),
                )),
            }
        } else {
            Some(Ast::Let(id, type_param, None))
        }
    }
    fn parse_typed_identifier(&mut self) -> (Identifier, Option<Identifier>) {
        let id = self.parse_identifier();

        let type_param = if self.expect_token(Token::Colon) {
            let tok = self.current.clone();
            match &tok {
                Token::Identifier(t) => {
                    self.advance();
                    Some(t.clone())
                }
                _ => panic!(
                    "parse error: expected type identifier after : {}:{}",
                    self.lexer.line, self.lexer.col
                ),
            }
        } else {
            None
        };
        (id, type_param)
    }

    fn parse_fn_params(&mut self) -> Vec<Ast> {
        let mut params = vec![];
        self.advance();
        while self.current != Token::Rp {
            self.skip_token(Token::Comma);
            match &self.current {
                Token::Identifier(_) => match self.parse_typed_identifier() {
                    (p, Some(p_type)) => {
                        let t = identifier_to_type(p_type);
                        params.push(Ast::Id(p, t));
                    }
                    (p, None) => params.push(Ast::Id(p, tvar())),
                },
                Token::TripleDot => {
                    params.push(Ast::VarArg);
                    self.advance();
                }
                _ => (),
            }
        }

        self.advance();
        params
    }

    fn parse_fn_expression(&mut self) -> Option<Ast> {
        self.advance();
        if self.current != Token::Lp {
            panic!(
                "fn expression requires a signature after fn {}:{}",
                self.lexer.line, self.lexer.col
            );
        }
        let params = self.parse_fn_params();
        let return_type = if self.expect_token(Token::Colon) {
            identifier_to_type(self.parse_identifier())
        } else {
            tvar()
        };
        let body = match self.current {
            Token::LeftBrace => self.parse_body(),
            // Token::Nl => vec![],
            // _ => self.parse_lambda_body(),
            _ => vec![],
        };

        let mut fn_type: Vec<Ttype> = params.iter().map(|x| x.ttype()).collect();
        fn_type.push(return_type);

        Some(Ast::Fn(params, body, Ttype::Fn(fn_type)))
    }
    fn parse_prefix_expression(&mut self) -> Option<Ast> {
        let tok = self.current.clone();
        let prec = token_to_precedence(&tok);
        self.advance();
        self.parse_expression(prec)
            .map(|expr| unop_expr!(tok, expr))
    }

    fn parse_spread_expression(&mut self) -> Option<Ast> {
        let tok = self.current.clone();
        let prec = token_to_precedence(&tok);
        self.advance();

        self.parse_expression(prec)
            .map(|expr| unop_expr!(tok, expr))
    }

    fn parse_infix_expr(&mut self, left: Option<Ast>) -> Option<Ast> {
        let tok = self.current.clone();
        let prec = token_to_precedence(&tok);

        self.advance();
        left.as_ref()?;

        self.parse_expression(prec)
            .map(|expr| binop_expr!(tok, left.unwrap(), expr))
    }

    fn parse_grouping(&mut self) -> Option<Ast> {
        self.advance();
        let expr = self.parse_expression(Precedence::None);

        match self.current {
            Token::Rp => {
                self.advance();
                expr // return just single expression
            }
            Token::Comma => self.parse_tuple(expr.unwrap()),
            _ => None,
        }
    }

    fn parse_array(&mut self) -> Option<Ast> {
        self.advance();
        // println!("parse array cons {:?}", self.current);
        let mut array_elements: Vec<Ast> = vec![];

        while self.current != Token::RightSq {
            self.skip_token(Token::Comma);
            if let Some(expr) = self.parse_expression(Precedence::None) {
                array_elements.push(expr);
            }
        }
        let arr = array_expr!(array_elements);

        self.advance();
        Some(arr)
    }

    fn parse_tuple(&mut self, first: Ast) -> Option<Ast> {
        let mut exprs = vec![first];
        self.advance();

        while self.current != Token::Rp {
            self.skip_token(Token::Comma);
            if let Some(expr) = self.parse_expression(Precedence::None) {
                exprs.push(expr);
            }
        }
        self.advance();

        Some(tuple_expr!(exprs))
    }

    fn parse_conditional_expression(&mut self) -> Option<Ast> {
        self.advance();

        let condition = self.parse_expression(Precedence::None)?;

        let then = if self.expect_token(Token::LeftBrace) {
            let body = self.parse_body();

            self.advance();
            body
        } else {
            panic!(
                "parse error: expected then block {{ after after if {}:{}",
                self.lexer.line, self.lexer.col
            );
        };

        let elze = if self.expect_token(Token::Else) {
            if !self.expect_token(Token::LeftBrace) {
                panic!(
                    "parse error: expected {{ after else {}:{}",
                    self.lexer.line, self.lexer.col
                );
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
                        call_params.push(expr);
                    }
                }
                Some(call_expr!(c, call_params))
            }
            _ => None,
        }
    }

    fn parse_index_expr(&mut self, obj: Option<Ast>) -> Option<Ast> {
        // println!("parse index expr {:?}", obj);
        self.advance();
        let expr = self
            .parse_expression(Precedence::None)
            .map(|idx| Ast::Index(Box::new(obj.unwrap()), Box::new(idx), tvar()));

        if self.expect_token(Token::RightSq) {
            expr
        } else {
            panic!("Parse error, need to close index expr, expected ]")
        }
    }

    fn parse_assignment_expression(&mut self, id: Option<Ast>) -> Option<Ast> {
        self.advance();

        id.as_ref()?;

        self.parse_expression(Precedence::None)
            .map(|e| assignment_expr!(id.unwrap(), e))
    }
    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let mut pattern = self.parse_expression(Precedence::None)?;
        if self.expect_token(Token::If) {
            pattern = unop_expr!(
                Token::If,
                tuple_expr!(vec![pattern, self.parse_expression(Precedence::None)?])
            );
        }

        if self.expect_token(Token::Pipe) {
            let expr = self.parse_match_body();
            Some((pattern, expr))
        } else {
            panic!("Parse error, match arm needs a final expression");
        }
    }
    fn parse_match_expression(&mut self) -> Option<Ast> {
        self.advance();
        let matched_expr = self.parse_expression(Precedence::None)?;
        // println!("matched expr {:?}", matched_expr);
        let mut match_arms: Vec<MatchArm> = vec![];
        while self.expect_token(Token::Bar) {
            let arm = self.parse_match_arm()?;
            // println!("expr {:?} cur: {:?}", expr, self.current);
            match_arms.push(arm)
        }

        Some(match_expr!(matched_expr, match_arms))
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
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expression(),
            Token::DoubleDot => self.parse_spread_expression(),
            Token::Lp => self.parse_grouping(),
            Token::If => self.parse_conditional_expression(),
            Token::Match => self.parse_match_expression(),
            Token::LeftSq => self.parse_array(),

            _ => {
                return None;
            }
        };

        tok = self.current.clone();

        while precedence < token_to_precedence(&tok) {
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

    fn parse_spread_id(&mut self, id: String) -> Option<Ast> {
        self.advance();
        Some(id_expr!(id))
    }

    fn expect_token(&mut self, tok: Token) -> bool {
        if self.current == tok {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_token(&mut self, tok: Token) {
        if self.current == tok {
            self.advance();
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
                Some(tint()),
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
        );
        assert_eq!(
            program[0].ttype(),
            Ttype::Fn(vec![tint(), tnum(), tbool(), tint()])
        );
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
                Box::new(Ast::Fn(vec![], vec![], tvar(),))
            )],
            program
        );
        assert_eq!(program[0].ttype(), Ttype::Fn(vec![tint()]));
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

    #[test]
    fn call_exprs_add() {
        let input = r#"(f(1) + f(2))"#;
        // let input = r#"1 + 2"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![binop_expr!(
                Token::Plus,
                call_expr!(id_expr!("f"), vec![int_expr!(1)]),
                call_expr!(id_expr!("f"), vec![int_expr!(2)])
            )],
            program,
        );
    }

    #[test]
    fn test_match() {
        let input = r#"
        match x
        | x > 1 -> 1
        | _ -> 2
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![match_expr!(
                id_expr!("x"),
                vec![
                    (
                        binop_expr!(Token::Gt, id_expr!("x"), int_expr!(1)),
                        vec![int_expr!(1)]
                    ),
                    (id_expr!("_"), vec![int_expr!(2)])
                ]
            )],
            program,
        )
    }

    #[test]
    fn test_match_complex() {
        let input = r#"
        match x
        | x if x > 1 -> {1 + x}
        | _ -> 2
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![match_expr!(
                id_expr!("x"),
                vec![
                    (
                        unop_expr!(
                            Token::If,
                            tuple_expr!(vec![
                                id_expr!("x"),
                                binop_expr!(Token::Gt, id_expr!("x"), int_expr!(1))
                            ])
                        ),
                        vec![binop_expr!(Token::Plus, int_expr!(1), id_expr!("x"))]
                    ),
                    (id_expr!("_"), vec![int_expr!(2)])
                ]
            )],
            program,
        )
    }

    #[test]
    fn test_match_complex_array() {
        let input = r#"
        match x
        | [] -> 1
        | [x, ..rest] -> 2
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![match_expr!(
                id_expr!("x"),
                vec![
                    (array_expr!(vec![]), vec![int_expr!(1)]),
                    (
                        array_expr!(vec![
                            id_expr!("x"),
                            unop_expr!(Token::DoubleDot, id_expr!("rest"))
                        ]),
                        vec![int_expr!(2)]
                    )
                ]
            )],
            program,
        )
    }
    #[test]
    fn array_cons() {
        let input = r#"
        [x, ..rest]
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![array_expr!(vec![
                id_expr!("x"),
                unop_expr!(Token::DoubleDot, id_expr!("rest"))
            ])],
            program
        );
    }
}
