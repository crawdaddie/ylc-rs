use crate::lexer;
use crate::symbols::{Numeric, Ttype};
use lexer::{Lexer, Token};
/*
    AST_INTEGER,
    AST_NUMBER,
    AST_BOOL,
    AST_STRING,
    AST_ADD,
    AST_SUBTRACT,
    AST_MUL,
    AST_DIV,
    AST_MAIN,
    AST_UNOP,
    AST_BINOP,
    AST_EXPRESSION,
    AST_STATEMENT,
    AST_STATEMENT_LIST,
    AST_FN_DECLARATION,
    AST_ASSIGNMENT,
    AST_IDENTIFIER,
    AST_SYMBOL_DECLARATION,
    AST_FN_PROTOTYPE,
    AST_CALL,
    AST_TUPLE,
    AST_IF_ELSE,
    AST_MATCH,
    AST_STRUCT,
    AST_TYPE_DECLARATION,
    AST_MEMBER_ACCESS,
    AST_MEMBER_ASSIGNMENT,
    AST_INDEX_ACCESS,
    AST_IMPORT,
    AST_IMPORT_LIB,
    AST_VAR_ARG,
    AST_CURRIED_FN,
    AST_ARRAY,
*/
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
// #[derive(Debug, PartialEq, Clone)]
// pub struct Identifier(pub String);
pub type Identifier = String;
#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Let(
        Identifier,
        Option<Box<Expr>>, // optional explicit type parameter
        //
        Option<Box<Expr>>, // optional immediate assignment expression
    ),
    FnDeclaration(Identifier, Expr),
    TypeDeclaration(Identifier, Box<Ast>),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    //expressions
    Id(Identifier, Ttype),
    Binop(Token, Box<Expr>, Box<Expr>, Ttype),
    Unop(Token, Box<Expr>, Ttype),
    Tuple(Vec<Expr>, Ttype),
    Index(Box<Expr>, Box<Expr>, Ttype),
    Assignment(Box<Expr>, Box<Expr>, Ttype),
    Fn(Vec<Ast>, Option<Box<Expr>>, Vec<Ast>, Ttype),
    Call(Box<Expr>, Vec<Expr>, Ttype),
    Body(Vec<Ast>, Ttype),
    If(Box<Ast>, Vec<Ast>, Option<Vec<Ast>>, Ttype),
    // literals
    Int8(i8),
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}

impl Ast {
    pub fn get_ttype(&self) -> Option<Ttype> {
        match self {
            Ast::Expr(e) => {
                match e {
                    Expr::Id(_, t)
                    | Expr::Binop(_, _, _, t)
                    | Expr::Unop(_, _, t)
                    | Expr::Tuple(_, t)
                    | Expr::Index(_, _, t)
                    | Expr::Assignment(_, _, t)
                    | Expr::Fn(_, _, _, t)
                    | Expr::Body(_, t)
                    | Expr::Call(_, _, t)
                    | Expr::If(_, _, _, t) => Some(t.clone()),

                    // literals
                    Expr::Int8(_) => Some(Ttype::Numeric(Numeric::Int8)),
                    Expr::Integer(_) => Some(Ttype::Numeric(Numeric::Int)),
                    Expr::Number(_) => Some(Ttype::Numeric(Numeric::Num)),
                    Expr::Bool(_) => Some(Ttype::Bool),
                    Expr::String(_) => Some(Ttype::Str),
                }
            }
            _ => None,
        }
    }

    pub fn set_ttype(&mut self, t: Ttype) {
        match self {
            Ast::Expr(e) => match e {
                Expr::Id(_, ref mut ttype)
                | Expr::Binop(_, _, _, ref mut ttype)
                | Expr::Unop(_, _, ref mut ttype)
                | Expr::Tuple(_, ref mut ttype)
                | Expr::Index(_, _, ref mut ttype)
                | Expr::Assignment(_, _, ref mut ttype)
                | Expr::Fn(_, _, _, ref mut ttype)
                | Expr::Body(_, ref mut ttype)
                | Expr::Call(_, _, ref mut ttype)
                | Expr::If(_, _, _, ref mut ttype) => match ttype {
                    Ttype::Var(n) => {
                        if n == "" {
                            *ttype = t
                        }
                    }
                    _ => (),
                },

                _ => (),
            },
            _ => (),
        }
    }
}

fn tvar() -> Ttype {
    Ttype::Var("".into())
}

pub type Block = Vec<Ast>;
pub type Program = Block;

#[macro_export]
macro_rules! int_expr {
    ($value:expr) => {
        Expr::Integer($value)
    };
}

macro_rules! num_expr {
    ($value:expr) => {
        Expr::Number($value)
    };
}

macro_rules! str_expr {
    ($value:expr) => {
        Expr::String($value)
    };
}

#[macro_export]
macro_rules! bool_expr {
    ($value:expr) => {
        Expr::Bool($value)
    };
}
#[macro_export]
macro_rules! id_expr {
    ($value:expr) => {
        Expr::Id($value.into(), tvar())
    };

    ($value:expr, $var: expr) => {
        Expr::Id($value.into(), $var)
    };
}

#[macro_export]
macro_rules! tuple_expr {
    ($values:expr) => {
        Expr::Tuple($values, tvar())
    };

    ($values:expr, $var:expr) => {
        Expr::Tuple($values, $var)
    };
}
#[macro_export]
macro_rules! binop_expr {
    ($op:expr,$l:expr,$r:expr) => {
        Expr::Binop($op, Box::new($l), Box::new($r), tvar())
    };

    ($op:expr, $l:expr, $r:expr, $var:expr) => {
        Expr::Binop($op, Box::new($l), Box::new($r), $var)
    };
}

macro_rules! unop_expr {
    ($op:expr,$operand:expr) => {
        Expr::Unop($op, Box::new($operand), tvar())
    };

    ($op:expr,$operand:expr, $var: expr) => {
        Expr::Unop($op, Box::new($operand), $var)
    };
}

macro_rules! assignment_expr {
    ($assignee:expr,$value:expr) => {
        Expr::Assignment(Box::new($assignee), Box::new($value), tvar())
    };

    ($assignee:expr,$value:expr, $var:expr) => {
        Expr::Assignment(Box::new($assignee), Box::new($value), $var)
    };
}

#[macro_export]
macro_rules! if_expr {
    ($cond: expr, $then: expr, $else: expr) => {
        Expr::If(Box::new($cond), $then, $else, tvar())
    };

    ($cond: expr, $then: expr, $else: expr, $var: expr) => {
        Expr::If(Box::new($cond), $then, $else, $var)
    };
}

macro_rules! call_expr {
    ($callable: expr, $args: expr) => {
        Expr::Call(Box::new($callable), $args, tvar())
    };

    ($callable: expr, $args: expr, $var: expr) => {
        Expr::Call(Box::new($callable), $args, $var)
    };
}

#[macro_export]
macro_rules! ast_wrap {
    ($e: expr) => {
        Ast::Expr($e)
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
            _ => self
                .parse_expression(Precedence::None)
                .map(|e| Ast::Expr(e)),
        }
    }

    fn parse_assignment_expression(&mut self, id: Option<Expr>) -> Option<Expr> {
        self.advance();

        if id.is_none() {
            return None;
        };

        self.parse_expression(Precedence::None)
            .map(|e| assignment_expr!(id.unwrap(), e))
    }
    // fn parse_assignment(&mut self) {
    //     match self.current {
    //         Token::Fn => Some(Statement::FnDeclaration(
    //             id1,
    //             self.parse_expression(Precedence::None),
    //         )),
    //
    //         _ => Some(Statement::Let(
    //             id2.clone().unwrap_or(id1.clone()),
    //             if id2.is_some() { Some(id1) } else { None },
    //             self.parse_expression(Precedence::None),
    //         )),
    //     };
    // }

    fn parse_let_statement(&mut self) -> Option<Ast> {
        self.advance();
        let id1 = self.parse_identifier(); // type??
        let id2 = self.parse_identifier(); // id

        if self.expect_token(Token::Assignment) {
            return match self.current {
                Token::Fn => Some(Ast::FnDeclaration(
                    id1.unwrap(),
                    self.parse_fn_expression().unwrap(),
                )),

                _ => {
                    if id2.is_some() {
                        // id1 is type & id2 is id
                        Some(Ast::Let(
                            id2.unwrap(),
                            id1.map(|i| Box::new(id_expr!(i))),
                            Some(Box::new(self.parse_expression(Precedence::None).unwrap())),
                        ))
                    } else {
                        Some(Ast::Let(
                            id1.unwrap(),
                            None,
                            Some(Box::new(self.parse_expression(Precedence::None).unwrap())),
                        ))
                    }
                }
            };
        } else {
            return Some(Ast::Let(
                id2.clone().unwrap_or(id1.clone().unwrap()),
                id1.map(|i| Box::new(id_expr!(i))),
                None,
            ));
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
            Token::Slash | Token::Star => Precedence::Factor,
            Token::LeftSq => Precedence::Index,
            Token::Lp | Token::Rp | Token::Dot => Precedence::Call,
            Token::Bang => Precedence::Unary,
            Token::LogicalOr => Precedence::Or,
            Token::LogicalAnd => Precedence::And,
            _ => Precedence::None,
        }
    }

    fn parse_infix_expr(&mut self, left: Option<Expr>) -> Option<Expr> {
        let tok = self.current.clone();
        let precedence = self.token_to_precedence(&tok);

        self.advance();
        if left.is_none() {
            return None;
        };

        match self.parse_expression(precedence) {
            Some(expr) => Some(binop_expr!(tok, left.unwrap(), expr)),
            None => None,
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let tok = self.current.clone();

        let precedence = self.token_to_precedence(&tok);
        self.advance();
        match self.parse_expression(precedence) {
            Some(expr) => Some(unop_expr!(tok, expr)),
            None => None,
        }
    }
    fn parse_tuple(&mut self, first: Option<Expr>) -> Option<Expr> {
        if first.is_none() {
            return None;
        }

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

    fn parse_index_expr(&mut self, obj: Option<Expr>) -> Option<Expr> {
        self.advance();
        self.parse_expression(Precedence::None)
            .map(|idx| Expr::Index(Box::new(obj.unwrap()), Box::new(idx), tvar()))
    }

    fn parse_grouping(&mut self) -> Option<Expr> {
        self.advance();
        let expr = self.parse_expression(Precedence::None);

        match self.current {
            Token::Rp => {
                // self.advance();
                expr
            }
            Token::Comma => self.parse_tuple(expr),
            _ => None,
        }
    }
    fn skip_token(&mut self, tok: Token) {
        if &self.current == &tok {
            self.advance();
        }
    }
    fn parse_type_expression(&mut self) -> Option<Expr> {
        None
    }

    fn parse_fn_args(&mut self) -> Vec<Expr> {
        let mut exprs = vec![];
        self.advance();

        while self.current != Token::Rp {
            self.skip_token(Token::Comma);
            if let Some(expr) = self.parse_expression(Precedence::None) {
                exprs.push(expr);
            }
        }
        self.advance(); // move past Rp

        exprs
    }

    fn parse_fn_expression(&mut self) -> Option<Expr> {
        self.advance();

        if self.current != Token::Lp {
            return None;
        };
        let args = self
            .parse_fn_args()
            .iter()
            .map(|e| Ast::Expr(e.clone()))
            .collect();

        let return_type = if self.current != Token::LeftBrace {
            Some(Box::new(self.parse_type_expression().unwrap()))
        } else {
            None
        };

        let body = self.parse_body();
        Some(Expr::Fn(args, return_type, body, tvar()))
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
    fn parse_conditional_expr(&mut self) -> Option<Expr> {
        self.advance();

        let condition = if let Some(cond) = self.parse_expression(Precedence::None) {
            cond
        } else {
            return None;
        };

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

        Some(if_expr!(Ast::Expr(condition), then, elze))
    }
    fn parse_call(&mut self, callee: Option<Expr>) -> Option<Expr> {
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
                println!("{:?} {:?}", c, call_params);
                Some(call_expr!(c, call_params))
            }
            _ => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        // prefix
        let mut left = match self.current {
            Token::Integer(i) => Some(int_expr!(i)),
            Token::Number(i) => Some(num_expr!(i)),
            Token::String(ref mut s) => Some(str_expr!(s.clone())),
            Token::True => Some(bool_expr!(true)),
            Token::False => Some(bool_expr!(false)),
            Token::Identifier(ref mut id) => Some(id_expr!(id.clone())),

            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expr(),
            Token::Lp => self.parse_grouping(),
            Token::If => self.parse_conditional_expr(),

            _ => {
                // self.error_no_prefix_parser();
                return None;
            }
        };

        self.advance();

        while precedence < self.token_to_precedence(&self.current) {
            match self.current {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Star
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
                    // println!("parse call! {:?}", left);
                    left = self.parse_call(left);
                    // return left;
                }
                // Token::Lparen => {
                //     self.bump();
                //     left = self.parse_call_expr(left.unwrap());
                // }
                _ => return left,
            }
        }
        left
    }

    fn print_current(&self) {
        println!("{:?}", self.current);
    }
    fn expect_token(&mut self, tok: Token) -> bool {
        if &self.current == &tok {
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
    let program = p.parse_program();
    program
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let() {
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
    fn test_let_typed() {
        let input = r#"
        let int a = 1 
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
    fn test_assignment() {
        let input = r#"a = 1 + 1"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(Expr::Assignment(
                Box::new(id_expr!("a")),
                Box::new(binop_expr!(Token::Plus, int_expr!(1), int_expr!(1))),
                tvar()
            ))],
            program
        )
    }

    #[test]
    fn test_math_exprs() {
        let tests = vec![
            (
                r#"1 + 7.0"#,
                Ast::Expr(binop_expr!(Token::Plus, int_expr!(1), num_expr!(7.0))),
            ),
            (
                r#"1 * (7.0 + 200)"#,
                Ast::Expr(binop_expr!(
                    Token::Star,
                    int_expr!(1),
                    binop_expr!(Token::Plus, num_expr!(7.0), int_expr!(200))
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input.into()));
            let program = parser.parse_program();
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_grouping() {
        let input = r#"(1 + 1)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(binop_expr!(
                Token::Plus,
                int_expr!(1),
                int_expr!(1)
            ))],
            program
        )
    }

    #[test]
    fn test_tuple() {
        let input = r#"(1, 1)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(tuple_expr!(vec![int_expr!(1), int_expr!(1)]))],
            program
        )
    }
    #[test]
    fn test_tuple_ids() {
        let input = r#"(a, b)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(tuple_expr!(vec![id_expr!("a"), id_expr!("b"),]))],
            program
        )
    }

    #[test]
    fn test_unop() {
        let input = r#"
        -7.0
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(unop_expr!(Token::Minus, num_expr!(7.0)))],
            program
        )
    }

    #[test]
    fn test_unop_bang() {
        let input = r#"
        !1
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(unop_expr!(Token::Bang, int_expr!(1)))],
            program
        )
    }

    #[test]
    fn test_if_else() {
        let input = r#"if (true) {1} else {2}"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(if_expr!(
                Ast::Expr(bool_expr!(true)),
                vec![Ast::Expr(int_expr!(1))],
                Some(vec![Ast::Expr(int_expr!(2))])
            ))],
            program
        );

        // assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn test_if_else_tuple() {
        let input = r#"if (true) {(1, 2)} else {(3, 4)}"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::Expr(if_expr!(
                Ast::Expr(bool_expr!(true)),
                vec![Ast::Expr(tuple_expr!(vec![int_expr!(1), int_expr!(2)]))],
                Some(vec![Ast::Expr(tuple_expr!(vec![
                    int_expr!(3),
                    int_expr!(4)
                ]))])
            ))],
            program
        );

        // assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn test_fn_declaration() {
        let input = r#"
        let f = fn (a, b, c) { a + b + c }
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![Ast::FnDeclaration(
                "f".into(),
                Expr::Fn(
                    vec![
                        Ast::Expr(id_expr!("a")),
                        Ast::Expr(id_expr!("b")),
                        Ast::Expr(id_expr!("c")),
                    ],
                    None,
                    vec![Ast::Expr(binop_expr!(
                        Token::Plus,
                        binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
                        id_expr!("c")
                    ))],
                    tvar(),
                )
            )],
            program
        )
    }

    #[test]
    fn test_call_expr() {
        let input = r#"f()"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(vec![Ast::Expr(call_expr!(id_expr!("f"), vec![]))], program,)
    }
    #[test]
    fn test_call_exprs() {
        let mut input = r#"f(1,2,3)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        assert_eq!(
            vec![Ast::Expr(call_expr!(
                id_expr!("f"),
                vec![int_expr!(1), int_expr!(2), int_expr!(3),]
            ))],
            program,
        );

        input = r#"f(1)"#;
        parser = Parser::new(Lexer::new(input.into()));
        program = parser.parse_program();
        assert_eq!(
            vec![Ast::Expr(call_expr!(id_expr!("f"), vec![int_expr!(1)]))],
            program,
        )
    }
}
