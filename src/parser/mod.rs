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
pub struct BinopExpr {
    pub token: Token,
    pub left: Box<Ast>,
    pub right: Box<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnopExpr {
    pub token: Token,
    pub operand: Box<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnExpr {
    pub args: Vec<Ast>,
    pub return_type: Option<Box<Ast>>,
    pub body: Vec<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentExpr {
    pub assignee: Box<Ast>,
    pub expr: Box<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpr {
    pub object: Box<Ast>,
    pub index: Box<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleExpr {
    pub members: Vec<Ast>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub condition: Box<Ast>,
    pub then: Vec<Ast>,
    pub elze: Option<Vec<Ast>>,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdExpr {
    pub id: Identifier,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntExpr {
    pub value: i64,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumExpr {
    pub value: f64,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolExpr {
    pub value: bool,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StrExpr {
    pub value: String,
    pub ttype: Ttype,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    // statements
    Let(
        IdExpr,
        Option<Box<Ast>>, // optional explicit type parameter
        //
        Option<Box<Ast>>, // optional immediate assignment expression
    ),
    FnDeclaration(Identifier, FnExpr),
    TypeDeclaration(Identifier, Box<Ast>),

    //expressions
    Id(IdExpr),
    Binop(BinopExpr),
    Unop(UnopExpr),
    Tuple(TupleExpr),
    Index(IndexExpr),
    Assignment(AssignmentExpr),
    Fn(FnExpr),
    If(IfExpr),

    // literals
    Integer(IntExpr),
    Number(NumExpr),
    Bool(BoolExpr),
    String(StrExpr),
}

impl Ast {
    pub fn get_ttype(&self) -> Option<Ttype> {
        match self {
            //expressions
            Ast::Id(e) => Some(e.ttype.clone()),
            Ast::Binop(e) => Some(e.ttype.clone()),
            Ast::Unop(e) => Some(e.ttype.clone()),
            Ast::Tuple(e) => Some(e.ttype.clone()),
            Ast::Index(e) => Some(e.ttype.clone()),
            Ast::Assignment(e) => Some(e.ttype.clone()),
            Ast::Fn(e) => Some(e.ttype.clone()),
            Ast::If(e) => Some(e.ttype.clone()),

            // literals
            Ast::Integer(e) => Some(e.ttype.clone()),
            Ast::Number(e) => Some(e.ttype.clone()),
            Ast::Bool(e) => Some(e.ttype.clone()),
            Ast::String(e) => Some(e.ttype.clone()),

            _ => None,
        }
    }

    pub fn set_ttype(&mut self, t: Ttype) {
        match self {
            //expressions
            Ast::Id(e) => e.ttype = t,
            Ast::Binop(e) => e.ttype = t,
            Ast::Unop(e) => e.ttype = t,
            Ast::Tuple(e) => e.ttype = t,
            Ast::Index(e) => e.ttype = t,
            Ast::Assignment(e) => e.ttype = t,
            Ast::Fn(e) => e.ttype = t,
            Ast::If(e) => e.ttype = t,

            _ => {}
        }
    }
}

fn tvar() -> Ttype {
    Ttype::Var("".into())
}

pub type Block = Vec<Ast>;
pub type Program = Block;

macro_rules! int_expr {
    ($value:expr) => {
        Ast::Integer(IntExpr {
            value: $value,
            ttype: Ttype::Numeric(Numeric::Int),
        })
    };
}
macro_rules! num_expr {
    ($value:expr) => {
        Ast::Number(NumExpr {
            value: $value,
            ttype: Ttype::Numeric(Numeric::Num),
        })
    };
}

macro_rules! str_expr {
    ($value:expr) => {
        Ast::String(StrExpr {
            value: $value,
            ttype: Ttype::Str,
        })
    };
}

macro_rules! bool_expr {
    ($value:expr) => {
        Ast::Bool(BoolExpr {
            value: $value,
            ttype: Ttype::Bool,
        })
    };
}
macro_rules! id_expr {
    ($value:expr) => {
        Ast::Id(IdExpr {
            id: $value.into(),
            ttype: tvar(),
        })
    };
}

macro_rules! tuple_expr {
    ($values:expr) => {
        Ast::Tuple(TupleExpr {
            members: $values,
            ttype: tvar(),
        })
    };
}

macro_rules! binop_expr {
    ($op:expr,$l:expr,$r:expr) => {
        Ast::Binop(BinopExpr {
            token: $op,
            left: Box::new($l),
            right: Box::new($r),
            ttype: tvar(),
        })
    };
}

macro_rules! unop_expr {
    ($op:expr,$operand:expr) => {
        Ast::Unop(UnopExpr {
            token: $op,
            operand: Box::new($operand),
            ttype: tvar(),
        })
    };
}

// pub struct IfExpr {
//     pub condition: Box<Ast>,
//     pub then: Vec<Ast>,
//     pub elze: Option<Vec<Ast>>,
//     pub ttype: Ttype,
// }
macro_rules! if_expr {
    ($cond: expr, $then: expr, $else: expr) => {
        Ast::If(IfExpr {
            condition: $cond,
            then: $then,
            elze: $else,
            ttype: tvar(),
        })
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

        if id.is_none() {
            return None;
        };

        match self.parse_expression(Precedence::None) {
            Some(expr) => Some(Ast::Assignment(AssignmentExpr {
                assignee: Box::new(id.unwrap()),
                expr: Box::new(expr),
                ttype: tvar(),
            })),
            _ => None,
        }
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
        let id1 = self.parse_identifier();
        let id2 = self.parse_identifier();

        if self.expect_token(Token::Assignment) {
            return match self.current {
                Token::Fn => Some(Ast::FnDeclaration(
                    id1.unwrap(),
                    self.parse_fn_expression().unwrap(),
                )),

                _ => Some(Ast::Let(
                    IdExpr {
                        id: id2.clone().unwrap_or(id1.clone().unwrap()),
                        ttype: Ttype::Var("".into()),
                    },
                    if id2.is_some() {
                        Some(Box::new(id_expr!(id1.unwrap())))
                    } else {
                        None
                    },
                    Some(Box::new(self.parse_expression(Precedence::None).unwrap())),
                )),
            };
        } else {
            return Some(Ast::Let(
                IdExpr {
                    id: id2.clone().unwrap_or(id1.clone().unwrap()),
                    ttype: Ttype::Var("".into()),
                },
                if id2.is_some() {
                    Some(Box::new(id_expr!(id1.clone().unwrap())))
                } else {
                    None
                },
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
    fn parse_call(&self, expr: Ast) -> Option<Ast> {
        return None;
    }

    fn parse_infix_expr(&mut self, left: Option<Ast>) -> Option<Ast> {
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

    fn parse_prefix_expr(&mut self) -> Option<Ast> {
        let tok = self.current.clone();

        let precedence = self.token_to_precedence(&tok);
        self.advance();
        match self.parse_expression(precedence) {
            Some(expr) => Some(unop_expr!(tok, expr)),
            None => None,
        }
    }
    fn parse_tuple(&mut self, first: Option<Ast>) -> Option<Ast> {
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

    fn parse_index_expr(&mut self, obj: Option<Ast>) -> Option<Ast> {
        self.advance();
        match self.parse_expression(Precedence::None) {
            Some(index_expr) => Some(Ast::Index(IndexExpr {
                object: Box::new(obj.unwrap()),
                index: Box::new(index_expr),
                ttype: tvar(),
            })),
            None => None,
        }
    }

    fn parse_grouping(&mut self) -> Option<Ast> {
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
    fn parse_type_expression(&mut self) -> Option<Ast> {
        None
    }

    fn parse_fn_args(&mut self) -> Vec<Ast> {
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

    fn parse_fn_expression(&mut self) -> Option<FnExpr> {
        self.advance();

        if self.current != Token::Lp {
            return None;
        };
        let args = self.parse_fn_args();

        let return_type = if self.current != Token::LeftBrace {
            Some(Box::new(self.parse_type_expression().unwrap()))
        } else {
            None
        };

        let mut body: Vec<Ast> = vec![];
        if self.expect_token(Token::LeftBrace) {
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }
        }
        Some(FnExpr {
            args,
            return_type,
            body,
            ttype: tvar(),
        })
    }
    fn parse_body(&mut self) -> Option<Vec<Ast>> {
        if self.expect_token(Token::LeftBrace) {
            let mut body = vec![];
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }
            Some(body)
        } else {
            return None;
        }
    }
    fn parse_conditional_expr(&mut self) -> Option<Ast> {
        self.advance();

        let condition = if let Some(cond) = self.parse_expression(Precedence::None) {
            cond
        } else {
            return None;
        };

        let then = if self.expect_token(Token::LeftBrace) {
            let mut body = vec![];
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }

            self.advance();
            body
        } else {
            return None;
        };

        let elze = if self.expect_token(Token::Else) {
            if !self.expect_token(Token::LeftBrace) {
                return None;
            }

            let mut body = vec![];
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }

            self.advance();

            Some(body)
        } else {
            None
        };

        Some(Ast::If(IfExpr {
            condition: Box::new(condition),
            then: then,
            elze: elze,
            ttype: Ttype::Var("".into()),
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Ast> {
        // prefix
        let mut left = match self.current {
            Token::Integer(i) => Some(int_expr!(i)),
            Token::Number(i) => Some(num_expr!(i)),
            Token::String(ref mut s) => Some(str_expr!(s.clone())),
            Token::True => Some(bool_expr!(true)),
            Token::False => Some(bool_expr!(false)),
            Token::Identifier(ref mut id) => Some(id_expr!(id.clone())),

            // Token::LeftSq => self.parse_array_expr(),
            // Token::Lbrace => self.parse_hash_expr(),
            //
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expr(),
            Token::Lp => self.parse_grouping(),
            Token::Fn => {
                let f = self.parse_fn_expression();
                Some(Ast::Fn(f.unwrap()))
            }

            Token::If => self.parse_conditional_expr(),

            // Token::If => self.parse_if_expr(),
            // Token::Func => self.parse_func_expr(),
            //
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
            vec![Ast::Let(
                IdExpr {
                    id: "a".into(),
                    ttype: Ttype::Var("".into())
                },
                None,
                Some(Box::new(int_expr!(1)))
            )],
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
                IdExpr {
                    id: "a".into(),
                    ttype: Ttype::Var("".into())
                },
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
            vec![Ast::Assignment(AssignmentExpr {
                assignee: Box::new(id_expr!("a")),
                expr: Box::new(binop_expr!(Token::Plus, int_expr!(1), int_expr!(1))),
                ttype: tvar()
            })],
            program
        )
    }

    #[test]
    fn test_math_exprs() {
        let tests = vec![
            (
                r#"1 + 7.0"#,
                Ast::Binop(BinopExpr {
                    token: Token::Plus,
                    left: Box::new(int_expr!(1)),
                    right: Box::new(num_expr!(7.0)),
                    ttype: tvar(),
                }),
            ),
            (
                r#"1 * (7.0 + 200)"#,
                Ast::Binop(BinopExpr {
                    token: Token::Star,
                    left: Box::new(int_expr!(1)),
                    ttype: tvar(),
                    right: Box::new(Ast::Binop(BinopExpr {
                        token: Token::Plus,
                        left: Box::new(num_expr!(7.0)),
                        right: Box::new(int_expr!(200)),
                        ttype: tvar(),
                    })),
                }),
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
            vec![Ast::Binop(BinopExpr {
                token: Token::Plus,
                left: Box::new(int_expr!(1)),
                right: Box::new(int_expr!(1)),
                ttype: tvar()
            })],
            program
        )
    }

    #[test]
    fn test_tuple() {
        let input = r#"(1, 1)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![tuple_expr!(vec![int_expr!(1), int_expr!(1)])], program)
    }
    #[test]
    fn test_tuple_ids() {
        let input = r#"(a, b)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![tuple_expr!(vec![id_expr!("a"), id_expr!("b"),])],
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

        assert_eq!(vec![unop_expr!(Token::Minus, num_expr!(7.0))], program)
    }

    #[test]
    fn test_unop_bang() {
        let input = r#"
        !1
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(vec![unop_expr!(Token::Bang, int_expr!(1))], program)
    }

    #[test]
    fn test_if_else() {
        let input = r#"if (true) {1} else {2}"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();

        assert_eq!(
            vec![if_expr!(
                Box::new(bool_expr!(true)),
                vec![int_expr!(1)],
                Some(vec![int_expr!(2)])
            )],
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
            vec![if_expr!(
                Box::new(bool_expr!(true)),
                vec![tuple_expr!(vec![int_expr!(1), int_expr!(2)])],
                Some(vec![tuple_expr!(vec![int_expr!(3), int_expr!(4)])])
            )],
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
                FnExpr {
                    args: vec![id_expr!("a"), id_expr!("b"), id_expr!("c"),],
                    return_type: None,
                    body: vec![binop_expr!(
                        Token::Plus,
                        binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
                        id_expr!("c")
                    )],
                    ttype: tvar()
                }
            )],
            program
        )
    }
}
