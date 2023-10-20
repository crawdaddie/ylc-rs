use crate::lexer;
use lexer::{Lexer, Token};
pub mod types;
use types::*;

macro_rules! int_expr {
    ($value:expr) => {
        Expression::Literal(Literal::Integer($value))
    };
}
macro_rules! num_expr {
    ($value:expr) => {
        Expression::Literal(Literal::Number($value))
    };
}

macro_rules! str_expr {
    ($value:expr) => {
        Expression::Literal(Literal::String($value))
    };
}

macro_rules! bool_expr {
    ($value:expr) => {
        Expression::Literal(Literal::Bool($value))
    };
}
macro_rules! id_expr {
    ($value:expr) => {
        Expression::Id($value.into())
    };
}

macro_rules! binop_expr {
    ($op:expr,$l:expr,$r:expr) => {
        Expression::Binop($op, Box::new($l), Box::new($r))
    };
}

macro_rules! unop_expr {
    ($op:expr,$operand:expr) => {
        Expression::Unop($op, Box::new($operand))
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

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.current {
            Token::Let => self.parse_let_statement(),
            // Token::Identifier(id) => self.parse_assignment_statement(id.clone()),
            Token::Type => self.parse_type_declaration(),
            // Token::Nl => None,
            _ => {
                if let Some(exp) = self.parse_expression(Precedence::None) {
                    Some(Statement::Expression(exp))
                } else {
                    None
                }
            }
        }
    }

    fn parse_assignment_expression(&mut self, id: Option<Expression>) -> Option<Expression> {
        self.advance();
        println!("parse assignment: ");
        self.print_current();

        if id.is_none() {
            return None;
        };

        match self.parse_expression(Precedence::None) {
            Some(expr) => Some(Expression::Assignment(
                Box::new(id.unwrap()),
                Box::new(expr),
            )),
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

    fn parse_let_statement(&mut self) -> Option<Statement> {
        self.advance();
        let id1 = match self.parse_identifier() {
            Some(id) => id.clone(),
            None => return None,
        };
        let id2 = self.parse_identifier();
        if self.expect_token(Token::Assignment) {
            return match self.current {
                Token::Fn => Some(Statement::FnDeclaration(
                    id1,
                    self.parse_fn_expression().unwrap(),
                )),

                _ => Some(Statement::Let(
                    id2.clone().unwrap_or(id1.clone()),
                    if id2.is_some() {
                        Some(id_expr!(id1))
                    } else {
                        None
                    },
                    self.parse_expression(Precedence::None),
                )),
            };
        } else {
            return Some(Statement::Let(
                id2.clone().unwrap_or(id1.clone()),
                if id2.is_some() {
                    Some(id_expr!(id1))
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

    fn parse_type_declaration(&mut self) -> Option<Statement> {
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
    fn parse_call(&self, expr: Expression) -> Option<Expression> {
        return None;
    }

    fn parse_infix_expr(&mut self, left: Option<Expression>) -> Option<Expression> {
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

    fn parse_prefix_expr(&mut self) -> Option<Expression> {
        let tok = self.current.clone();

        let precedence = self.token_to_precedence(&tok);
        self.advance();
        match self.parse_expression(precedence) {
            Some(expr) => Some(unop_expr!(tok, expr)),
            None => None,
        }
    }
    fn parse_tuple(&mut self, first: Option<Expression>) -> Option<Expression> {
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
        self.advance(); // move past Rp

        Some(Expression::Tuple(exprs))
    }

    fn parse_index_expr(&mut self, obj: Option<Expression>) -> Option<Expression> {
        self.advance();
        match self.parse_expression(Precedence::None) {
            Some(index_expr) => Some(Expression::Index(
                Box::new(obj.unwrap()),
                Box::new(index_expr),
            )),
            None => None,
        }
    }

    fn parse_grouping(&mut self) -> Option<Expression> {
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
    fn parse_type_expression(&mut self) -> Option<Expression> {
        None
    }

    fn parse_fn_expression(&mut self) -> Option<Expression> {
        self.advance();

        if self.current != Token::Lp {
            return None;
        };

        let signature = if let Some(sig) = self.parse_grouping() {
            sig
        } else {
            return None;
        };

        let return_type = if self.current != Token::LeftBrace {
            self.parse_type_expression()
        } else {
            None
        };

        let mut body: Vec<Statement> = vec![];
        if self.expect_token(Token::LeftBrace) {
            while self.current != Token::RightBrace {
                match self.parse_statement() {
                    Some(stmt) => body.push(stmt),
                    None => self.advance(),
                }
            }
        }

        Some(Expression::Fn(
            Box::new(signature),
            if return_type.is_none() {
                None
            } else {
                Some(Box::new(return_type.unwrap()))
            },
            body,
        ))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
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
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expr(),
            Token::Lp => self.parse_grouping(),
            Token::Fn => {
                let f = self.parse_fn_expression();
                if f.is_none() {
                    // error - fn parsing failed
                    return None;
                }
                f
            }

            // Token::If => self.parse_if_expr(),
            // Token::Func => self.parse_func_expr(),
            _ => {
                // self.error_no_prefix_parser();
                return None;
            }
        };
        self.advance();

        // infix
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
    println!("{:?}", program);
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
            vec![Statement::Let("a".into(), None, Some(int_expr!(1)))],
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
            vec![Statement::Let(
                "a".into(),
                Some(id_expr!("int")),
                Some(int_expr!(1))
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
            vec![Statement::Expression(Expression::Assignment(
                Box::new(id_expr!("a")),
                Box::new(binop_expr!(Token::Plus, int_expr!(1), int_expr!(1)))
            ))],
            program
        )
    }

    #[test]
    fn test_math_exprs() {
        let tests = vec![
            (
                r#"1 + 7.0"#,
                Statement::Expression(Expression::Binop(
                    Token::Plus,
                    Box::new(int_expr!(1)),
                    Box::new(num_expr!(7.0)),
                )),
            ),
            (
                r#"1 * (7.0 + 200)"#,
                Statement::Expression(Expression::Binop(
                    Token::Star,
                    Box::new(int_expr!(1)),
                    Box::new(Expression::Binop(
                        Token::Plus,
                        Box::new(num_expr!(7.0)),
                        Box::new(int_expr!(200)),
                    )),
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
            vec![Statement::Expression(Expression::Binop(
                Token::Plus,
                Box::new(int_expr!(1)),
                Box::new(int_expr!(1))
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
            vec![Statement::Expression(Expression::Tuple(vec![
                int_expr!(1),
                int_expr!(1)
            ]))],
            program
        )
    }
    #[test]
    fn test_tuple_ids() {
        let input = r#"(a, b)"#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![Statement::Expression(Expression::Tuple(vec![
                id_expr!("a"),
                id_expr!("b"),
            ]))],
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
            vec![Statement::Expression(Expression::Unop(
                Token::Minus,
                Box::new(num_expr!(7.0))
            ))],
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
            vec![Statement::Expression(Expression::Unop(
                Token::Bang,
                Box::new(int_expr!(1))
            ))],
            program
        )
    }

    #[test]
    fn test_fn_declaration() {
        let input = r#"
        let f = fn (a, b, c) { a + b + c }
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![Statement::FnDeclaration(
                "f".into(),
                Expression::Fn(
                    Box::new(Expression::Tuple(vec![
                        id_expr!("a"),
                        id_expr!("b"),
                        id_expr!("c"),
                    ])),
                    None,
                    vec![Statement::Expression(binop_expr!(
                        Token::Plus,
                        binop_expr!(Token::Plus, id_expr!("a"), id_expr!("b")),
                        id_expr!("c")
                    ))]
                )
            )],
            program
        )
    }
}