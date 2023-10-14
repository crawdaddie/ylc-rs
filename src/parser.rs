use crate::lexer;
use lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    previous: Token,
    current: Token,
}
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
    Lowest,
    Equality,   // ==
    Comparison, // > or <
    Sum,        // +
    Product,    // *
    Prefix,     // -X or !X
    Call,       // myFunction(x)
    Index,      // array[index]
}
#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Unop {
    Minus,
    Not,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Binop {
    Plus,
    Minus,
    Div,
    Mul,
    Equality,
    NotEqual,
    Gte,
    Gt,
    Lte,
    Lt,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Id(Identifier),
    Literal(Literal),
    Function,
    Binop(Token, Box<Expression>, Box<Expression>),
    Unop(Token, Box<Expression>),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Identifier, Option<Identifier>, Option<Expression>),
    FnDeclaration(Identifier, Option<Expression>),
    TypeDeclaration(Identifier, Expression),
    Expression(Expression),
}
pub type Block = Vec<Statement>;
pub type Program = Block;

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
            self.print_current();
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
        match self.current {
            Token::Let => self.parse_let_statement(),
            Token::Type => self.parse_type_declaration(),
            _ => {
                if let Some(exp) = self.parse_expression(Precedence::Lowest) {
                    Some(Statement::Expression(exp))
                } else {
                    None
                }
            }
        }
    }

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
                    self.parse_expression(Precedence::Lowest),
                )),

                _ => Some(Statement::Let(
                    id2.clone().unwrap_or(id1.clone()),
                    if id2.is_some() { Some(id1) } else { None },
                    self.parse_expression(Precedence::Lowest),
                )),
            };
        } else {
            return Some(Statement::Let(
                id2.clone().unwrap_or(id1.clone()),
                if id2.is_some() { Some(id1) } else { None },
                None,
            ));
        }
    }

    fn parse_identifier(&mut self) -> Option<Identifier> {
        let id = match self.current {
            Token::Identifier(ref mut ident) => Some(Identifier(ident.clone())),
            _ => return None,
        };
        self.advance();
        id
    }

    fn parse_fn(&mut self) -> Expression {
        Expression::Function
    }

    fn parse_type_declaration(&mut self) -> Option<Statement> {
        None
    }
    fn token_to_precedence(&self, tok: &Token) -> Precedence {
        match tok {
            Token::Equality | Token::NotEqual => Precedence::Equality,
            Token::Lt | Token::Lte | Token::Gt | Token::Gte => Precedence::Comparison,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Star => Precedence::Product,
            Token::LeftSq => Precedence::Index,
            Token::Lp => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
    fn parse_call(&self, expr: Expression) -> Option<Expression> {
        return None;
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.current.clone();
        let precedence = self.token_to_precedence(&tok);
        self.advance();
        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Binop(tok, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        println!("parse expr {:?}", self.current);
        // prefix
        let left = match self.current {
            Token::Integer(i) => Expression::Literal(Literal::Integer(i)),
            Token::Number(i) => Expression::Literal(Literal::Number(i)),
            Token::String(ref mut s) => Expression::Literal(Literal::String(s.clone())),
            Token::True => Expression::Literal(Literal::Bool(true)),
            Token::False => Expression::Literal(Literal::Bool(false)),
            Token::Identifier(ref mut id) => Expression::Id(Identifier(id.clone())),
            Token::Fn => self.parse_fn(),
            // Token::Bang => None, negation -> parse next expr
            // Token::Minus => None, negative -> parse next expr
            // Token::Lp => None, ( -> parse grouping
            // Token::LeftSq => None, [ -> parse array
            _ => return None,
        };

        // infix
        self.advance();
        while precedence < self.token_to_precedence(&self.current) {
            match self.current {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Star
                | Token::Equality
                | Token::NotEqual
                | Token::LogicalAnd
                | Token::LogicalOr
                | Token::Lt
                | Token::Lte
                | Token::Gt
                | Token::Gte => return self.parse_infix_expr(left),
                Token::Lp => {
                    self.advance();
                    return self.parse_call(left);
                }
                _ => return Some(left),
                // | Token::Comma,
                // | Token::Colon,
                // | Token::LeftBrace,
                // | Token::RightBrace,
                // | Token::Lp,
                // | Token::Rp,
                // | Token::LeftSq,
                // | Token::RightSq,
            }
        }

        Some(left)
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
            vec![Statement::Let(
                Identifier("a".into()),
                None,
                Some(Expression::Literal(Literal::Integer(1)))
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
            vec![Statement::Let(
                Identifier("a".into()),
                Some(Identifier("int".into())),
                Some(Expression::Literal(Literal::Integer(1)))
            )],
            program
        )
    }

    #[test]
    fn test_math_expr() {
        let input = r#"
        1 + 7.0
        "#;
        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse_program();
        assert_eq!(
            vec![Statement::Let(
                Identifier("a".into()),
                Some(Identifier("int".into())),
                Some(Expression::Literal(Literal::Integer(1)))
            )],
            program
        )
    }
}
