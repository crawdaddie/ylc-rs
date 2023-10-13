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
pub enum Literal {
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}
pub enum Expression {
    Identifier,
    Literal(Literal),
}
#[derive(Debug)]
pub struct Identifier(pub String);
pub enum Statement {
    Let(Identifier, Option<Expression>, Expression),
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
            _ => self.parse_expression(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        println!("let stmt ");
        // self.advance();
        // let mut t: Identifier;
        // let mut name: Identifier;
        // if let Some(first_id) = self.parse_identifier() {
        //     println!("id {:?}", first_id);
        //     t = first_id
        //     match self.current {
        //         Token::Identifier(ref mut x) => {
        //             name = Identifier(x.clone());
        //         }
        //         Token::Assignment => {
        //             }
        //         _ => {}
        //     }
        // }

        None
    }
    fn parse_identifier(&mut self) -> Option<Identifier> {
        let id = match self.current {
            Token::Identifier(ref mut ident) => Some(Identifier(ident.clone())),
            _ => return None,
        };
        self.advance();
        id
    }

    fn parse_type_declaration(&mut self) -> Option<Statement> {
        None
    }

    fn parse_expression(&mut self) -> Option<Statement> {
        None
    }

    fn print_current(&self) {
        println!("{:?}", self.current);
    }
    fn expect_token(&mut self, tok: Token) -> bool {
        if self.current == tok {
            self.advance();
            return true;
        } else {
            // push error
            return false;
        }
    }
}
