use crate::lexer::{Lexer, Token};

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

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(String),
}
// #[derive(Debug, PartialEq, Clone)]
// pub struct Identifier(pub String);
pub type Identifier = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Id(Identifier),
    Literal(Literal),
    Binop(Token, Box<Expression>, Box<Expression>),
    Unop(Token, Box<Expression>),
    Tuple(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Fn(Box<Expression>, Option<Box<Expression>>, Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(
        Identifier,
        Option<Expression>, // optional explicit type parameter
        Option<Expression>, // optional immediate assignment expression
    ),

    FnDeclaration(Identifier, Expression),
    TypeDeclaration(Identifier, Expression),
    Expression(Expression),
}
pub type Block = Vec<Statement>;
pub type Program = Block;
