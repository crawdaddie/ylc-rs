#[derive(PartialEq, Debug)]
pub enum Token {
    Start, // dummy token
    Lp,    // parens
    Rp,
    LeftBrace,
    RightBrace,
    LeftSq,
    RightSq,
    Comma,

    // operators
    Dot,
    TripleDot,
    Minus,
    Plus,
    Bang,
    Modulo,
    Slash,
    Star,
    Assignment,
    Equality,
    NotEqual,
    Lt,
    Gt,
    Lte,
    Gte,
    // statement terminator
    Nl,

    // special operator
    Pipe,

    Identifier(Vec<char>),
    String(Vec<char>), // literal
    Number,
    Integer,

    // keywords
    Fn,
    Return,
    True,
    False,
    Let,
    If,
    Else,
    While,
    Nil,

    Comment,
    Ws,
    Error,
    Eof,
    Bar,
    Match,
    Extern,
    Struct,
    Type,
    Import,
    Ampersand,
    LogicalAnd,
    Question,
    Colon,
}

pub struct Lexer {
    input: Vec<char>,         // Source code
    pub position: usize,      // Reading position
    pub read_position: usize, // Current moving reading position
    pub ch: char,             // Current read character
}
fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}
impl Lexer {
    pub fn new(input: String) -> Self {
        let chars = input.chars().collect();
        let s = Self {
            input: chars,
            position: 0,
            read_position: 0,
            ch: '0',
        };
        s
    }
    pub fn advance(&mut self, num: usize) {
        if self.read_position >= self.input.len() {
            self.ch = '0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position = self.read_position + num;
    }

    pub fn scan_token(&mut self) -> Token {
        // println!("current char: {:?}", self.ch);
        self.advance(self.skip_whitespace());
        self.advance(self.skip_comment());

        let (tok, num) = match self.ch {
            '=' => (Token::Assignment, 1),
            _ => {
                if is_digit(self.ch) {
                    self.scan_number()
                } else {
                    (Token::Eof, 1)
                }
            }
        };
        self.advance(num);
        tok
    }
    fn scan_number(&self) -> (Token, usize) {}
    fn skip_whitespace(&self) -> usize {
        let mut count = 0;
        for char_index in self.read_position..self.input.len() {
            if self.input[char_index].is_whitespace() {
                count += 1;
            } else {
                break; // Exit the loop when a non-whitespace character is encountered
            }
        }
        count
    }

    fn skip_comment(&self) -> usize {
        let mut count = 0;
        if self.input[self.read_position] == '#' {
            for char_index in self.read_position..self.input.len() {
                if self.input[char_index] == '\n' {
                    break;
                }
                count += 1;
            }
        }
        count
    }
}
