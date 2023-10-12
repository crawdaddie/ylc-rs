use std::cmp;
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

    Identifier(String),
    String(String), // literal
    Number(f64),
    Integer(i64),

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
        let chars: Vec<char> = input.chars().collect();
        let ch = chars[0];
        let s = Self {
            input: chars,
            position: 0,
            read_position: 0,
            ch,
        };
        s
    }

    pub fn advance(&mut self, num: usize) {
        self.position = self.read_position;
        self.read_position = self.read_position + num;
        if (self.read_position >= self.input.len()) {
            return;
        }
        self.ch = self.input[self.read_position]
    }

    pub fn peek(&mut self) -> char {
        self.input[self.read_position + 1]
    }

    pub fn scan_token(&mut self) -> Token {
        if (self.read_position >= self.input.len()) {
            return Token::Eof;
        }

        self.skip_whitespace();
        self.skip_comment();

        let tok = match self.ch {
            '=' => match self.peek() {
                '=' => {
                    self.advance(1);
                    Token::Equality
                }
                _ => Token::Assignment,
            },
            '+' => Token::Plus,
            '"' => self.scan_string(),
            _ => {
                if is_digit(self.ch) {
                    self.scan_number()
                } else {
                    Token::Eof
                }
            }
        };
        self.advance(1);
        tok
    }
    fn scan_number(&mut self) -> Token {
        let mut number_str = String::new();
        let mut num_dots = 0;
        for char_index in self.read_position..self.input.len() {
            let c = self.input[char_index];
            if !(is_digit(c) || c == '.') {
                break;
            }
            if c == '.' {
                num_dots += 1;
            }
            number_str.push(c);
        }

        let (tok, num) = match num_dots {
            0 => (
                Token::Integer(number_str.parse::<i64>().unwrap()),
                number_str.len(),
            ),
            1 => (
                Token::Number(number_str.parse::<f64>().unwrap()),
                number_str.len(),
            ),
            _ => (Token::Error, number_str.len()),
        };
        self.advance(num);
        tok
    }

    fn scan_string(&mut self) -> Token {
        let mut s = String::new();
        self.advance(1);
        for char_index in self.read_position..self.input.len() {
            let c = self.input[char_index];
            if c == '"' && self.input[char_index - 1] != '\\' {
                break;
            }
            s.push(c);
        }

        self.advance(s.len());
        Token::String(s)
    }
    fn skip_whitespace(&mut self) {
        if self.read_position == self.input.len() {
            return;
        }

        let mut count = 0;
        for char_index in self.read_position..self.input.len() {
            if self.input[char_index].is_whitespace() {
                count += 1;
            } else {
                break; // Exit the loop when a non-whitespace character is encountered
            }
        }
        self.advance(count);
    }

    fn skip_comment(&mut self) {
        if self.read_position == self.input.len() {
            return;
        }
        let mut count = 0;

        if self.input[self.read_position] == '#' {
            for char_index in self.read_position..self.input.len() {
                if self.input[char_index] == '\n' {
                    break;
                }
                count += 1;
            }
        }
        self.advance(count);
    }
}
