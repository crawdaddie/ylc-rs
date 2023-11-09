#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Start,
    Lp, // parens
    Rp,
    LeftBrace,
    RightBrace,

    LeftSq,
    RightSq,
    Comma,

    // operators
    Dot,
    DoubleDot,
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
    LogicalOr,
    Question,
    Colon,
}

pub struct Lexer {
    input: Vec<char>, // Source code
    // pub position: usize,      // Reading position
    pub read_position: usize, // Current moving reading position
    pub ch: char,             // Current read character
    pub line: usize,
    pub col: usize,
}
fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}
impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let ch = chars[0];
        Self {
            input: chars,
            // position: 0,
            read_position: 0,
            ch,
            line: 0,
            col: 0,
        }
    }

    pub fn advance(&mut self, num: usize) {
        // self.position = self.read_position;
        self.read_position += num;
        if self.read_position >= self.input.len() {
            return;
        }
        self.col += num;
        self.ch = self.input[self.read_position];
    }

    pub fn peek(&mut self) -> char {
        self.input[self.read_position + 1]
    }

    pub fn scan_token(&mut self) -> Token {
        match self.read_position >= self.input.len() {
            true => {
                return Token::Eof;
            }
            false => (),
        }

        self.skip_whitespace();
        self.skip_comment();

        let tok = match self.ch {
            '=' => match self.peek() {
                '=' => {
                    self.advance(1);
                    Token::Equality
                }

                '>' => {
                    self.advance(1);
                    Token::Pipe
                }
                _ => Token::Assignment,
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '/' => Token::Slash,
            '*' => Token::Star,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '(' => Token::Lp,
            ')' => Token::Rp,
            '[' => Token::LeftSq,
            ']' => Token::RightSq,
            '%' => Token::Modulo,
            '|' => match self.peek() {
                '|' => {
                    self.advance(1);
                    Token::LogicalOr
                }
                _ => Token::Bar,
            },
            '\n' => {
                self.col = 0;
                self.line += 1;
                Token::Nl
            }
            '!' => match self.peek() {
                '=' => {
                    self.advance(1);
                    Token::NotEqual
                }
                _ => Token::Bang,
            },
            '?' => Token::Question,
            '.' => match (self.peek(), self.peek()) {
                ('.', '.') => {
                    self.advance(2);
                    Token::TripleDot
                }
                ('.', _) => {
                    self.advance(1);
                    Token::DoubleDot
                }
                (_, _) => Token::Dot,
            },
            // Token::Dot,
            '&' => match self.peek() {
                '&' => {
                    self.advance(1);
                    Token::LogicalAnd
                }
                _ => Token::Ampersand,
            },

            '<' => match self.peek() {
                '=' => {
                    self.advance(1);
                    Token::Lte
                }
                _ => Token::Lt,
            },

            '>' => match self.peek() {
                '=' => {
                    self.advance(1);
                    Token::Gte
                }
                _ => Token::Gt,
            },

            '"' => self.scan_string(),
            _ => {
                if is_digit(self.ch) {
                    self.scan_number()
                } else if self.ch.is_alphabetic() {
                    self.scan_keyword()
                } else {
                    Token::Error
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

        let tok = match num_dots {
            0 => Token::Integer(number_str.parse::<i64>().unwrap()),
            1 => Token::Number(number_str.parse::<f64>().unwrap()),
            _ => Token::Error,
        };
        self.advance(number_str.len() - 1);

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
    fn scan_keyword(&mut self) -> Token {
        let kw = self.read_identifier();
        let tok = match kw.as_str() {
            "fn" => Token::Fn,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "nil" => Token::Nil,
            "match" => Token::Match,
            "extern" => Token::Extern,
            "struct" => Token::Struct,
            "type" => Token::Type,
            "import" => Token::Import,
            id => Token::Identifier(id.into()),
        };
        self.advance(kw.len() - 1);
        tok
    }

    // fn scan_identifier(&mut self) -> Token {
    //     let s = self.read_identifier();
    //     self.advance(s.len());
    //     Token::Identifier(s)
    // }
    fn read_identifier(&self) -> String {
        let mut s = String::new();
        for char_index in self.read_position..self.input.len() {
            let c = self.input[char_index];
            if !c.is_alphanumeric() {
                break;
            }
            s.push(c);
        }
        s
    }
    fn skip_whitespace(&mut self) {
        if self.read_position == self.input.len() {
            return;
        }

        let mut count = 0;
        for char_index in self.read_position..self.input.len() {
            let c = self.input[char_index];
            if c.is_whitespace() {
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
