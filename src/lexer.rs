#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    Type,
    Match,
    With,
    Of,
    Raise,

    // Primitive
    True,
    False,
    Char,
    String,
    Int,
    Float,

    // Algebraic
    List,
    Union,
    Record,
    Option,
    Result,
    Ok,
    Error,
    Unit,
    Some,
    None,

    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(String),
    FloatLiteral(String),
    Comment(String),

    // Parsing
    End,
    Illegal,

    // Operators
    Plus,         // +
    Minus,        // -
    Concat,       // ++
    Product,      // *
    ForwardSlash, // /
    Assign,       // =
    Bang,         // !
    Underscore,   // _
    Comma,        // ,
    Equal,        // ==
    DoesNotEqual, // !=
    GreaterThan,  // >
    LessThan,     // <
    GTOrEqual,    // >=
    LTOrEqual,    // <=
    Vbar,         // |
    Pipe,         // |>
    Arrow,        // ->
    Modulo,       // %
    Ampersand,    // &
    Caret,        // ^
    Polymorph,    // 'a

    // Delimiters
    LeftBrace,    // {
    RightBrace,   // }
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Cons,         // ::
    SemiColon,    // ;
    Period,       // .
    Over,         // ..
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    cur: usize,
    next_cur: usize,
    ch: char,
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

fn is_numeric(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alphanumeric(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || is_numeric(c) || c == '_'
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            cur: 0,
            next_cur: 0,
            ch: '\0',
        };
        lexer.read();
        lexer
    }

    pub fn read(&mut self) {
        if self.next_cur >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.next_cur];
        }
        self.cur = self.next_cur;
        self.next_cur += 1;
    }

    pub fn peek(&mut self) -> char {
        self.input[self.next_cur]
    }

    pub fn read_string(&mut self) -> Token {
        self.read();
        // on ident essentially
        let current = self.cur;
        loop {
            if is_alphanumeric(self.ch) || is_whitespace(self.ch) {
                self.read();
            } else {
                break;
            }
        }

        // TODO: Force end delimter to be doublequote or error
        self.read();
        Token::StringLiteral(self.input[current..self.cur - 1].iter().collect::<String>())
    }
    pub fn read_number(&mut self) -> Token {
        // TODO: Enable Floats
        let current = self.cur;
        loop {
            if is_numeric(self.ch) {
                self.read();
            } else {
                break;
            }
        }
        return Token::IntegerLiteral(self.input[current..self.cur].iter().collect::<String>());
    }

    pub fn read_comment(&mut self) -> Token {
        let current = self.cur;
        loop {
            if is_alphanumeric(self.ch) || self.ch == ' ' || self.ch == '\t' {
                self.read();
            } else {
                break;
            }
        }
        Token::Comment(self.input[current..self.cur].iter().collect::<String>())
    }

    pub fn read_identifier(&mut self) -> Token {
        let current = self.cur;
        loop {
            if is_alphanumeric(self.ch) {
                self.read();
            } else {
                break;
            }
        }
        let literal = self.input[current..self.cur].iter().collect::<String>();
        // Keywords
        // Let,
        // Fn,
        // Return,
        // If,
        // Else,
        // Type,
        // Match,
        // With,
        // Of,
        // Raise,
        //
        // // Primitive
        // True,
        // False,
        // Char,
        // String,
        // Int,
        // Float,
        //
        // // Algebraic
        // List,
        // Union,
        // Record,
        // Option,
        // Result,
        // Ok,
        // Error,
        // Unit,
        // Some,
        // None,
        return match literal.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "return" => Token::Return,
            "else" => Token::Else,
            "if" => Token::If,
            "type" => Token::Type,
            "match" => Token::Match,
            "with" => Token::With,
            "of" => Token::Of,
            "Raise" => Token::Raise,
            "Ok" => Token::Ok,
            "Some" => Token::Some,
            "None" => Token::None,
            "Error" => Token::Error,
            "Option" => Token::Option,
            "Result" => Token::Result,

            "True" => Token::True,
            "False" => Token::False,
            // "char" => Token::Char,
            "String" => Token::String,
            "Int" => Token::Int,
            "Float" => Token::Float,
            "List" => Token::List,
            "Union" => Token::Union,
            "Record" => Token::Record,
            _ => Token::Identifier(literal),
        };
    }

    pub fn advance(&mut self) -> Token {
        loop {
            if is_whitespace(self.ch) {
                self.read()
            } else {
                break;
            }
        }
        let token: Token = match self.ch {
            '=' => {
                if self.peek() == '=' {
                    self.read();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            ';' => Token::SemiColon,
            '(' => {
                if self.peek() == ')' {
                    self.read();
                    Token::Unit
                } else {
                    Token::LeftParen
                }
            }
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '+' => {
                if self.peek() == '+' {
                    self.read();
                    Token::Concat
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.peek() == '>' {
                    self.read();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.read();
                    Token::DoesNotEqual
                } else {
                    Token::Bang
                }
            }
            ':' => {
                if self.peek() == ':' {
                    self.read();
                    Token::Cons
                } else {
                    Token::Colon
                }
            }
            '.' => {
                if self.peek() == '.' {
                    self.read();
                    Token::Over
                } else {
                    Token::Period
                }
            }
            '|' => {
                if self.peek() == '>' {
                    self.read();
                    Token::Pipe
                } else {
                    Token::Vbar
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.read();
                    Token::LTOrEqual
                } else {
                    Token::LessThan
                }
            }
            '/' => {
                if self.peek() == '/' {
                    self.read();
                    self.read();
                    let comment = self.read_comment();
                    return comment;
                } else {
                    Token::ForwardSlash
                }
            }
            '[' => Token::LeftBracket,
            '_' => Token::Underscore,
            ']' => Token::RightBracket,
            '*' => Token::Product,
            '&' => Token::Ampersand,
            '%' => Token::Modulo,
            '>' => {
                if self.peek() == '=' {
                    self.read();
                    Token::GTOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '"' => return self.read_string(),
            '0'..='9' => return self.read_number(),
            'a'..='z' | 'A'..='Z' => return self.read_identifier(),
            '\0' => Token::End,
            _ => Token::Illegal,
        };

        self.read();
        token
    }
}
