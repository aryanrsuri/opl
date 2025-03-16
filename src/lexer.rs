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
    Use,
    Std,

    // Algebraic
    Union,
    Record,
    Ok,
    Err,
    Some,
    None,
    List,
    Option,
    Result,
    HashMap,

    // Primitive
    IntType,
    FloatType,
    StringType,
    CharType,
    BoolType,
    UnitType,
    
    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(String),
    FloatLiteral(String),
    Comment(String),
    Boolean(bool), 

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
    LeftArrow,    // <-
    Modulo,       // %
    Ampersand,    // &
    Caret,        // ^
    Polymorph,    // 'a
    Cons,         // ::
    Tilde,        // ~

    // Delimiters
    LeftBrace,    // {
    RightBrace,   // }
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    SemiColon,    // ;
    Period,       // .
    Over,         // ..

    // Built in functions
    Map, // map : (a -> b) -> [a] -> [b]
    Filter, // filter : (a -> bool) -> [a] -> [a]
    Fold, // fold : (b -> a -> b) -> b -> [a] -> b
    Any, // any : (a -> bool) -> [a] -> bool
    All, // all : (a -> bool) -> [a] -> bool


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
        // Consume the opening double quote.
        self.read();
        let mut result = String::new();

        // Loop until we hit the closing double quote or end-of-input.
        while self.ch != '"' && self.ch != '\0' {
            // Handle escape sequences.
            if self.ch == '\\' {
                self.read();
                match self.ch {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    // Add additional escape sequences as needed.
                    _ => result.push(self.ch),
                }
            } else {
                // Accept any character inside a string literal, including punctuation.
                result.push(self.ch);
            }
            self.read();
        }

        // Check if we ended with a proper closing quote.
        if self.ch == '"' {
            // Consume the closing double quote.
            self.read();
            Token::StringLiteral(result)
        } else {
            // Unterminated string literal, could also log an error or return a special token.
            Token::Illegal
        }
    }

    pub fn read_comment(&mut self) -> Token {
        // In many languages, a comment goes until the end of the line.
        // We can read until we hit a newline or end-of-input.
        let mut comment = String::new();
        while self.ch != '\n' && self.ch != '\0' {
            // Accept all characters, including punctuation and whitespace,
            // so that comments can include a full sentence or code snippet.
            comment.push(self.ch);
            self.read();
        }
        Token::Comment(comment)
    }

    pub fn read_number(&mut self) -> Token {
        let current = self.cur;
        // Read the integer part.
        while is_numeric(self.ch) {
            self.read();
        }

        // Check for a '.' indicating a float, but only if it's followed by a digit.
        if self.ch == '.' && is_numeric(self.peek()) {
            self.read(); // Consume the '.'
            while is_numeric(self.ch) {
                self.read();
            }
            let float_literal = self.input[current..self.cur].iter().collect::<String>();
            return Token::FloatLiteral(float_literal);
        }

        Token::IntegerLiteral(self.input[current..self.cur].iter().collect::<String>())
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
        return match literal.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "return" => Token::Return,
            "else" => Token::Else,
            "if" => Token::If,
            "std" => Token::Std,
            "use" => Token::Use,
            "type" => Token::Type,
            "match" => Token::Match,
            "with" => Token::With,
            "of" => Token::Of,
            "raise" => Token::Raise,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            // Lowercase primitive types
            "int" => Token::IntType,
            "float" => Token::FloatType,
            "string" => Token::StringType,
            "char" => Token::CharType,
            "bool" => Token::BoolType,
            "unit" => Token::UnitType,
            // Lowercase type constructors
            "list" => Token::List,
            "option" => Token::Option,
            "result" => Token::Result,
            "hashmap" => Token::HashMap,
            // Uppercase type constructors
            //"List" => Token::List,
            // "Option" => Token::Option,
            //"Result" => Token::Result,
            //"Map" => Token::Map,
            //"Record" => Token::Record,
            "Ok" => Token::Ok,
            "Err" => Token::Err,
            "Some" => Token::Some,
            "None" => Token::None,
            // Built in functions
            "map" => Token::Map,
            "filter" => Token::Filter,
            "fold" => Token::Fold,
            "any" => Token::Any,
            "all" => Token::All,
            // TODO: Add the uppercase type constructors only for type module files
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
                    Token::UnitType
                } else {
                    Token::LeftParen
                }
            }
            ')' => Token::RightParen,
            '~' => Token::Tilde,
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
                } 
                else if self.peek() == '-' {
                    self.read();
                    self.read();
                    return self.read_comment()
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
                } else if self.peek() == '-' {
                    self.read();
                    Token::LeftArrow
                } else {
                    Token::LessThan
                }
            }
            '/' => Token::ForwardSlash,
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
