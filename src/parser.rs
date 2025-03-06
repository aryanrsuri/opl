use crate::ast::*;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Precedence {
    Lowest,
    Pipe,
    Equals,
    LessGreater,
    Sum,
    Product,
    Cons,
    Prefix,
    Call,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken { want: Option<Token>, got: Token },
}

pub type ParseErrors = Vec<ParseError>;

pub struct Parser {
    lexer: Lexer,
    pub curr: Token,
    pub peek: Token,
    pub errors: ParseErrors,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr: Token::End,
            peek: Token::End,
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.curr = self.peek.clone();
        self.peek = self.lexer.advance();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = vec![];
        while self.curr != Token::End {
            if let Some(statement) = self.parse_statement() {
                program.push(statement);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            // Token::Comment(_) => self.parse_comment_statement(),
            // Token::Identifier(_) => self.parse_expression_statement(),
            // Token::Type => self.parse_variant_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Return(expr))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        println!("In let statemnt parsing");
        match &self.peek {
            Token::Identifier(_) => self.next_token(),
            _ => return None,
        }
        let ident = match self.parse_identifier() {
            Some(ident) => ident,
            None => return None,
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Let(ident, expr))
    }

    fn parse_identifier(&self) -> Option<Identifier> {
        match self.curr {
            Token::Identifier(_) => Some(self.curr.clone()),
            _ => None,
        }
    }

    fn curr_token_is(&self, token: Token) -> bool {
        self.curr == token
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(ParseError::UnexpectedToken {
            want: Some(token),
            got: self.peek.clone(),
        });
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek == token
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token.to_owned()) {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        match self.curr {
            Token::Identifier(_) => self.parse_identifier_expression(),
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match self.parse_identifier() {
            Some(ident) => Some(Expression::Identifier(ident)),
            None => None,
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => {
                if self.peek_token_is(Token::SemiColon) {
                    self.next_token();
                }
                Some(Statement::Expression(expr))
            }
            None => None,
        }
    }
}
