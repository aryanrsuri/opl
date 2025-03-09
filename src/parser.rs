use crate::ast::*;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Precedence {
    Lowest,
    Pipe,        // |>
    Equals,      // == =/=
    LessGreater, // < >
    Sum,         // + - ++
    Product,     // * / %
    Cons,        // ::
    Prefix,      // - ! ~
    Call,        // lambda x
                 // way to index into a list
}

fn token_to_precedence(token: &Token) -> Precedence {
    match token {
        Token::Pipe => Precedence::Pipe,
        Token::Equal | Token::DoesNotEqual => Precedence::Equals,
        Token::LessThan | Token::GreaterThan | Token::GTOrEqual | Token::LTOrEqual => {
            Precedence::LessGreater
        }
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Product | Token::ForwardSlash | Token::Period => Precedence::Product,
        Token::Cons | Token::Concat => Precedence::Cons,
        Token::LeftParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken { want: Option<Token>, got: Token },
    Log(String),
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

    fn peek_precedence(&mut self) -> Precedence {
        token_to_precedence(&self.peek)
    }

    fn curr_precedence(&mut self) -> Precedence {
        token_to_precedence(&self.curr)
    }

    fn no_prefix_parse_fn_error(&mut self, t: Token) {
        self.errors.push(ParseError::Log(format!(
            "No prefix parse function for {:?} found",
            t
        )));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // FIXME: <asuri> Abstract each of these into their own functions.
        let mut left = match &self.curr {
            Token::Identifier(_) => match self.parse_identifier() {
                Some(ident) => Some(Expression::Identifier(ident)),
                None => None,
            },
            Token::IntegerLiteral(s) => match s.parse::<i64>() {
                Ok(d) => Some(Expression::Literal(Literal::Integer(d))),
                Err(_) => {
                    self.errors
                        .push(ParseError::Log(format!("Could not parse {} as integer", s)));
                    return None;
                }
            },
            Token::FloatLiteral(s) => match s.parse::<f64>() {
                Ok(d) => Some(Expression::Literal(Literal::Float(d))),
                Err(_) => {
                    self.errors
                        .push(ParseError::Log(format!("Could not parse {} as float", s)));
                    return None;
                }
            },
            Token::Boolean(b) => Some(Expression::Literal(Literal::Boolean(*b))),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expression(),
            Token::LeftParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest);
                if !self.expect_peek(Token::RightParen) {
                    return None;
                }
                expr
            }
            Token::If => self.parse_if_expression(),
            _ => {
                self.no_prefix_parse_fn_error(self.curr.clone());
                return None;
            }
        };

        // Infix expressions
        while !self.peek_token_is(Token::SemiColon) && precedence < token_to_precedence(&self.peek)
        {
            match self.peek {
                Token::Plus
                | Token::Minus
                | Token::Product
                | Token::ForwardSlash
                | Token::Equal
                | Token::DoesNotEqual
                | Token::LessThan
                | Token::GreaterThan
                | Token::Pipe
                | Token::Cons
                | Token::Concat => {
                    self.next_token();
                    left = self.parse_infix_expression(left.unwrap());
                }
                Token::LeftParen => {
                    self.next_token();
                    left = self.parse_call_expression(left.unwrap());
                }
                // Lbraket => {
                //    self.next_token();
                //   left = self.parse_index_expression(left.unwrap());
                // }
                // Dot => {
                //   self.next_token();
                // left = self.parse_dot_expression(left.unwrap());
                // }
                _ => return left,
            }
        }

        left
    }

    fn parse_call_expression(&mut self, expression: Expression) -> Option<Expression> {
        _ = expression;
        None
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

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = match self.curr {
            Token::Bang => Prefix::Bang,
            Token::Minus => Prefix::Minus,
            Token::Plus => Prefix::Plus,
            _ => return None,
        };

        self.next_token();
        self.parse_expression(Precedence::Prefix)
            .map(|expr| Expression::Prefix(prefix, Box::new(expr)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        //     Caret,
        //     Modulo,
        //     Ampersand,

        let infix = match self.curr {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Product => Infix::Product,
            Token::ForwardSlash => Infix::ForwardSlash,
            Token::Equal => Infix::Equal,
            Token::DoesNotEqual => Infix::DoesNotEqual,
            Token::LessThan => Infix::LessThan,
            Token::GreaterThan => Infix::GreaterThan,
            Token::GTOrEqual => Infix::GTOrEqual,
            Token::LTOrEqual => Infix::LTOrEqual,
            Token::Pipe => Infix::Pipe,
            Token::Cons => Infix::Cons,
            Token::Concat => Infix::Concat,
            _ => return None,
        };

        let precedence = self.curr_precedence();
        self.next_token();
        self.parse_expression(precedence)
            .map(|expr| Expression::Infix(infix, Box::new(left), Box::new(expr)))
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(Token::LeftBrace) {
            return None;
        }
        self.next_token();

        let consequence = self.parse_block_statement();
        let mut alternative = None;
        if self.peek_token_is(Token::Else) {
            self.next_token();
            if !self.expect_peek(Token::LeftBrace) {
                return None;
            }
            self.next_token();
            alternative = Some(self.parse_block_statement());
        }
        Some(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Program {
        let mut statements = vec![];
        while !self.curr_token_is(Token::RightBrace) && !self.curr_token_is(Token::End) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }
        statements
    }
}
