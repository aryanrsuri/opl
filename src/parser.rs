use crate::ast::*;
use crate::lexer::{Lexer, Token};
use std::io::Write;

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
    BitwiseOp,   // & ^
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
        Token::Product | Token::ForwardSlash | Token::Period | Token::Modulo => Precedence::Product,
        Token::Cons | Token::Concat => Precedence::Cons,
        Token::Ampersand | Token::Caret => Precedence::BitwiseOp, // New precedence level needed
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
    pub log_file: Option<std::fs::File>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr: Token::End,
            peek: Token::End,
            errors: Vec::new(),
            log_file: None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn set_log_file(&mut self, file: std::fs::File) {
        self.log_file = Some(file);
    }

    pub fn log(&mut self, message: &str) {
        if let Some(ref mut file) = self.log_file {
            writeln!(file, "{}", message).expect("Failed to write to log file");
        } else {
            println!("{}", message);
        }
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
            } else if !self.errors.is_empty() {
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Comment(_) => Some(Statement::Comment(self.curr.clone())),
            Token::Type => self.parse_type_statement(),
            // TODO: Match
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

    // fn peek_precedence(&mut self) -> Precedence {
    //     token_to_precedence(&self.peek)
    // }

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
        let mut left = match &self.curr {
            Token::Identifier(_) => match self.parse_identifier() {
                Some(ident) => Some(Expression::Identifier(ident)),
                None => None,
            },
            Token::StringLiteral(s) => Some(Expression::Literal(Literal::String(s.clone()))),
            Token::IntegerLiteral(s) => match s.parse::<i64>() {
                Ok(d) => Some(Expression::Literal(Literal::Integer(d))),
                Err(_) => {
                    self.errors.push(ParseError::Log(format!("Could not parse {} as integer", s)));
                    return None;
                }
            },
            Token::FloatLiteral(s) => match s.parse::<f64>() {
                Ok(d) => Some(Expression::Literal(Literal::Float(d))),
                Err(_) => {
                    self.errors.push(ParseError::Log(format!("Could not parse {} as float", s)));
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
            Token::LeftBrace => self.parse_record_expression(),
            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_function_literal(),
            Token::Some => self.parse_some_expression(),
            Token::None => Some(Expression::OptionNone),
            Token::Ok => self.parse_ok_expression(),
            Token::Err => self.parse_err_expression(),
            // Handle built-in types as identifiers in expression context
            Token::StringType | Token::IntType | Token::FloatType | Token::CharType | Token::BoolType | Token::UnitType | Token::List | Token::Option | Token::Result | Token::Map => {
                Some(Expression::Identifier(self.curr.clone()))
            },
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

    fn parse_record_expression(&mut self) -> Option<Expression> {
        let mut fields = Vec::new();
        
        // Consume opening brace
        if !self.curr_token_is(Token::LeftBrace) {
            return None;
        }
        
        while !self.peek_token_is(Token::RightBrace) {
            self.next_token(); // move to field name
            let field_name = self.parse_identifier()?;
            
            // Expect = for assignment
            if !self.expect_peek(Token::Assign) {
                return None;
            }
            
            self.next_token(); // move to value
            let field_value = self.parse_expression(Precedence::Lowest)?;
            fields.push((field_name, field_value));
            
            // Handle comma if present
            if self.peek_token_is(Token::Comma) {
                self.next_token(); // consume comma
            }
        }
        
        // Consume closing brace and semicolon
        if !self.expect_peek(Token::RightBrace) {
            return None;
        }
        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }
        
        Some(Expression::Literal(Literal::Record(fields)))
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        // Syntax: fn <params> -> <body> or fn <params> -> { <body> }
        let params = {
            let mut params = Vec::new();
            while self.peek != Token::Arrow {
                self.next_token();
                if let Token::Identifier(s) = &self.curr {
                    params.push(Token::Identifier(s.clone()));
                } else if let Token::UnitType = &self.curr {
                    params.push(Token::UnitType)
                } 
                else {
                    self.errors.push(ParseError::Log(format!(
                        "expected identifier in function parameters, got {:?}",
                        self.curr
                    )));
                    return None;
                }
                if self.peek == Token::Comma {
                    self.next_token();
                }
            }
            params
        };

        if !self.expect_peek(Token::Arrow) {
            return None;
        }
        self.next_token();

        // Handle both block and single-line expressions
        let body = if self.curr_token_is(Token::LeftBrace) {
            let block = self.parse_block_statement();
            
            // Validate block return semantics
            if let Some(last) = block.last() {
                match last {
                    Statement::Return(_) => (), // Valid return statement
                    Statement::Expression(_) => {
                        if self.peek_token_is(Token::SemiColon) {
                            self.errors.push(ParseError::Log(
                                "Function block's last expression must not end with semicolon".to_string()
                            ));
                            return None;
                        }
                    }
                    _ => {
                        self.errors.push(ParseError::Log(
                            "Function block must end with expression or return statement".to_string()
                        ));
                        return None;
                    }
                }
            } else {
                self.errors.push(ParseError::Log(
                    "Empty function body".to_string()
                ));
                return None;
            }
            block
        } else {
            // Single-line expression becomes implicit return
            let expr = self.parse_expression(Precedence::Lowest)?;
            if !self.peek_token_is(Token::SemiColon) {
                self.errors.push(ParseError::Log(
                    "Single-line function body must end with semicolon".to_string()
                ));
                return None;
            }
            self.next_token(); // consume semicolon
            vec![Statement::Expression(expr)]
        };

        Some(Expression::Function {
            parameters: params,
            body,
        })
    }

    pub fn parse_fn_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut params: Vec<Identifier> = vec![];
        match self.parse_identifier() {
            Some(ident) => params.push(ident),
            None => return None,
        };

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_identifier() {
                Some(ident) => params.push(ident),
                None => return None,
            };
        }

        Some(params)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let mut arguments = vec![];
        
        // Handle empty argument lists
        if self.peek_token_is(Token::RightParen) {
            self.next_token();
            return Some(Expression::Call {
                function: Box::new(function),
                arguments,
            });
        }

        // Parse first argument
        self.next_token();
        if let Some(arg) = self.parse_expression(Precedence::Lowest) {
            arguments.push(arg);
        } else {
            return None;
        }

        // Parse remaining arguments
        while self.peek_token_is(Token::Comma) {
            self.next_token(); // consume comma
            self.next_token(); // move to next argument
            if let Some(arg) = self.parse_expression(Precedence::Lowest) {
                arguments.push(arg);
            } else {
                return None;
            }
        }

        if !self.expect_peek(Token::RightParen) {
            return None;
        }

        Some(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        
        // Consume semicolon if present
        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Statement::Expression(expr))
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
        self.next_token(); 
        
        while !self.curr_token_is(Token::RightBrace) && !self.curr_token_is(Token::End) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }
        
        if self.curr_token_is(Token::RightBrace) {
            self.next_token();
        }
        
        statements
    }

    fn parse_type_statement(&mut self) -> Option<Statement> {
        self.next_token(); // consume 'type'
        
        let name = match self.parse_identifier() {
            Some(ident) => ident,
            None => return None,
        };
        
        if !self.expect_peek(Token::Assign) {
            return None;
        }
        
        // After = we might see a | directly for union types
        if self.peek_token_is(Token::Vbar) {
            self.next_token(); // consume =
            self.next_token(); // consume |
            let type_def = self.parse_union_type()?;
            return Some(Statement::Type(name, type_def));
        }
        
        self.next_token();
        
        let type_def = match self.curr {
            Token::LeftBrace => self.parse_record_type()?,
            _ => self.parse_type_alias()?,
        };
        
        Some(Statement::Type(name, type_def))
    }

    fn parse_union_type(&mut self) -> Option<Type> {
        let mut variants = Vec::new();
        
        loop {
            if let Token::Identifier(_) = &self.curr {
                let variant_name = self.curr.clone();
                
                // Check if variant has associated type (Of)
                let associated_type = if self.peek_token_is(Token::Of) {
                    self.next_token(); // consume 'of'
                    self.next_token(); // move to type
                    Some(self.parse_type_annotation()?)
                } else {
                    None
                };
                
                variants.push((variant_name, associated_type));
                
                // Check for next variant or end
                if !self.peek_token_is(Token::Vbar) {
                    break;
                }
                self.next_token(); // consume |
            } else if let Token::Vbar = &self.curr {
                self.next_token(); // consume the Vbar
            } else {
                self.errors.push(ParseError::Log(format!(
                    "Expected variant name, got {:?}",
                    self.curr
                )));
                return None;
            }
        }
        
        // Expect semicolon at end
        if !self.expect_peek(Token::SemiColon) {
            return None;
        }
        
        Some(Type::Union(variants))
    }

    fn parse_record_type(&mut self) -> Option<Type> {
        let mut fields = Vec::new();
        while !self.peek_token_is(Token::RightBrace) {
            self.next_token();
            // Parse field name
            let field_name = if let Token::Identifier(_) = &self.curr {
                self.curr.clone()
            } else {
                self.errors.push(ParseError::Log(format!(
                    "Expected field name, got {:?}",
                    self.curr
                )));
                return None;
            };
            
            // Expect colon
            if !self.expect_peek(Token::Colon) {
                return None;
            }
            
            self.next_token();
            
            // Parse type annotation
            let type_ann = self.parse_type_annotation()?;
            fields.push((field_name, type_ann));
            
            // Handle comma if present
            if self.peek_token_is(Token::Comma) {
                self.next_token();
            }
        }
        
        // Consume closing brace and expect semicolon
        if !self.expect_peek(Token::RightBrace) || !self.expect_peek(Token::SemiColon) {
            return None;
        }
        
        Some(Type::Record(fields))
    }

    fn parse_type_alias(&mut self) -> Option<Type> {
        // Parse the aliased type
        let type_ann = self.parse_type_annotation()?;
        
        // Expect semicolon
        if !self.expect_peek(Token::SemiColon) {
            return None;
        }
        
        Some(Type::Alias(type_ann))
    }

    fn parse_type_annotation(&mut self) -> Option<Alias> {
        match &self.curr {
            // Handle type variables
            Token::Polymorph => {
                self.next_token(); // consume Polymorph
                if let Token::Identifier(var) = &self.curr {
                    Some(Alias {
                        name: TypeConstructor::TypeVar(var.clone()),
                        parameters: Vec::new(),
                    })
                } else {
                    self.errors.push(ParseError::Log(
                        "Expected identifier after polymorphic type variable".to_string()
                    ));
                    None
                }
            },
            // Handle lowercase primitive types
            Token::IntType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Int),
                parameters: Vec::new(),
            }),
            Token::FloatType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Float),
                parameters: Vec::new(),
            }),
            Token::StringType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::String),
                parameters: Vec::new(),
            }),
            Token::CharType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Char),
                parameters: Vec::new(),
            }),
            Token::BoolType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Bool),
                parameters: Vec::new(),
            }),
            Token::UnitType => Some(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Unit),
                parameters: Vec::new(),
            }),
            // Handle product types for list, map, and option
            Token::List | Token::Map | Token::Option => {
                let constructor = match &self.curr {
                    Token::List => Constructor::List,
                    Token::Map => Constructor::Map,
                    Token::Option => Constructor::Option,
                    _ => unreachable!(),
                };
                
                // Expect * after type constructor
                if !self.expect_peek(Token::Product) {
                    self.errors.push(ParseError::Log(
                        "Expected * after type constructor".to_string()
                    ));
                    return None;
                }
                
                self.next_token(); // move past *
                let param = self.parse_type_annotation()?;
                Some(Alias {
                    name: TypeConstructor::BuiltIn(constructor),
                    parameters: vec![param],
                })
            },
            // Handle result type with two type parameters
            Token::Result => {
                // Expect * after result
                if !self.expect_peek(Token::Product) {
                    self.errors.push(ParseError::Log(
                        "Expected * after result type".to_string()
                    ));
                    return None;
                }
                
                self.next_token(); // move past *
                
                // Parse first type parameter
                let first_param = self.parse_type_annotation()?;
                
                // Expect * between parameters
                if !self.expect_peek(Token::Product) {
                    self.errors.push(ParseError::Log(
                        "Expected * between result type parameters".to_string()
                    ));
                    return None;
                }
                
                self.next_token(); // move past *
                
                // Parse second type parameter
                let second_param = self.parse_type_annotation()?;
                
                Some(Alias {
                    name: TypeConstructor::BuiltIn(Constructor::Result),
                    parameters: vec![first_param, second_param],
                })
            },
            // Custom type identifiers can be any case
            Token::Identifier(_) => {
                let name = self.curr.clone();
                // Check if this is a product type
                if self.peek_token_is(Token::Product) {
                    self.next_token(); // move past identifier
                    self.next_token(); // move past *
                    let param = self.parse_type_annotation()?;
                    Some(Alias {
                        name: TypeConstructor::Custom(name),
                        parameters: vec![param],
                    })
                } else {
                    Some(Alias {
                        name: TypeConstructor::Custom(name),
                        parameters: Vec::new(),
                    })
                }
            },
            Token::LeftBrace => {
                self.errors.push(ParseError::Log(
                    "Inline record types are not allowed. Define a named record type instead.".to_string()
                ));
                None
            },
            _ => {
                self.errors.push(ParseError::Log(format!(
                    "Expected type name, got {:?}",
                    self.curr
                )));
                None
            }
        }
    }

    fn parse_some_expression(&mut self) -> Option<Expression> {
        self.next_token(); // consume 'Some'
        let expr = self.parse_expression(Precedence::Lowest)?;
        Some(Expression::OptionSome(Box::new(expr)))
    }

    fn parse_ok_expression(&mut self) -> Option<Expression> {
        self.next_token(); // consume 'Ok'
        let expr = self.parse_expression(Precedence::Lowest)?;
        Some(Expression::ResultOk(Box::new(expr)))
    }

    fn parse_err_expression(&mut self) -> Option<Expression> {
        self.next_token(); // consume 'Err'
        let expr = self.parse_expression(Precedence::Lowest)?;
        Some(Expression::ResultErr(Box::new(expr)))
    }
}

