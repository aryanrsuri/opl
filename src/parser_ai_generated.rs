// parser.rs
use crate::ast_ai_generated::*;
use crate::lexer::Lexer;
use crate::lexer::Token;

#[derive(PartialOrd, PartialEq, Debug)]
enum Precedence {
    Lowest,
    Pipe,        // |>
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -, ++
    Product,     // *, /, %
    Cons,        // ::
    Prefix,      // -x, !x
    Call,        // function application (juxtaposition)
}

fn token_precedence(tok: &Token) -> Precedence {
    use Token::*;
    match tok {
        Equal | DoesNotEqual => Precedence::Equals,
        LessThan | GreaterThan | LTOrEqual | GTOrEqual => Precedence::LessGreater,
        Plus | Minus | Concat => Precedence::Sum,
        Product | ForwardSlash | Modulo => Precedence::Product,
        Cons => Precedence::Cons,
        Pipe => Precedence::Pipe,
        _ => Precedence::Lowest,
    }
}

/// Determines if a token can begin an expression that qualifies as an argument
/// in a function call (via juxtaposition).
fn is_application_candidate(tok: &Token) -> bool {
    use Token::*;
    match tok {
        Identifier(_) | IntegerLiteral(_) | FloatLiteral(_) | StringLiteral(_) | Boolean(_)
        | LeftParen | LeftBrace | LeftBracket | If | Fn | Match | Ok | Error | Some | None => true,
        _ => false,
    }
}

pub struct Parser {
    l: Lexer,
    pub cur_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let cur = l.advance();
        let peek = l.advance();
        Parser {
            l,
            cur_token: cur,
            peek_token: peek,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.l.advance());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();
        while self.cur_token != Token::End {
            if let Some(stmt) = self.parse_statement() {
                program.push(stmt);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        use Token::*;
        match self.cur_token {
            Let => self.parse_let_statement(),
            Type => self.parse_type_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // Syntax: let <identifier> = <expression> ;
        self.expect_peek_identifier()?;
        let ident = if let Token::Identifier(ref s) = self.cur_token {
            crate::ast_ai_generated::Identifier(s.clone())
        } else {
            return None;
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token(); // move to the expression
        let expr = self.parse_expression(Precedence::Lowest, true)?;
        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        Some(Statement::LetStatement(ident, expr))
    }

    fn parse_type_declaration(&mut self) -> Option<Statement> {
        // Syntax: type <identifier> = { ... } OR type <identifier> = | ... ;
        self.expect_peek_identifier()?;
        let ident = if let Token::Identifier(ref s) = self.cur_token {
            crate::ast_ai_generated::Identifier(s.clone())
        } else {
            return None;
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token(); // move to type definition start

        let type_def = if self.cur_token == Token::LeftBrace {
            self.parse_record_type_definition()
        } else if self.cur_token == Token::Vbar {
            self.parse_union_type_definition()
        } else {
            self.parse_type_annotation().map(TypeDefinition::Alias)
        }?;

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        Some(Statement::TypeDeclaration(ident, type_def))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest, true)?;
        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        Some(Statement::ExpressionStatement(expr))
    }

    /// The `allow_application` flag controls whether function application (juxtaposition)
    /// is enabled. For example, in an if-condition we disable application so that a '{'
    /// is not consumed as an argument.
    fn parse_expression(
        &mut self,
        precedence: Precedence,
        allow_application: bool,
    ) -> Option<Expression> {
        let mut left = match &self.cur_token {
            Token::Identifier(s) => Option::Some(Expression::Identifier(
                crate::ast_ai_generated::Identifier(s.clone()),
            )),
            Token::IntegerLiteral(s) => match s.parse::<i64>() {
                Ok(val) => Option::Some(Expression::IntegerLiteral(val)),
                Err(_) => {
                    self.errors.push(format!("could not parse integer: {}", s));
                    Option::None
                }
            },
            Token::FloatLiteral(s) => match s.parse::<f64>() {
                Ok(val) => Option::Some(Expression::FloatLiteral(val)),
                Err(_) => {
                    self.errors.push(format!("could not parse float: {}", s));
                    Option::None
                }
            },
            Token::StringLiteral(s) => Option::Some(Expression::StringLiteral(s.clone())),
            Token::Boolean(b) => Option::Some(Expression::BooleanLiteral(*b)),
            Token::Bang | Token::Minus => self.parse_prefix_expression(allow_application),
            Token::LeftParen => self.parse_grouped_expression(allow_application),
            Token::If => self.parse_if_expression(),
            Token::Fn => self.parse_function_literal(),
            Token::LeftBrace => self.parse_block_expression(),
            Token::LeftBracket => self.parse_list_literal(),
            Token::Match => self.parse_match_expression(),
            // Special handling for Option/Result constructors:
            Token::Ok => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Prefix, true)?;
                Option::Some(Expression::ResultOk(Box::new(expr)))
            }
            Token::Error => {
                self.next_token();
                // Expect a string literal following Error.
                match &self.cur_token {
                    Token::StringLiteral(s) => {
                        let error_msg = s.clone();
                        self.next_token();
                        Option::Some(Expression::ResultError(Box::new(
                            Expression::StringLiteral(error_msg),
                        )))
                    }
                    _ => {
                        self.errors
                            .push("expected string literal after Error".to_string());
                        Option::None
                    }
                }
            }
            Token::Some => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Prefix, true)?;
                Option::Some(Expression::OptionSome(Box::new(expr)))
            }
            Token::None => Option::Some(Expression::OptionNone),
            _ => {
                self.errors.push(format!(
                    "no prefix parse function for token {:?}",
                    self.cur_token
                ));
                Option::None
            }
        }?;

        // IMPORTANT: If the next token is Else, break out of the loop so that
        // it can be handled as part of an if-expression.
        while self.peek_token != Token::SemiColon && self.peek_token != Token::Else {
            if allow_application && is_application_candidate(&self.peek_token) {
                self.next_token();
                let arg = self.parse_expression(Precedence::Call, allow_application)?;
                left = match left {
                    Expression::FunctionCall {
                        function,
                        mut arguments,
                    } => {
                        arguments.push(arg);
                        Expression::FunctionCall {
                            function,
                            arguments,
                        }
                    }
                    _ => Expression::FunctionCall {
                        function: Box::new(left),
                        arguments: vec![arg],
                    },
                };
                continue;
            } else if token_precedence(&self.peek_token) > precedence {
                self.next_token();
                left = self.parse_infix_expression(left, allow_application)?;
            } else {
                break;
            }
        }
        Option::Some(left)
    }

    fn parse_prefix_expression(&mut self, allow_application: bool) -> Option<Expression> {
        let operator = match &self.cur_token {
            Token::Bang => "!",
            Token::Minus => "-",
            _ => return Option::None,
        }
        .to_string();

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix, allow_application)?;
        Option::Some(Expression::Prefix {
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression,
        allow_application: bool,
    ) -> Option<Expression> {
        let operator = match &self.cur_token {
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Concat => "++",
            Token::Product => "*",
            Token::ForwardSlash => "/",
            Token::Modulo => "%",
            Token::Equal => "==",
            Token::DoesNotEqual => "!=",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::LTOrEqual => "<=",
            Token::GTOrEqual => ">=",
            Token::Cons => "::",
            Token::Pipe => "|>",
            _ => {
                self.errors.push(format!(
                    "no infix parse function for token {:?}",
                    self.cur_token
                ));
                return Option::None;
            }
        }
        .to_string();

        let cur_precedence = token_precedence(&self.cur_token);
        self.next_token();
        let right = self.parse_expression(cur_precedence, allow_application)?;
        Option::Some(Expression::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self, allow_application: bool) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest, allow_application);
        if !self.expect_peek(Token::RightParen) {
            return Option::None;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        // Syntax: if <condition> { <consequence> } else { <alternative> }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest, false)?;
        if !self.expect_peek(Token::LeftBrace) {
            return Option::None;
        }
        let consequence = self.parse_block_expression()?;
        let alternative = if self.peek_token == Token::Else {
            self.next_token(); // consume 'else'
            if !self.expect_peek(Token::LeftBrace) {
                return Option::None;
            }
            Some(Box::new(self.parse_block_expression()?))
        } else {
            None
        };
        Option::Some(Expression::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        // Syntax: fn <params> -> <body>
        let params = {
            let mut params = Vec::new();
            while self.peek_token != Token::Arrow {
                self.next_token();
                if let Token::Identifier(s) = &self.cur_token {
                    params.push(crate::ast_ai_generated::Identifier(s.clone()));
                } else {
                    self.errors.push(format!(
                        "expected identifier in function parameters, got {:?}",
                        self.cur_token
                    ));
                    return Option::None;
                }
                if self.peek_token == Token::Comma {
                    self.next_token();
                }
            }
            params
        };
        if !self.expect_peek(Token::Arrow) {
            return Option::None;
        }
        self.next_token();
        let body = self.parse_expression(Precedence::Lowest, true)?;
        Option::Some(Expression::Function {
            parameters: params,
            body: Box::new(body),
        })
    }

    fn parse_block_expression(&mut self) -> Option<Expression> {
        // Parse a block: { <statements> }
        let mut stmts = Vec::new();
        self.next_token(); // consume '{'
        while self.cur_token != Token::RightBrace && self.cur_token != Token::End {
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            self.next_token();
        }
        // Consume the closing '}' so it doesn't interfere with later parsing.
        self.next_token();
        Option::Some(Expression::Block(stmts))
    }

    fn parse_list_literal(&mut self) -> Option<Expression> {
        let elements = self.parse_expression_list(Token::RightBracket, true)?;
        Option::Some(Expression::ListLiteral(elements))
    }

    /// Parses a comma-separated list of expressions until the given end token.
    fn parse_expression_list(
        &mut self,
        end: Token,
        allow_application: bool,
    ) -> Option<Vec<Expression>> {
        let mut list = Vec::new();
        if self.peek_token == end {
            self.next_token();
            return Option::Some(list);
        }
        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest, allow_application)?);
        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest, allow_application)?);
        }
        if !self.expect_peek(end) {
            return Option::None;
        }
        Option::Some(list)
    }

    fn parse_match_expression(&mut self) -> Option<Expression> {
        // Syntax: match <expr> with | <pattern> -> <expr> | ...
        self.next_token(); // consume 'match'
        let match_expr = self.parse_expression(Precedence::Lowest, true)?;
        if !self.expect_peek(Token::With) {
            return Option::None;
        }
        // Consume the 'with' token so that the next token becomes the first arm introducer.
        self.next_token();
        let mut arms = Vec::new();
        // Now, while the current token is a vertical bar, parse an arm.
        while self.cur_token == Token::Vbar {
            self.next_token(); // consume '|'
            let pattern = self.parse_pattern()?;
            if !self.expect_peek(Token::Arrow) {
                return Option::None;
            }
            self.next_token(); // consume '->'
            let arm_expr = self.parse_expression(Precedence::Lowest, true)?;
            arms.push((pattern, arm_expr));
        }
        Option::Some(Expression::Match {
            expr: Box::new(match_expr),
            arms,
        })
    }

    // --- Pattern parsing ---

    /// Parses a pattern, allowing infix (cons) patterns.
    fn parse_pattern(&mut self) -> Option<Pattern> {
        let mut left = self.parse_simple_pattern()?;
        while token_precedence(&self.peek_token) == Precedence::Cons {
            self.next_token(); // consume the '::' operator
            let operator = "::".to_string();
            self.next_token();
            let right = self.parse_simple_pattern()?;
            left = Pattern::Infix {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        Option::Some(left)
    }

    /// Parses a simple (primary) pattern.
    fn parse_simple_pattern(&mut self) -> Option<Pattern> {
        use crate::lexer::Token;
        match &self.cur_token {
            Token::Identifier(s) => Option::Some(Pattern::Identifier(
                crate::ast_ai_generated::Identifier(s.clone()),
            )),
            Token::IntegerLiteral(s) => {
                s.parse::<i64>()
                    .ok()
                    .map(Pattern::IntegerLiteral)
                    .or_else(|| {
                        self.errors
                            .push(format!("invalid integer in pattern: {}", s));
                        Option::None
                    })
            }
            Token::FloatLiteral(s) => {
                s.parse::<f64>()
                    .ok()
                    .map(Pattern::FloatLiteral)
                    .or_else(|| {
                        self.errors.push(format!("invalid float in pattern: {}", s));
                        Option::None
                    })
            }
            Token::StringLiteral(s) => Option::Some(Pattern::StringLiteral(s.clone())),
            Token::Boolean(b) => Option::Some(Pattern::BooleanLiteral(*b)),
            Token::Underscore => Option::Some(Pattern::Wildcard),
            &Token::Some => {
                let name = crate::ast_ai_generated::Identifier("Some".to_string());
                self.next_token();
                let argument = if is_application_candidate(&self.cur_token) {
                    self.parse_pattern()
                } else {
                    Option::None
                };
                Option::Some(Pattern::Constructor {
                    name,
                    argument: argument.map(Box::new),
                })
            }
            &Token::Ok => {
                let name = crate::ast_ai_generated::Identifier("Ok".to_string());
                self.next_token();
                let argument = if is_application_candidate(&self.cur_token) {
                    self.parse_pattern()
                } else {
                    Option::None
                };
                Option::Some(Pattern::Constructor {
                    name,
                    argument: argument.map(Box::new),
                })
            }
            &Token::Error => {
                let name = crate::ast_ai_generated::Identifier("Error".to_string());
                self.next_token();
                let argument = if is_application_candidate(&self.cur_token) {
                    self.parse_pattern()
                } else {
                    Option::None
                };
                Option::Some(Pattern::Constructor {
                    name,
                    argument: argument.map(Box::new),
                })
            }
            &Token::None => Option::Some(Pattern::Constructor {
                name: crate::ast_ai_generated::Identifier("None".to_string()),
                argument: Option::None,
            }),
            Token::LeftBracket => {
                // Parse empty list pattern: "[]"
                self.next_token();
                if self.cur_token == Token::RightBracket {
                    self.next_token();
                    Option::Some(Pattern::EmptyList)
                } else {
                    self.errors
                        .push("list patterns with elements not implemented".to_string());
                    Option::None
                }
            }
            _ => {
                self.errors
                    .push(format!("unsupported pattern: {:?}", self.cur_token));
                Option::None
            }
        }
    }

    fn parse_record_type_definition(&mut self) -> Option<TypeDefinition> {
        // Parse a record type: { field: type, ... }
        let mut fields = Vec::new();
        self.next_token(); // consume '{'
        while self.cur_token != Token::RightBrace && self.cur_token != Token::End {
            if let Token::Identifier(field) = &self.cur_token {
                let field_ident = crate::ast_ai_generated::Identifier(field.clone());
                if !self.expect_peek(Token::Colon) {
                    return Option::None;
                }
                self.next_token();
                let ty = self.parse_type_annotation()?;
                fields.push((field_ident, ty));
                if self.peek_token == Token::Comma {
                    self.next_token();
                }
            } else {
                self.errors.push(format!(
                    "expected field identifier, got {:?}",
                    self.cur_token
                ));
                return Option::None;
            }
            self.next_token();
        }
        Option::Some(TypeDefinition::Record(fields))
    }

    fn parse_union_type_definition(&mut self) -> Option<TypeDefinition> {
        // Parse a union type: | Variant of Type | Variant | ...
        let mut variants = Vec::new();
        while self.peek_token == Token::Vbar {
            self.next_token(); // consume '|'
            self.next_token();
            if let Token::Identifier(variant) = &self.cur_token {
                let variant_ident = crate::ast_ai_generated::Identifier(variant.clone());
                let assoc_type = if self.peek_token == Token::Of {
                    self.next_token(); // consume 'of'
                    self.next_token();
                    self.parse_type_annotation()
                } else {
                    Option::None
                };
                variants.push((variant_ident, assoc_type));
            } else {
                self.errors.push(format!(
                    "expected variant identifier, got {:?}",
                    self.cur_token
                ));
                return Option::None;
            }
        }
        Option::Some(TypeDefinition::Union(variants))
    }

    fn parse_type_annotation(&mut self) -> Option<crate::ast_ai_generated::TypeAnnotation> {
        // Simple type annotation: an identifier optionally followed by type parameters.
        if let Token::Identifier(name) = &self.cur_token {
            let ident = crate::ast_ai_generated::Identifier(name.clone());
            let params = Vec::new();
            if self.peek_token == Token::LessThan {
                self.next_token();
            }
            Option::Some(crate::ast_ai_generated::TypeAnnotation {
                name: ident,
                parameters: params,
            })
        } else {
            self.errors.push(format!(
                "expected type identifier, got {:?}",
                self.cur_token
            ));
            Option::None
        }
    }

    // Helper: ensure the peek token is what we expect, and advance.
    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token == t {
            self.next_token();
            true
        } else {
            self.errors.push(format!(
                "expected next token to be {:?}, got {:?} instead",
                t, self.peek_token
            ));
            false
        }
    }

    fn expect_peek_identifier(&mut self) -> Option<()> {
        self.next_token();
        if let Token::Identifier(_) = self.cur_token {
            Option::Some(())
        } else {
            self.errors
                .push(format!("expected identifier, got {:?}", self.cur_token));
            Option::None
        }
    }
}
