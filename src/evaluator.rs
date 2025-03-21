use crate::ast::Identifier;
use crate::ast::*;
use crate::environment::Env;
use crate::lexer::Token;
use crate::object::Object;
use crate::builtin::println_builtin;
use std::cell::RefCell;
use std::rc::Rc;
pub struct Evaluator {
    pub env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Evaluator { env }
    }

    pub fn is_truthy(&self, object: &Object) -> bool {
        match object {
            Object::Boolean(true) => true,
            Object::Boolean(false) => false,
            _ => true,
        }
    }

    pub fn eval(&mut self, program: &Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program {
            match self.eval_statement(statement) {
                object => result = object,
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> Option<Object> {
        match statement {
            Statement::Let(identifier, expression) => self.eval_let(identifier, expression),
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(expression) => self.eval_return(expression),
            _ => unreachable!("[ERR] Only expression statement evaluation works."),
        }
    }

    fn eval_let(&mut self, identifier: &Identifier, expression: &Expression) -> Option<Object> {
        if let Some(value) = self.eval_expression(expression) {
            if let Token::Identifier(name) = identifier {
                self.env.borrow_mut().set(name.clone(), value);
                None
            } else {
                Some(Object::Error(format!(
                    "Expected identifier, got {:?}",
                    identifier
                )))
            }
        } else {
            Some(Object::Error(format!(
                "Expected value, got {:?}",
                expression
            )))
        }
    }

    fn eval_return(&mut self, expression: &Expression) -> Option<Object> {
        let result = self.eval_expression(expression);
        match result {
            Some(result) => Some(Object::Return(Box::new(result))),
            None => None,
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Option<Object> {
        match expression {
            Expression::Identifier(identifier) => self.eval_identifier(identifier),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if(condition, consequence, alternative),
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            Expression::OptionNone => Some(Object::OptionNone),
            Expression::Function { parameters, body } => Some(Object::Function(
                parameters.clone(),
                body.clone(),
                Rc::clone(&self.env),
            )),
            Expression::Call {
                function,
                arguments,
            } => Some(self.eval_call(function, arguments)),
            Expression::Prefix(prefix, expression) => self
                .eval_expression(&expression) // Evalute the right hand side
                .map(|right| self.eval_prefix(prefix, right)),
            Expression::Infix(infix, left_expression, right_expression) => {
                let left = self.eval_expression(left_expression);
                let right = self.eval_expression(right_expression);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            Expression::BuiltIn { function, arguments } => {
                let args = arguments.iter()
                    .map(|arg| self.eval_expression(arg).unwrap_or(Object::Error("Failed to evaluate argument".to_string())))
                    .collect();
                
                match function {
                    Token::Println => Some(println_builtin(args)),
                    _ => Some(Object::Error("Unknown builtin function".to_string())),
                }
            }
            _ => unreachable!("[ERR] Only literal expression evaluation works."),
        }
    }

    fn eval_call(&mut self, function: &Expression, arguments: &Vec<Expression>) -> Object {
        let arguments = arguments
            .iter()
            .map(|argument| {
                self.eval_expression(argument)
                    .unwrap_or(Object::Error(String::from("Expected value")))
            })
            .collect::<Vec<Object>>();

        let (parameters, body, env) = match self.eval_expression(function) {
            Some(Object::Function(parameters, body, env)) => (parameters, body, env),
            _ => return Object::Error(String::from("Expected function")),
        };

        if parameters.len() != arguments.len() {
            return Object::Error(format!(
                "Expected {} arguments, got {}",
                parameters.len(),
                arguments.len()
            ));
        }

        let mut inner_env = Env::new_with_outer(Rc::clone(&env));

        for (ident, arg) in parameters.iter().zip(arguments.iter()) {
            if let Token::Identifier(name) = ident.clone() {
                inner_env.set(name, arg.clone());
            } else {
                return Object::Error(format!("Expected identifier, got {:?}", ident));
            }
        }

        let current_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(inner_env));
        let object = self.eval_block(&body);
        self.env = current_env;

        match object {
            Some(Object::Return(value)) => *value,
            Some(o) => o,
            None => Object::Error(String::from("Expected return value")),
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> Option<Object> {
        if let Token::Identifier(name) = identifier {
            match self.env.borrow_mut().get(name.clone()) {
                Some(value) => Some(value.clone()),
                None => Some(Object::Error(format!("Undefined variable: {:?}", name))),
            }
        } else {
            Some(Object::Error(format!(
                "Expected identifier, got {:?}",
                identifier
            )))
        }
    }

    fn eval_if(
        &mut self,
        condition: &Expression,
        consequence: &Program,
        alternative: &Option<Program>,
    ) -> Option<Object> {
        let condition = match self.eval_expression(condition) {
            Some(condition) => condition,
            None => return None,
        };

        if self.is_truthy(&condition) {
            self.eval_block(consequence)
        } else if let Some(alt) = alternative {
            self.eval_block(alt)
        } else {
            // If the condition is not truthy and there is no alternative, return None
            // Should this be an error?
            None
        }
    }

    fn eval_block(&mut self, program: &Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program {
            match self.eval_statement(statement) {
                Some(Object::Return(value)) => return Some(Object::Return(value)),
                object => result = object,
            }
        }
        result
    }

    fn eval_literal(&mut self, literal: &Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(*value),
            Literal::Float(value) => Object::Float(*value),
            Literal::String(value) => Object::String(value.clone()),
            Literal::Boolean(value) => Object::Boolean(*value),
            Literal::Unit => Object::Unit,
            _ => unreachable!("[ERR] Only primitive literal evaluation works."),
        }
    }

    fn eval_infix(&mut self, infix: &Infix, left: Object, right: Object) -> Object {
        match left {
            Object::Integer(left_value) => {
                if let Object::Integer(right_value) = right {
                    self.eval_integer_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(format!(
                        "Invalid operator {:?} for type int",
                        infix
                    )))
                }
            }
            Object::Float(left_value) => {
                if let Object::Float(right_value) = right {
                    self.eval_float_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(
                        "Type Mismatch for infix: float infix float -> float",
                    ))
                }
            }
            Object::Boolean(left_value) => {
                if let Object::Boolean(right_value) = right {
                    self.eval_boolean_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(
                        "Type Mismatch for infix: bool infix bool -> bool",
                    ))
                }
            }
            Object::String(left_value) => {
                if let Object::String(right_value) = right {
                    self.eval_string_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(
                        "Type Mismatch for infix: string infix string -> string",
                    ))
                }
            }
            _ => Object::Error(String::from(
                "Type Mismatch for infix: int infix int -> int | bool infix bool -> bool",
            )),
        }
    }

    fn eval_string_infix(&mut self, infix: &Infix, left: String, right: String) -> Object {
        match infix {
            Infix::Concat => {
                let mut concat = left.clone();
                concat.push_str(right.as_str());
                Object::String(concat)
            }
            _ => Object::Error(String::from(format!(
                "Invalid infix operator {:?} for given type: string",
                infix
            ))),
        }
    }

    fn eval_boolean_infix(&mut self, infix: &Infix, left: bool, right: bool) -> Object {
        match infix {
            Infix::Equal => Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            _ => Object::Error(String::from(format!(
                "Invalid infix operator {:?} for given type: bool",
                infix
            ))),
        }
    }

    fn eval_float_infix(&mut self, infix: &Infix, left: f64, right: f64) -> Object {
        match infix {
            Infix::Plus => Object::Float(left + right),
            Infix::Minus => Object::Float(left - right),
            Infix::Product => Object::Float(left * right),
            Infix::ForwardSlash => Object::Float(left / right),
            Infix::Modulo => Object::Float(left % right),
            Infix::Equal => Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            Infix::GreaterThan => Object::Boolean(left > right),
            Infix::LessThan => Object::Boolean(left < right),
            Infix::GTOrEqual => Object::Boolean(left >= right),
            Infix::LTOrEqual => Object::Boolean(left <= right),
            Infix::Caret | Infix::Cons | Infix::Concat | Infix::Ampersand | Infix::Pipe => {
                Object::Error(String::from(format!(
                    "Invalid infix operator {:?} for given type: float",
                    infix
                )))
            }
        }
    }

    fn eval_integer_infix(&mut self, infix: &Infix, left: i64, right: i64) -> Object {
        match infix {
            Infix::Plus => Object::Integer(left + right),
            Infix::Minus => Object::Integer(left - right),
            Infix::Product => Object::Integer(left * right),
            Infix::ForwardSlash => Object::Integer(left / right),
            Infix::Modulo => Object::Integer(left % right),
            Infix::Equal => Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            Infix::GreaterThan => Object::Boolean(left > right),
            Infix::LessThan => Object::Boolean(left < right),
            Infix::GTOrEqual => Object::Boolean(left >= right),
            Infix::LTOrEqual => Object::Boolean(left <= right),
            Infix::Caret | Infix::Cons | Infix::Concat | Infix::Ampersand | Infix::Pipe => {
                Object::Error(String::from(format!(
                    "Invalid infix operator {:?} for given type: int",
                    infix
                )))
            }
        }
    }

    // MANIFEST: Prefix Eval
    fn eval_prefix(&mut self, prefix: &Prefix, object: Object) -> Object {
        match prefix {
            Prefix::Plus => self.eval_plus_prefix(object),
            Prefix::Minus => self.eval_minus_prefix(object),
            Prefix::Bang => self.eval_bang_prefix(object),
        }
    }

    fn eval_plus_prefix(&mut self, object: Object) -> Object {
        match object {
            Object::Integer(value) => Object::Integer(value),
            Object::Float(value) => Object::Float(value),
            _ => Object::Error(String::from(
                "Type Mismatch for (-): int -> int | float -> float",
            )),
        }
    }

    fn eval_minus_prefix(&mut self, object: Object) -> Object {
        match object {
            Object::Integer(value) => Object::Integer(-value),
            Object::Float(value) => Object::Float(-value),
            _ => Object::Error(String::from(
                "Type Mismatch for (-): int -> int | float -> float",
            )),
        }
    }

    fn eval_bang_prefix(&mut self, object: Object) -> Object {
        match object {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            _ => Object::Error(String::from("Type Mismatch for (!): bool -> bool")),
        }
    }
}
