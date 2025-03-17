use crate::ast::*;
use crate::object::Object;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
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
            Statement::Expression(expression) => self.eval_expression(expression),
            _ => unreachable!("[ERR] Only expression statement evaluation works."),
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Option<Object> {
        match expression {
            Expression::If { condition, consequence, alternative } => self.eval_if(condition, consequence, alternative),
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            Expression::OptionNone => Some(Object::OptionNone),
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
            _ => unreachable!("[ERR] Only literal expression evaluation works."),
        }
    }

    // MANIFEST: If Eval
    fn eval_if(&mut self, condition: &Expression, consequence: &Program, alternative: &Option<Program>) -> Option<Object> {
        let condition = match self.eval_expression(condition) {
            Some(condition) => condition,
            None => return None,
        };

        if self.is_truthy(&condition) {
            self.eval_block(consequence)
        } else if let Some(alt) = alternative {
            self.eval_block(alt)
        } else {
            None
        }
    }

    fn eval_block(&mut self, program: &Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program {
            match self.eval_statement(statement) {
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

    // MANIFEST: Infix Eval

    fn eval_infix(&mut self, infix: &Infix, left: Object, right: Object) -> Object {
        match left {
            Object::Integer(left_value) => {
                if let Object::Integer(right_value) = right {
                    self.eval_integer_infix(infix, left_value, right_value)
                } else {
                    Object::TypeError(String::from("Type Mismatch for infix: int infix int -> int"))
                }
            },
            Object::Float(left_value) => {
                if let Object::Float(right_value) = right {
                    self.eval_float_infix(infix, left_value, right_value)
                } else {
                    Object::TypeError(String::from("Type Mismatch for infix: float infix float -> float"))
                }
            },
            Object::Boolean(left_value) => {
                if let Object::Boolean(right_value) = right {
                    self.eval_boolean_infix(infix, left_value, right_value)
                } else {
                    Object::TypeError(String::from("Type Mismatch for infix: bool infix bool -> bool"))
                }
            },
            _ => Object::TypeError(String::from("Type Mismatch for infix: int infix int -> int | bool infix bool -> bool")),
        }
    }

    fn eval_boolean_infix(&mut self, infix: &Infix, left: bool, right: bool) -> Object {
        match infix {
            Infix::Equal=> Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            _ => Object::TypeError(String::from("Type Mismatch for infix: bool infix bool -> bool")),
        }
    }

    fn eval_float_infix(&mut self, infix: &Infix, left: f64, right: f64) -> Object {
        match infix {
            Infix::Plus => Object::Float(left + right),
            Infix::Minus => Object::Float(left - right),
            Infix::Product=> Object::Float(left * right),
            Infix::ForwardSlash => Object::Float(left / right),
            Infix::Modulo => Object::Float(left % right),
            Infix::Equal=> Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            Infix::GreaterThan => Object::Boolean(left > right),
            Infix::LessThan => Object::Boolean(left < right),
            Infix::GTOrEqual => Object::Boolean(left >= right),
            Infix::LTOrEqual=> Object::Boolean(left <= right),
            Infix::Caret | Infix::Cons | Infix::Concat | Infix::Ampersand | Infix::Pipe => Object::TypeError(String::from("Type Mismatch for infix: int infix int -> int")),
        }
    }

    fn eval_integer_infix(&mut self, infix: &Infix, left: i64, right: i64) -> Object {
        match infix {
            Infix::Plus => Object::Integer(left + right),
            Infix::Minus => Object::Integer(left - right),
            Infix::Product=> Object::Integer(left * right),
            Infix::ForwardSlash => Object::Integer(left/ right),
            Infix::Modulo => Object::Integer(left % right),
            Infix::Equal=> Object::Boolean(left == right),
            Infix::DoesNotEqual => Object::Boolean(left != right),
            Infix::GreaterThan => Object::Boolean(left > right),
            Infix::LessThan => Object::Boolean(left < right),
            Infix::GTOrEqual => Object::Boolean(left >= right),
            Infix::LTOrEqual=> Object::Boolean(left <= right),
            Infix::Caret | Infix::Cons | Infix::Concat | Infix::Ampersand | Infix::Pipe => Object::TypeError(String::from("Type Mismatch for infix: int infix int -> int")),
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
            _ => Object::TypeError(String::from(
                "Type Mismatch for (-): int -> int | float -> float",
            )),
        }
    }

    fn eval_minus_prefix(&mut self, object: Object) -> Object {
        match object {
            Object::Integer(value) => Object::Integer(-value),
            Object::Float(value) => Object::Float(-value),
            _ => Object::TypeError(String::from(
                "Type Mismatch for (-): int -> int | float -> float",
            )),
        }
    }

    fn eval_bang_prefix(&mut self, object: Object) -> Object {
        match object {
            Object::Boolean(true) => Object::Boolean(false),
            Object::Boolean(false) => Object::Boolean(true),
            _ => Object::TypeError(String::from("Type Mismatch for (!): bool -> bool")),
        }
    }
}
