use crate::ast::*;
use crate::object::Object;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
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
        todo!("Infix evaluation")
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
        // NOTE: The value stored is simply the actual integer value, so no -1 * value is necessary
        match object {
            Object::Integer(value) => Object::Integer(value),
            Object::Float(value) => Object::Float(value),
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
