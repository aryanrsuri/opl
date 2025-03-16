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
                Some(Object::Integer(value)) => result = Some(Object::Integer(value)),
                _ => unreachable!("Only integer works"),
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            _ => unreachable!("Only integer works"),
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.eval_literal(literal)),
            _ => unreachable!("Only integerworks"),
        }
    }

    fn eval_literal(&mut self, literal: &Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(*value),
            _ => unreachable!("Only integer works"),
        }
    }
}
