use crate::ast::Identifier;
use crate::ast::*;
use crate::environment::Env;
use crate::lexer::Token;
use crate::object::{Object, TypeDefinitionInner};
use crate::builtin::list::{map_builtin, fold_builtin, filter_builtin, flatten_builtin, flatmap_builtin};
use crate::builtin::std::println_builtin;
use crate::builtin::string::{from_int_builtin, from_float_builtin, from_bool_builtin, length_builtin, split_builtin, string_trim};
use crate::builtin::option::unwrap_builtin;
use crate::builtin::union::{is_variant_builtin, variant_name_builtin, has_value_builtin, value_of_builtin, type_of_builtin};
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

pub struct Evaluator {
    env: Rc<RefCell<Env>>,
    type_env: Rc<RefCell<HashMap<String, Type>>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Evaluator {
            env,
            type_env: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn set_type_env(&mut self, type_env: Rc<RefCell<HashMap<String, Type>>>) {
        self.type_env = type_env;
    }

    pub fn is_truthy(&self, object: &Object) -> bool {
        match object {
            Object::Boolean(value) => *value,
            _ => false  // Non-boolean values should be treated as false in conditionals
        }
    }

    pub fn eval(&mut self, program: &Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program {
            match self.eval_statement(statement) {
                Some(obj) => {
                    if let Object::Error(_) = obj {
                        return Some(obj);
                    }
                    result = Some(obj);
                },
                None => {
                    result = None;
                }
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> Option<Object> {
        match statement {
            Statement::Let(identifier, expression) => self.eval_let(identifier, expression),
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(expression) => self.eval_return(expression),
            Statement::Type(identifier, declaration) => self.eval_type(identifier, declaration),
            Statement::Comment(_) => None,
        }
    }

    fn eval_type(&mut self, identifier: &Identifier, declaration: &Type) -> Option<Object> {
        match declaration {
            Type::Record(fields) => {
                if let Token::Identifier(name) = identifier {
                    self.type_env.borrow_mut().insert(name.clone(), Type::Record(fields.clone()));
                    None
                } else {
                    Some(Object::Error("Invalid record type name".to_string()))
                }
            },
            Type::Union(variants) => {
                if let Token::Identifier(name) = identifier {
                    self.type_env.borrow_mut().insert(name.clone(), Type::Union(variants.clone()));
                    None
                } else {
                    Some(Object::Error("Invalid union type name".to_string()))
                }
            },
            Type::Alias(alias) => {
                if let Token::Identifier(name) = identifier {
                    self.type_env.borrow_mut().insert(name.clone(), Type::Alias(alias.clone()));
                    None
                } else {
                    Some(Object::Error("Invalid type alias name".to_string()))
                }
            },
        }
    }

    fn eval_let(&mut self, identifier: &Identifier, expression: &Expression) -> Option<Object> {
        if let Some(value) = self.eval_expression(expression) {
            if let Token::Identifier(name) = identifier {
                if self.env.borrow().exists_in_current_scope(&name) {
                    return Some(Object::Error(format!(
                        "Cannot redefine variable '{}' in the same scope. Variable shadowing is not allowed.",
                        name
                    )));
                }
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
            Expression::Literal(literal) => self.eval_literal(literal),
            Expression::Range { start, end } => Some(self.eval_range(start, end)),
            Expression::OptionNone => Some(Object::OptionNone),
            Expression::Function { parameters, body } => Some(Object::Function(
                parameters.clone(),
                body.clone(),
                Rc::clone(&self.env),
                Rc::clone(&self.type_env),
            )),
            Expression::Call {
                function,
                arguments,
            } => Some(self.eval_call(function, arguments)),
            Expression::Prefix(prefix, expression) => self
                .eval_expression(&expression)
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
            Expression::DotAccess { object, property } => {
                match self.eval_dot_access(object, property) {
                    Some(Object::Error(msg)) => Some(Object::Error(msg)),
                    Some(obj) => Some(obj),
                    None => None,
                }
            },
            Expression::NamespacedCall { namespace, function, arguments } => {
                self.eval_namespaced_call(namespace, function, arguments)
            },
            Expression::OptionSome(expression) => self.eval_option_some(expression),
            Expression::ResultOk(expression) => self.eval_result_ok(expression),
            Expression::ResultErr(expression) => self.eval_result_err(expression),
            Expression::Variant(variant, value) => self.eval_variant(variant, value),
            _ => unreachable!("[ERR] Only literal expression evaluation works."),
        }
    }

    fn eval_option_some(&mut self, expression: &Expression) -> Option<Object> {
        let value = self.eval_expression(expression);
        match value {
            Some(v) => Some(Object::OptionSome(Box::new(v))),
            None => Some(Object::OptionNone),
        }
    }

    fn eval_result_ok(&mut self, expression: &Expression) -> Option<Object> {   
        let value = self.eval_expression(expression);
        match value {
            Some(v) => Some(Object::ResultOk(Box::new(v))),
            None => Some(Object::ResultErr(Box::new(Object::Error("Failed to evaluate result".to_string())))),
        }
    }

    fn eval_result_err(&mut self, expression: &Expression) -> Option<Object> {
        let value = self.eval_expression(expression);
        match value {
            Some(v) => Some(Object::ResultErr(Box::new(v))),
            None => Some(Object::ResultErr(Box::new(Object::Error("Failed to evaluate result".to_string())))),
        }
    }

    fn eval_range(&mut self, start: &Expression, end: &Expression) -> Object {
        let start_val = self.eval_expression(start).unwrap_or(Object::Error("Failed to evaluate start".to_string()));
        let end_val = self.eval_expression(end).unwrap_or(Object::Error("Failed to evaluate end".to_string()));
        
        match (start_val, end_val) {
            (Object::Integer(start_int), Object::Integer(end_int)) => {
                let mut list = Vec::new();
                for i in start_int..=end_int {
                    list.push(Object::Integer(i));
                }
                Object::List(list)
            },
            (non_int_start, _) if !matches!(non_int_start, Object::Integer(_)) => {
                Object::Error(format!("Range start must be an integer, got {:?}", non_int_start))
            },
            (_, non_int_end) if !matches!(non_int_end, Object::Integer(_)) => {
                Object::Error(format!("Range end must be an integer, got {:?}", non_int_end))
            },
            _ => {
                Object::Error("Error creating range".to_string())
            }
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

        match self.eval_expression(function) {
            Some(Object::Union { type_name, variant, value: _ }) => {
                // Find the type definition for this variant
                let type_env = self.type_env.borrow();
                let type_def = match type_env.get(&type_name) {
                    Some(def) => def,
                    None => return Object::Error(format!("Type {} not found", type_name)),
                };

                // Get the expected type for this variant
                let expected_type = match type_def {
                    Type::Union(variants) => {
                        match variants.iter()
                            .find(|(v, _)| v == &Token::Identifier(variant.clone())) {
                                Some((_, t)) => t.clone(),
                                None => return Object::Error(format!("Variant {} not found in type {}", variant, type_name)),
                            }
                    }
                    _ => return Object::Error(format!("{} is not a union type", type_name)),
                };

                // If the variant has no inner type (None), it shouldn't accept arguments
                if expected_type.is_none() {
                    if !arguments.is_empty() {
                        return Object::Error(format!(
                            "Variant {} does not accept arguments",
                            variant
                        ));
                    }
                    return Object::Union {
                        type_name,
                        variant,
                        value: None,
                    };
                }

                // For variants with inner types, check argument count and type
                if arguments.len() != 1 {
                    return Object::Error(format!(
                        "Expected 1 argument for variant constructor, got {}",
                        arguments.len()
                    ));
                }

                // Check if the argument matches the expected type
                if let Some(expected_alias) = &expected_type {
                    if !self.check_type_match(&arguments[0], expected_alias) {
                        return Object::Error(format!(
                            "Type mismatch for variant {}: expected {:?}, got {:?}",
                            variant, expected_alias, arguments[0]
                        ));
                    }
                }

                Object::Union {
                    type_name,
                    variant,
                    value: Some(Box::new(arguments[0].clone())),
                }
            },
            Some(Object::Function(parameters, body, env, type_env)) => {
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
                let current_type_env = Rc::clone(&self.type_env);
                self.env = Rc::new(RefCell::new(inner_env));
                self.type_env = type_env;
                let object = self.eval_block(&body);
                self.env = current_env;
                self.type_env = current_type_env;

                match object {
                    Some(Object::Return(value)) => *value,
                    Some(o) => o,
                    None => Object::Error(String::from("Expected return value")),
                }
            },
            Some(method @ Object::BuiltinMethod { .. }) => {
                self.eval_method_call(method, arguments)
            },
            Some(Object::Builtin(builtin)) => {
                builtin(arguments)
            },
            _ => Object::Error(String::from("Expected function")),
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> Option<Object> {
        if let Token::Identifier(name) = identifier {
            // First check regular environment for value bindings
            if let Some(value) = self.env.borrow_mut().get(name.clone()) {
                return Some(value);
            }
            
            // Then check if this is a variant constructor
            let type_info = self.type_env.borrow()
                .iter()
                .find(|(_, type_def)| {
                    if let Type::Union(variants) = type_def {
                        variants.iter().any(|(v, _)| v == identifier)
                    } else {
                        false
                    }
                })
                .map(|(name, type_def)| (name.clone(), type_def.clone()));

            if let Some((type_name, type_def)) = type_info {
                // Get variant type to determine if it requires an argument
                if let Type::Union(variants) = type_def {
                    let variant_name = name.clone();
                    Some(Object::Union {
                        type_name,
                        variant: variant_name,
                        value: None, // Value will be provided if called with arguments
                    })
                } else {
                    None
                }
            } else {
                // Finally check type environment for type definitions
                if let Some(type_def) = self.type_env.borrow().get(name) {
                    let result = match type_def {
                        Type::Record(fields) => {
                            // We need to convert (Identifier, Alias) to (String, Type)
                            let type_fields = fields.iter()
                                .filter_map(|(name, alias)| {
                                    if let Token::Identifier(field_name) = name {
                                        Some((field_name.clone(), Type::Alias(alias.clone())))
                                    } else {
                                        None
                                    }
                                })
                                .collect();

                            Object::TypeDefinition {
                                name: name.clone(),
                                inner: TypeDefinitionInner::Record(type_fields),
                            }
                        },
                        Type::Union(variants) => {
                            // We need to convert (Identifier, Option<Alias>) to (String, Option<Type>)
                            let type_variants = variants.iter()
                                .filter_map(|(variant, opt_alias)| {
                                    if let Token::Identifier(variant_name) = variant {
                                        let opt_type = opt_alias.as_ref()
                                            .map(|alias| Type::Alias(alias.clone()));
                                        Some((variant_name.clone(), opt_type))
                                    } else {
                                        None
                                    }
                                })
                                .collect();

                            Object::TypeDefinition {
                                name: name.clone(),
                                inner: TypeDefinitionInner::Union(type_variants),
                            }
                        },
                        Type::Alias(alias) => {
                            Object::TypeDefinition {
                                name: name.clone(),
                                inner: TypeDefinitionInner::Alias(Type::Alias(alias.clone())),
                            }
                        },
                    };
                    Some(result)
                } else {
                    None
                }
            }
        } else {
            None
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
            None
        }
    }

    pub fn eval_block(&mut self, program: &Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program {
            match self.eval_statement(statement) {
                Some(Object::Return(value)) => return Some(Object::Return(value)),
                Some(obj) => {
                    if let Object::Error(_) = obj {
                        return Some(obj);
                    }
                    result = Some(obj);
                },
                None => {
                    result = None;
                }
            }
        }
        result
    }

    fn check_type_match(&self, value: &Object, type_alias: &Alias) -> bool {
        match (value, &type_alias.name) {
            // Built-in type matches
            (Object::Integer(_), TypeConstructor::BuiltIn(Constructor::Int)) => true,
            (Object::Float(_), TypeConstructor::BuiltIn(Constructor::Float)) => true,
            (Object::String(_), TypeConstructor::BuiltIn(Constructor::String)) => true,
            (Object::Boolean(_), TypeConstructor::BuiltIn(Constructor::Bool)) => true,
            (Object::List(_), TypeConstructor::BuiltIn(Constructor::List)) => true,
            (Object::Unit, TypeConstructor::BuiltIn(Constructor::Unit)) => true,
            
            // Tuple type match
            (Object::Tuple(elements), TypeConstructor::BuiltIn(Constructor::Tuple)) => 
                elements.len() == type_alias.parameters.len() && 
                elements.iter().zip(type_alias.parameters.iter()).all(|(value, param_type)| 
                    self.check_type_match(value, param_type)
                ),
            
            // Result type matches
            (Object::ResultOk(inner), TypeConstructor::BuiltIn(Constructor::Result)) => 
                type_alias.parameters.first().map_or(false, |param_type| 
                    self.check_type_match(inner, param_type)
                ),
            (Object::ResultErr(inner), TypeConstructor::BuiltIn(Constructor::Result)) => 
                type_alias.parameters.get(1).map_or(false, |param_type| 
                    self.check_type_match(inner, param_type)
                ),
            
            (Object::OptionSome(inner), TypeConstructor::BuiltIn(Constructor::Option)) => 
                type_alias.parameters.first().map_or(false, |param_type| 
                    self.check_type_match(inner, param_type)
                ),
            (Object::OptionNone, TypeConstructor::BuiltIn(Constructor::Option)) => true,
            
            // Custom type matches
            (value, TypeConstructor::Custom(name)) => 
                if let Token::Identifier(ident) = name {
                    self.type_env.borrow().get(ident).map_or(false, |type_def| 
                        match type_def {
                            Type::Alias(alias) => self.check_type_match(value, alias),
                            Type::Record(fields) => 
                                if let Object::Record { fields: value_fields, .. } = value {
                                    fields.iter().all(|(field_name, field_type)| 
                                        value_fields.iter()
                                            .find(|(name, _)| name == field_name)
                                            .map_or(false, |(_, val)| self.check_type_match(val, field_type))
                                    )
                                } else { false },
                            Type::Union(variants) => 
                                if let Object::Union { type_name, variant, value } = value {
                                    type_name == ident && variants.iter()
                                        .find(|(v, _)| v == &Token::Identifier(variant.clone()))
                                        .map_or(false, |(_, variant_type)| 
                                            match (variant_type, value) {
                                                (None, None) => true, // Empty variant
                                                (Some(t), Some(box_val)) => self.check_type_match(box_val, t),
                                                _ => false  // Mismatch between expected and actual value
                                            }
                                        )
                                } else { false }
                        }
                    )
                } else { false },
            _ => false
        }
    }

    fn eval_literal(&mut self, literal: &Literal) -> Option<Object> {
        match literal {
            Literal::Tuple(elements) => {
                let mut evaluated_elements = Vec::new();
                for element in elements {
                    match self.eval_expression(element) {
                        Some(value) => evaluated_elements.push(value),
                        None => return Some(Object::Error("Failed to evaluate tuple element".to_string())),
                    }
                }
                Some(Object::Tuple(evaluated_elements))
            },
            Literal::Record(fields) => {
                // First, find the record type that matches these fields
                let mut matching_type = None;
                let mut type_definition = None;
                
                // Get field names from the literal
                let literal_field_names: Vec<String> = fields.iter()
                    .filter_map(|(name, _)| {
                        if let Token::Identifier(n) = name {
                            Some(n.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                // Look through all type definitions in the type environment
                {
                    let type_env = self.type_env.borrow();
                    for (type_name, type_def) in type_env.iter() {
                        if let Type::Record(type_fields) = type_def {
                            // Get field names from the type definition
                            let type_field_names: Vec<String> = type_fields.iter()
                                .filter_map(|(name, _)| {
                                    if let Token::Identifier(n) = name {
                                        Some(n.clone())
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            
                            if type_field_names.len() == literal_field_names.len() 
                               && type_field_names.iter().all(|f| literal_field_names.contains(f)) {
                                matching_type = Some(type_name.clone());
                                type_definition = Some(type_fields.clone());
                                break;
                            }
                        }
                    }
                }

                // Get the type definition
                let (type_name, type_fields) = match (matching_type, type_definition) {
                    (Some(name), Some(fields)) => (name, fields),
                    _ => return Some(Object::Error("Record literal doesn't match any defined record type".to_string())),
                };

                // Evaluate the fields and check types
                let mut evaluated_fields = Vec::new();
                for (field_name, field_value) in fields {
                    // Find the expected type for this field
                    let expected_type = type_fields.iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, type_alias)| type_alias);

                    // Evaluate the field value
                    match self.eval_expression(field_value) {
                        Some(value) => {
                            // Check if the value matches the expected type
                            if let Some(expected_type) = expected_type {
                                if !self.check_type_match(&value, expected_type) {
                                    return Some(Object::Error(format!(
                                        "Type mismatch for field '{}': expected {:?}, got {:?}",
                                        field_name, expected_type, value
                                    )));
                                }
                            }
                            evaluated_fields.push((field_name.clone(), value));
                        }
                        None => return Some(Object::Error(format!("Failed to evaluate field {}", field_name))),
                    }
                }

                Some(Object::Record {
                    type_name,
                    fields: evaluated_fields,
                })
            },
            Literal::Integer(value) => Some(Object::Integer(*value)),
            Literal::Float(value) => Some(Object::Float(*value)),
            Literal::String(value) => Some(Object::String(value.clone())),
            Literal::Boolean(value) => Some(Object::Boolean(*value)),
            Literal::Unit => Some(Object::Unit),
            Literal::List(elements) => Some(self.eval_list(elements)),
            _ => Some(Object::Error("Unsupported literal type".to_string())),
        }
    }

    fn eval_list(&mut self, elements: &Vec<Expression>) -> Object {
        let mut evaluated = Vec::new();
        let mut first_type: Option<Object> = None;
        
        for element in elements {
            match self.eval_expression(element) {
                Some(value) => {
                    if let Some(ref first) = first_type {
                        if !self.assert_list_type(&vec![value.clone()], first) {
                            return Object::Error(format!(
                                "List elements must be of the same type. Expected {:?}, got {:?}",
                                first, value
                            ));
                        }
                    } else {
                        first_type = Some(value.clone());
                    }
                    evaluated.push(value);
                }
                None => return Object::Error("Failed to evaluate list element".to_string()),
            }
        }
        
        Object::List(evaluated)
    }

    fn same_type(&self, a: &Object, b: &Object) -> bool {
        std::mem::discriminant(a) == std::mem::discriminant(b)
    }


    fn assert_list_type(&self, elements: &Vec<Object>, expected_type: &Object) -> bool {
        elements.iter().all(|v| self.same_type(v, expected_type))
    }

    fn eval_cons_infix(&mut self, left_value: Object, right_value: Object) -> Object {
        if let Object::List(elements) = right_value {
            if self.assert_list_type(&elements, &left_value) {
                let mut new_list = elements.clone();
                new_list.insert(0, left_value);
                Object::List(new_list)
            } else {
                Object::Error(String::from(format!("Type mismatch, expected type {:?}",std::mem::discriminant(&left_value))))
            }
        } else {
            Object::Error(String::from(format!("Invalid cons operation for type {:?}",  std::mem::discriminant(&right_value))))
        }
    }

    fn eval_infix(&mut self, infix: &Infix, left: Object, right: Object) -> Object {
        match left {
            Object::List(left_value) => {
                if let Object::List(right_value) = right {
                    match infix {
                        Infix::Concat => {
                            let mut concat = left_value.clone();
                            concat.extend(right_value.clone());
                            Object::List(concat)
                        },
                        _ => Object::Error(String::from(format!("Invalid infix operator {:?} for given type: list", infix)))
                    }
                } else {
                    Object::Error(String::from(format!("Invalid infix operator {:?} for given type: list", infix)))
                }
            }
            Object::Integer(left_value) => {
                if let Object::List(right_value) = right {
                    match infix {
                        Infix::Cons => self.eval_cons_infix( Object::Integer(left_value), Object::List(right_value)),
                        _ => Object::Error(String::from(format!("Invalid infix operator {:?} for given type: int", infix)))
                    }
                } else if let Object::Integer(right_value) = right {
                    self.eval_integer_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(format!(
                        "Type Mismatch for infix: int infix {:?} -> int | {:?}",
                        infix, std::mem::discriminant(&right)
                    )))
                }
            }
            Object::Float(left_value) => {
                if let Object::List(right_value) = right {
                    match infix {
                        Infix::Cons => self.eval_cons_infix(Object::Float(left_value), Object::List(right_value)),
                        _ => Object::Error(String::from(format!("Invalid infix operator {:?} for given type: float", infix)))
                    }
                } else  if let Object::Float(right_value) = right {
                    self.eval_float_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(format!(
                        "Type Mismatch for infix: float infix {:?} -> float | {:?}",
                        infix, std::mem::discriminant(&right)
                    )))
                }
            }
            Object::Boolean(left_value) => {
                if let Object::List(right_value) = right {
                    match infix {
                        Infix::Cons => self.eval_cons_infix( Object::Boolean(left_value), Object::List(right_value)),
                        _ => Object::Error(String::from(format!("Invalid infix operator {:?} for given type: bool", infix)))
                    }
                } else if let Object::Boolean(right_value) = right {
                    self.eval_boolean_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(format!(
                        "Type Mismatch for infix: bool infix {:?} -> bool | {:?}",
                        infix, std::mem::discriminant(&right)
                    )))
                }
            }
            Object::String(left_value) => {
                if let Object::List(right_value) = right {
                    match infix {
                        Infix::Cons => self.eval_cons_infix(Object::String(left_value), Object::List(right_value)),
                        _ => Object::Error(String::from(format!("Invalid infix operator {:?} for given type: string", infix)))
                    }
                } else if let Object::String(right_value) = right {
                    self.eval_string_infix(infix, left_value, right_value)
                } else {
                    Object::Error(String::from(format!(
                        "Type Mismatch for infix: string infix {:?} -> string | {:?}",
                        infix, std::mem::discriminant(&right)
                    )))
                }
            }
            Object::Union { type_name: left_type, variant: left_variant, value: left_value } => {
                match infix {
                    Infix::Equal | Infix::DoesNotEqual => {
                        match right {
                            // Full comparison of union values
                            Object::Union { type_name: right_type, variant: right_variant, value: right_value } => {
                                let variant_eq = left_variant == right_variant && left_type == right_type;
                                
                                // If variant names match, check values if they exist
                                let values_eq = match (left_value, right_value) {
                                    (Some(left_val), Some(right_val)) => *left_val == *right_val,
                                    (None, None) => true,
                                    _ => false
                                };
                                
                                let result = variant_eq && values_eq;
                                if matches!(infix, Infix::DoesNotEqual) {
                                    Object::Boolean(!result)
                                } else {
                                    Object::Boolean(result)
                                }
                            },
                            // Simple variant constructor on right side (no value)
                            _ => Object::Error(format!(
                                "Type mismatch for union comparison: expected union type, got {:?}",
                                std::mem::discriminant(&right)
                            ))
                        }
                    },
                    _ => Object::Error(format!(
                        "Invalid operator {:?} for union type",
                        infix
                    ))
                }
            },
            _ => Object::Error(String::from(format!(
                "Type Mismatch for infix: {:?} infix {:?} -> {:?}",
                std::mem::discriminant(&left), infix, std::mem::discriminant(&right)
            ))),
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

    fn eval_integer_infix(&mut self, infix: &Infix, left: i128, right: i128) -> Object {
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

    fn eval_dot_access(&mut self, object: &Expression, property: &Token) -> Option<Object> {
        let obj = self.eval_expression(object)?;
        match (&obj, property) {
            (Object::Record { fields, .. }, Token::Identifier(field_name)) => {
                for (name, value) in fields {
                    if let Token::Identifier(n) = name {
                        if n == field_name {
                            return Some(value.clone());
                        }
                    }
                }
                Some(Object::Error(format!("Field '{}' not found in record", field_name)))
            },
            // Handle list indexing
            (Object::List(elements), Token::IntegerLiteral(index_str)) => {
                match index_str.parse::<i128>() {
                    Ok(index) => {
                        if index < 0 {
                            return Some(Object::Error("List index cannot be negative".to_string()));
                        }
                        if index >= elements.len() as i128 {
                            return Some(Object::Error("List index out of bounds".to_string()));
                        }
                        Some(elements[index as usize].clone())
                    },
                    Err(_) => Some(Object::Error("Invalid list index".to_string())),
                }
            },
            (Object::Tuple(elements), Token::IntegerLiteral(index_str)) => {
                match index_str.parse::<i128>() {
                    Ok(index) => {
                        if index < 0 {
                            return Some(Object::Error("Tuple index cannot be negative".to_string()));
                        }
                        if index >= elements.len() as i128 {
                            return Some(Object::Error("Tuple index out of bounds".to_string()));
                        }
                        Some(elements[index as usize].clone())
                    },
                    Err(_) => Some(Object::Error("Invalid tuple index".to_string())),
                }
            },
            _ => Some(Object::Error(format!("Type {:?} does not support field access or method calls", std::mem::discriminant(&obj)))),
        }
    }

    fn eval_namespaced_call(&mut self, namespace: &Identifier, function: &Identifier, arguments: &Vec<Expression>) -> Option<Object> {
        let args = arguments
            .iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Vec<_>>();

        let args: Vec<Object> = args.into_iter().filter_map(|arg| arg).collect();

        // Call the appropriate builtin function based on namespace and function name
        match (namespace, function) {
            (Token::Std, Token::Println) => Some(println_builtin(args)),
            (Token::List, Token::Map) => Some(map_builtin(args)),
            (Token::List, Token::Filter) => Some(filter_builtin(args)),
            (Token::List, Token::Fold) => Some(fold_builtin(args)),
            (Token::List, Token::Flatten) => Some(flatten_builtin(args)),
            (Token::List, Token::FlatMap) => Some(flatmap_builtin(args)),
            (Token::StringType, Token::FromInt) => Some(from_int_builtin(args)),
            (Token::StringType, Token::FromFloat) => Some(from_float_builtin(args)),
            (Token::StringType, Token::FromBool) => Some(from_bool_builtin(args)),
            (Token::StringType, Token::Length) => Some(length_builtin(args)),
            (Token::StringType, Token::Split) => Some(split_builtin(args)),
            (Token::StringType, Token::Trim) => Some(string_trim(args)),
            (Token::Option, Token::Unwrap) => Some(unwrap_builtin(args)),
            // (Token::Option, Token::IsSome) => Some(is_some_builtin(args)),
            // (Token::Option, Token::IsNone) => Some(is_none_builtin(args)),
            (Token::Union, Token::IsVariant) => Some(is_variant_builtin(args)),
            (Token::Union, Token::VariantName) => Some(variant_name_builtin(args)),
            (Token::Union, Token::HasValue) => Some(has_value_builtin(args)),
            (Token::Union, Token::ValueOf) => Some(value_of_builtin(args)),
            (Token::Union, Token::TypeOf) => Some(type_of_builtin(args)),
            _ => Some(Object::Error(format!(
                "Unknown namespaced function: {}::{}",
                namespace, function
            ))),
        }
    }

    fn eval_list_flatten(&mut self, args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error("flatten takes exactly one argument".to_string());
        }

        match &args[0] {
            Object::List(outer) => {
                let mut flattened = Vec::new();
                for item in outer {
                    match item {
                        Object::List(inner) => flattened.extend(inner.clone()),
                        _ => flattened.push(item.clone()),
                    }
                }
                Object::List(flattened)
            },
            _ => Object::Error("flatten argument must be a list".to_string()),
        }
    }

    // Add method call evaluation for when a builtin method is called
    fn eval_method_call(&mut self, method: Object, args: Vec<Object>) -> Object {
        match method {
            Object::BuiltinMethod { namespace, method, receiver } => {
                let mut all_args = vec![*receiver];
                all_args.extend(args);
                
                match namespace {
                    Namespace::List => match method.as_str() {
                        "map" => map_builtin(all_args),
                        "filter" => filter_builtin(all_args),
                        "fold" => fold_builtin(all_args),
                        "flatten" => self.eval_list_flatten(all_args),
                        _ => Object::Error(format!("Unknown list method '{}'", method)),
                    },
                    Namespace::String => match method.as_str() {
                        "length" => length_builtin(all_args),
                        "split" => split_builtin(all_args),
                        "trim" => string_trim(all_args),
                        _ => Object::Error(format!("Unknown string method '{}'", method)),
                    },
                    Namespace::Union => match method.as_str() {
                        "is_variant" => is_variant_builtin(all_args),
                        "variant_name" => variant_name_builtin(all_args),
                        "has_value" => has_value_builtin(all_args),
                        "value_of" => value_of_builtin(all_args),
                        "type_of" => type_of_builtin(all_args),
                        _ => Object::Error(format!("Unknown union method '{}'", method)),
                    },
                    _ => Object::Error("Unsupported namespace for method calls".to_string()),
                }
            },
            _ => Object::Error("Not a method".to_string()),
        }
    }



    fn eval_variant(&mut self, variant: &Identifier, value_expr: &Option<Box<Expression>>) -> Option<Object> {
        if let Token::Identifier(variant_name) = variant {
            // Find which union type this variant belongs to
            let type_info = self.type_env.borrow()
                .iter()
                .find(|(_, type_def)| {
                    if let Type::Union(variants) = type_def {
                        variants.iter().any(|(v, _)| v == variant)
                    } else {
                        false
                    }
                })
                .map(|(name, type_def)| (name.clone(), type_def.clone()));

            if let Some((type_name, type_def)) = type_info {
                // Get the expected type for this variant
                if let Type::Union(variants) = type_def {
                    let variant_type = variants.iter()
                        .find(|(v, _)| v == variant)
                        .map(|(_, t)| t.clone());
                    
                    // Process value based on variant type
                    match (variant_type, value_expr) {
                        // No type expected, no value provided - correct
                        (None, None) => {
                            return Some(Object::Union {
                                type_name,
                                variant: variant_name.clone(),
                                value: None,
                            });
                        },
                        // Type expected, value provided - check and wrap
                        (Some(Some(expected_alias)), Some(expr)) => {
                            let value = self.eval_expression(expr);
                            if let Some(evaluated_value) = value {
                                // Check if value matches expected type
                                if !self.check_type_match(&evaluated_value, &expected_alias) {
                                    return Some(Object::Error(format!(
                                        "Type mismatch for variant {}: expected {:?}, got {:?}",
                                        variant_name, expected_alias, evaluated_value
                                    )));
                                }
                                return Some(Object::Union {
                                    type_name,
                                    variant: variant_name.clone(),
                                    value: Some(Box::new(evaluated_value)),
                                });
                            } else {
                                return Some(Object::Error(format!(
                                    "Failed to evaluate value for variant {}",
                                    variant_name
                                )));
                            }
                        },
                        // Type expected but no value provided
                        (Some(Some(_)), None) => {
                            return Some(Object::Error(format!(
                                "Variant {} requires a value but none was provided",
                                variant_name
                            )));
                        },
                        // No type expected but value provided
                        (None, Some(_)) => {
                            return Some(Object::Error(format!(
                                "Variant {} does not accept a value",
                                variant_name
                            )));
                        },
                        _ => {
                            // Handle other cases
                            return Some(Object::Error(format!(
                                "Invalid variant type or value for variant {}",
                                variant_name
                            )));
                        }
                    }
                }
            }
            
            Some(Object::Error(format!("Unknown variant: {}", variant_name)))
        } else {
            Some(Object::Error("Invalid variant name".to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_eval_let() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        let program = vec![Statement::Let(Identifier::Identifier("x".to_string()), Expression::Literal(Literal::Integer(1))), Statement::Expression(Expression::Identifier(Identifier::Identifier("x".to_string())))];
        let result = evaluator.eval(&program);
        assert_eq!(result, Some(Object::Integer(1)));
    }

    #[test]     
    fn test_eval_return() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        let program = vec![Statement::Return(Expression::Literal(Literal::Integer(1)))];
        let result = evaluator.eval(&program);
        assert_eq!(result, Some(Object::Return(Box::new(Object::Integer(1)))));
    }

    #[test] 
    fn test_eval_if_else_else() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        let program = vec![Statement::Expression(Expression::If { condition: Box::new(Expression::Literal(Literal::Boolean(true))), consequence: vec![Statement::Return(Expression::Literal(Literal::Integer(1)))], alternative: Some(vec![Statement::Return(Expression::Literal(Literal::Integer(2)))]) })];
        let result = evaluator.eval(&program);
        assert_eq!(result, Some(Object::Return(Box::new(Object::Integer(1)))));
    }

    #[test]
    fn test_eval_list() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        let program = vec![Statement::Let(Identifier::Identifier("x".to_string()), Expression::Literal(Literal::List(vec![Expression::Literal(Literal::Integer(1)), Expression::Literal(Literal::Integer(2))]))), Statement::Expression(Expression::Identifier(Identifier::Identifier("x".to_string())))];
        let result = evaluator.eval(&program);
        assert_eq!(result, Some(Object::List(vec![Object::Integer(1), Object::Integer(2)])));
    }

    #[test]
    fn test_eval_list_cons() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        let program = vec![Statement::Let(Identifier::Identifier("x".to_string()), Expression::Literal(Literal::List(vec![Expression::Literal(Literal::Integer(1)), Expression::Literal(Literal::Integer(2))]))), Statement::Expression(Expression::Infix(Infix::Cons, Box::new(Expression::Literal(Literal::Integer(3))), Box::new(Expression::Identifier(Identifier::Identifier("x".to_string())))))];
        let result = evaluator.eval(&program);
        assert_eq!(result, Some(Object::List(vec![Object::Integer(3), Object::Integer(1), Object::Integer(2)])));
    }

    #[test]
    fn test_eval_range() {
        let env = Rc::new(RefCell::new(Env::new()));
        let mut evaluator = Evaluator::new(env);
        let start = Expression::Literal(Literal::Integer(1));
        let end = Expression::Literal(Literal::Integer(5));
        let range_expr = Expression::Range {
            start: Box::new(start),
            end: Box::new(end),
        };
        
        let result = evaluator.eval_expression(&range_expr).unwrap();
        match result {
            Object::List(elements) => {
                assert_eq!(elements.len(), 5);
                for (i, obj) in elements.iter().enumerate() {
                    assert_eq!(*obj, Object::Integer((i + 1) as i128));
                }
            }
            _ => panic!("Expected list, got {:?}", result),
        }
        
        let start = Expression::Literal(Literal::Float(1.5));
        let end = Expression::Literal(Literal::Integer(5));
        let range_expr = Expression::Range {
            start: Box::new(start),
            end: Box::new(end),
        };
        
        let result = evaluator.eval_expression(&range_expr).unwrap();
        match result {
            Object::Error(msg) => {
                assert!(msg.contains("Range start must be an integer"));
            }
            _ => panic!("Expected error for non-integer start, got {:?}", result),
        }
        
        let start = Expression::Literal(Literal::Integer(1));
        let end = Expression::Literal(Literal::String("5".to_string()));
        let range_expr = Expression::Range {
            start: Box::new(start),
            end: Box::new(end),
        };
        
        let result = evaluator.eval_expression(&range_expr).unwrap();
        match result {
            Object::Error(msg) => {
                assert!(msg.contains("Range end must be an integer"));
            }
            _ => panic!("Expected error for non-integer end, got {:?}", result),
        }
    }

    #[test]
    fn test_complex_type_aliases() {
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
        
        // Define type aliases
        evaluator.eval_type(
            &Token::Identifier("alive".to_string()),
            &Type::Alias(Alias {
                name: TypeConstructor::BuiltIn(Constructor::Bool),
                parameters: vec![],
            }),
        );

        evaluator.eval_type(
            &Token::Identifier("tags".to_string()),
            &Type::Alias(Alias {
                name: TypeConstructor::BuiltIn(Constructor::List),
                parameters: vec![Alias {
                    name: TypeConstructor::BuiltIn(Constructor::Result),
                    parameters: vec![
                        Alias {
                            name: TypeConstructor::BuiltIn(Constructor::Int),
                            parameters: vec![],
                        },
                        Alias {
                            name: TypeConstructor::BuiltIn(Constructor::String),
                            parameters: vec![],
                        },
                    ],
                }],
            }),
        );

        // Test simple alias
        assert!(evaluator.check_type_match(
            &Object::Boolean(true),
            &Alias {
                name: TypeConstructor::Custom(Token::Identifier("alive".to_string())),
                parameters: vec![],
            }
        ));

        // Test complex nested alias
        let complex_value = Object::List(vec![
            Object::ResultOk(Box::new(Object::Integer(42))),
            Object::ResultErr(Box::new(Object::String("error".to_string()))),
        ]);

        assert!(evaluator.check_type_match(
            &complex_value,
            &Alias {
                name: TypeConstructor::Custom(Token::Identifier("tags".to_string())),
                parameters: vec![],
            }
        ));
    }
}