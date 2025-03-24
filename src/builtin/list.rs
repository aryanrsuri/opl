use crate::environment::Env;
use crate::evaluator::Evaluator;
use crate::lexer::Token;
use crate::object::Object;
use std::cell::RefCell;
use std::rc::Rc;

pub fn filter_builtin(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error("filter expects exactly two arguments: function and list".to_string());
    }

    let function = &args[0];
    let list = &args[1];

    match (function, list) {
        (Object::Function(params, body, env, type_env), Object::List(elements)) => {
            if params.len() != 1 {
                return Object::Error("filter function must take exactly one argument".to_string());
            }

            let mut filtered = Vec::new();
            
            for element in elements {
                let mut inner_env = Env::new_with_outer(Rc::clone(env));
                if let Token::Identifier(ref name) = params[0] {
                    inner_env.set(name.clone(), element.clone());
                }

                let mut evaluator = Evaluator::new(Rc::new(RefCell::new(inner_env)));
                evaluator.set_type_env(Rc::clone(type_env));
                let result = match evaluator.eval_block(body) {
                    Some(Object::Return(value)) => *value,
                    Some(value) => value,
                    None => return Object::Error("Function returned no value".to_string()),
                };
                
                match result {
                    Object::Boolean(true) => filtered.push(element.clone()),
                    Object::Boolean(false) => {},
                    _ => return Object::Error(format!("Filter function must return a boolean, got {:?}", result)),
                }
            }

            Object::List(filtered)
        }
        (_, Object::List(_)) => Object::Error("First argument must be a function".to_string()),
        (Object::Function(_, _, _, _), _) => Object::Error("Second argument must be a list".to_string()),
        _ => Object::Error("Invalid arguments for filter".to_string()),
    }
}

pub fn map_builtin(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error("map expects exactly two arguments: function and list".to_string());
    }

    let function = &args[0];
    let list = &args[1];

    match (function, list) {
        (Object::Function(params, body, env, type_env), Object::List(elements)) => {
            if params.len() != 1 {
                return Object::Error("map function must take exactly one argument".to_string());
            }

            let mut mapped = Vec::new();
            
            for element in elements {
                let mut inner_env = Env::new_with_outer(Rc::clone(env));
                if let Token::Identifier(ref name) = params[0] {
                    inner_env.set(name.clone(), element.clone());
                }

                let mut evaluator = Evaluator::new(Rc::new(RefCell::new(inner_env)));
                evaluator.set_type_env(Rc::clone(type_env));
                match evaluator.eval_block(body) {
                    Some(Object::Return(value)) => mapped.push(*value),
                    Some(value) => mapped.push(value),
                    None => return Object::Error("Function returned no value".to_string()),
                }
            }

            if !mapped.is_empty() {
                let first = &mapped[0];
                for value in &mapped {
                    if std::mem::discriminant(value) != std::mem::discriminant(first) {
                        return Object::Error("Map function must return same type for all elements".to_string());
                    }
                }
            }

            Object::List(mapped)
        }
        (_, Object::List(_)) => Object::Error("First argument must be a function".to_string()),
        (Object::Function(_, _, _, _), _) => Object::Error("Second argument must be a list".to_string()),
        _ => Object::Error("Invalid arguments for map".to_string()),
    }
}

pub fn fold_builtin(args: Vec<Object>) -> Object {
    if args.len() != 3 {
        return Object::Error("fold expects exactly three arguments: function, initial value, and list".to_string());
    }

    let function = &args[0];
    let initial = &args[1];
    let list = &args[2];

    match (function, initial, list) {
        (Object::Function(params, body, env, type_env), initial, Object::List(elements)) => {
            if params.len() != 2 {
                return Object::Error("fold function must take exactly two arguments: accumulator and element".to_string());
            }

            let mut accumulator = initial.clone();

            for element in elements {
                let mut inner_env = Env::new_with_outer(Rc::clone(env));
                
                if let Token::Identifier(ref name) = params[0] {
                    inner_env.set(name.clone(), accumulator.clone());
                } else {
                    return Object::Error("First parameter must be an identifier".to_string());
                }
                
                if let Token::Identifier(ref name) = params[1] {
                    inner_env.set(name.clone(), element.clone());
                } else {
                    return Object::Error("Second parameter must be an identifier".to_string());
                }

                let mut evaluator = Evaluator::new(Rc::new(RefCell::new(inner_env)));
                evaluator.set_type_env(Rc::clone(type_env));
                match evaluator.eval_block(body) {
                    Some(Object::Return(value)) => accumulator = *value,
                    Some(value) => accumulator = value,
                    None => return Object::Error("Function returned no value".to_string()),
                }
            }

            accumulator
        }
        (_, _, not_list) if !matches!(not_list, Object::List(_)) => {
            Object::Error(format!("Third argument to fold must be a list, got {:?}", not_list))
        }
        (not_fn, _, _) if !matches!(not_fn, Object::Function(_, _, _, _)) => {
            Object::Error(format!("First argument to fold must be a function, got {:?}", not_fn))
        }
        (_, _, _) => Object::Error("Invalid arguments for fold".to_string()),
    }
}

pub fn flatten_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("flatten expects exactly one argument: list of lists".to_string());
    }

    let list = &args[0];

    match list {
        Object::List(elements) => {
            let mut flattened = Vec::new();
            for element in elements {
                match element {
                    Object::List(inner_list) => {
                        flattened.extend(inner_list.iter().cloned());
                    }
                    _ => flattened.push(element.clone()),
                }
            }
            Object::List(flattened)
        }
        _ => Object::Error("flatten expects a list of lists".to_string()),
    }
}

pub fn flatmap_builtin(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error("flatmap expects exactly two arguments: function and list".to_string());
    }

    let function = &args[0];
    let list = &args[1];

    match (function, list) {
        (Object::Function(params, body, env, type_env), Object::List(elements)) => {
            if params.len() != 1 {
                return Object::Error("flatmap function must take exactly one argument".to_string());
            }

            let mut flattened = Vec::new();

            for element in elements {
                let mut inner_env = Env::new_with_outer(Rc::clone(env));
                if let Token::Identifier(ref name) = params[0] {
                    inner_env.set(name.clone(), element.clone());
                }

                let mut evaluator = Evaluator::new(Rc::new(RefCell::new(inner_env)));
                evaluator.set_type_env(Rc::clone(type_env));
                match evaluator.eval_block(body) {
                    Some(Object::Return(value)) => match *value {
                        Object::List(inner_list) => {
                            flattened.extend(inner_list.iter().cloned());
                        }
                        other => flattened.push(other),
                    },
                    Some(value) => match value {
                        Object::List(inner_list) => {
                            flattened.extend(inner_list.iter().cloned());
                        }
                        other => flattened.push(other),
                    },
                    None => return Object::Error("Function returned no value".to_string()),
                }
            }
            Object::List(flattened)
        }
        (_, Object::List(_)) => Object::Error("First argument must be a function".to_string()),
        (Object::Function(_, _, _, _), _) => Object::Error("Second argument must be a list".to_string()),
        _ => Object::Error("Invalid arguments for flatmap".to_string()),
    }
}
