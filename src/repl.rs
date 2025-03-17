// repl.rs

use crate::{environment::Env, evaluator::Evaluator};
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::{cell::RefCell, io::{self, Write}, rc::Rc};

pub fn start(evaluate: bool) {

    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::new())));
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if let Err(err) = io::stdin().read_line(&mut input) {
            println!("Error reading input: {}", err);
            continue;
        }

        let input = input.trim_matches(|c: char| c.is_whitespace() && c != '\n');
        if input.is_empty() {
            continue;
        }
        if input.trim() == "exit" {
            break;
        }
        if input.trim() == "clear" {
            print!("\x1b[2J\x1b[H");
            continue;
        }

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            println!("Parser errors:");
            for error in parser.errors {
                println!("Errors {:#?}", error);
            }
        } else {
            if evaluate {
                match evaluator.eval(&program) {
                    Some(object) => println!("# {}", object),
                    None => (),
                };
            } else {
                println!("{:?}", program);
            }
        }
    }
}
