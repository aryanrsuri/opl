// repl.rs

use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub fn start(evaluate: bool) {
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if let Err(err) = io::stdin().read_line(&mut input) {
            println!("Error reading input: {}", err);
            continue;
        }

        // Only trim trailing whitespace except newlines
        let input = input.trim_matches(|c: char| c.is_whitespace() && c != '\n');
        if input.is_empty() {
            continue;
        }
        if input.eq_ignore_ascii_case("exit") {
            break;
        }

        // Create a lexer and parser from the input
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut evaluator = Evaluator::new();
        // If there are parse errors, print them.
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
