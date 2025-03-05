// repl.rs

use crate::lexer::Lexer;
use crate::parser_ai_generated::Parser;
use std::io::{self, Write};

pub fn start() {
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if let Err(err) = io::stdin().read_line(&mut input) {
            println!("Error reading input: {}", err);
            continue;
        }

        // Remove any leading and trailing whitespace.
        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue; // skip empty input
        }
        if trimmed.eq_ignore_ascii_case("exit") {
            break;
        }

        // Create a lexer and parser from the trimmed input.
        let lexer = Lexer::new(trimmed);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        // If there are parse errors, print them.
        if !parser.errors.is_empty() {
            println!("Parser errors:");
            for error in parser.errors {
                println!("  - {}", error);
            }
        } else {
            // Otherwise, print the AST.
            println!("{:#?}", program);
        }
    }
}
