pub mod lexer;

use lexer::Token;
use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();

    // Check for command line arguments.
    if args.len() > 1 {
        match args[1].as_str() {
            "--file" => {
                if args.len() > 2 {
                    process_file(&args[2]);
                } else {
                    eprintln!("Usage: cargo run -- --file <file_path>");
                }
            }
            "--repl" => {
                start();
            }
            _ => {
                eprintln!("Usage: cargo run -- --file <file_path> OR cargo run -- --repl");
            }
        }
    } else {
        // Default to REPL if no arguments are provided.
        start();
    }
}

pub fn start() {
    loop {
        print!("$ ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Read failed.");
        let read = input.trim();

        if read.eq_ignore_ascii_case("exit") {
            break;
        }

        let mut lexer = lexer::Lexer::new(read);
        loop {
            let token = lexer.advance();
            match token {
                Token::End => break,
                _ => println!("{:#?}", token),
            }
        }
    }
}

fn process_file(file_path: &str) {
    let content = fs::read_to_string(file_path).expect("Unable to read file");
    let mut lexer = lexer::Lexer::new(&content);
    loop {
        let token = lexer.advance();
        match token {
            Token::End => break,
            _ => println!("{:#?}", token),
        }
    }
}

