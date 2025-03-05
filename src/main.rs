pub mod ast;
pub mod ast_ai_generated;
pub mod lexer;
pub mod parser;
pub mod parser_ai_generated;
pub mod repl;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        match args[1].as_str() {
            "--file" => {
                if args.len() > 2 {
                    process_file(&args[2]);
                } else {
                    eprintln!("Usage: cargo run -- --file <file_path>");
                }
            }
            "--repl" => repl::start(),
            _ => {
                eprintln!("Usage: cargo run -- --file <file_path> OR cargo run -- --repl");
            }
        }
    } else {
        // Default to REPL if no arguments are provided.
        repl::start();
    }
}

fn process_file(file_path: &str) {
    let content = fs::read_to_string(file_path).expect("Unable to read file");
    let lexer = lexer::Lexer::new(&content);
    let mut parser = parser_ai_generated::Parser::new(lexer);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        for error in parser.errors {
            println!("Parser error: {}", error);
        }
    } else {
        println!("{:#?}", program);
    }
}
