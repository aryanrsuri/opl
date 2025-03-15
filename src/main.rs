pub mod ast;
pub mod ast_ai_generated;
pub mod lexer;
pub mod parser;
pub mod repl;
use std::env;
use std::fs;
fn main() {
    let args: Vec<String> = env::args().collect();
    
    // Create tests directory if it doesn't exist
    fs::create_dir_all("tests").expect("Failed to create tests directory");
    
    match args.get(1) {
        Some(path) => {
            let full_path = format!("tests/{}", path);
            let input = match fs::read_to_string(&full_path) {
                Ok(content) => content,
                Err(e) => {
                    eprintln!("Error reading file '{}': {}", full_path, e);
                    return;
                }
            };

            // Create log file
            let log_path = format!("tests/{}.log", path);
            let log_file = fs::File::create(log_path).expect("Failed to create log file");

            let lexer = lexer::Lexer::new(&input);
            let mut parser = parser::Parser::new(lexer);
            parser.set_log_file(log_file);

            let program = parser.parse_program();
            
            // Write final AST and errors to log file
            let mut log_messages = Vec::new();
            log_messages.push(format!("Final AST: {:#?}", program));
            
            if !parser.errors.is_empty() {
                log_messages.push("Parser errors:".to_string());
                for error in &parser.errors {
                    log_messages.push(format!("  {:#?}", error));
                }
            }

            for msg in log_messages {
                parser.log(&msg);
            }
        },
        None => {
            println!("Usage: cargo run <filename>");
            println!("No file provided, entering REPL mode...");
            repl::start();
        }
    }
}
