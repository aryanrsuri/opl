pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod object;
pub mod evaluator;
pub mod environment;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Check for --eval flag
    let evaluate = args.contains(&"--eval".to_string());

    fs::create_dir_all("tests").expect("Failed to create tests directory");

    match args.get(1) {
        Some(arg) if arg != "--eval" => {
            let input = match fs::read_to_string(arg) {
                Ok(content) => content,
                Err(e) => {
                    eprintln!("Error reading file '{}': {}", arg, e);
                    return;
                }
            };

            let log_path = format!("{}.log", arg);
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
        _ => {
            println!("[USAGE] \n\t cargo run <filename> \n\t cargo run -- --eval \n\t cargo run");
            println!("No file provided, entering REPL mode (evaluate: {})...", evaluate);
            repl::start(evaluate);
        }
    }
}
