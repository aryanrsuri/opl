use clap::{Parser, Subcommand};
use std::cell::RefCell;
use std::fs;
use std::rc::Rc;
use crate::{lexer, parser, evaluator, environment, repl};

const VERSION: &str = "0.1.0";
const ABOUT: &str = "OPL - A pure and orthogonal programming language";
const ZEN: &str = "* Strive to be pure and orthogonal";

#[derive(Parser)]
#[command(
    name = "opl",
    version = VERSION,
    about = ABOUT,
    long_about = None,
)]
pub struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Repl,
    Run {
        #[arg(name = "FILE")]
        file: String,
    },
    Zen,
}

pub fn run() {
    let cli = Cli::parse();

    match cli.command {
        None => {
            let _ = Cli::parse_from(&["opl", "--help"]);
        },
        Some(command) => match command {
            Commands::Repl => {
                println!("Starting OPL REPL...");
                repl::start(true);
            },
            Commands::Run { file } => {
                let input = match fs::read_to_string(&file) {
                    Ok(content) => content,
                    Err(e) => {
                        eprintln!("Error reading file '{}': {}", file, e);
                        return;
                    }
                };

                let lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(lexer);
                let mut evaluator = evaluator::Evaluator::new(Rc::new(RefCell::new(environment::Env::new())));
                let program = parser.parse_program();
                
                if !parser.errors.is_empty() {
                    eprintln!("Parser errors:");
                    for error in &parser.errors {
                        eprintln!("  {:#?}", error);
                    }
                    return;
                }

                if let Some(result) = evaluator.eval(&program) {
                    println!("Result: {:?}", result);
                }
            },
            Commands::Zen => {
                println!("{}", ZEN);
            },
        },
    }
} 