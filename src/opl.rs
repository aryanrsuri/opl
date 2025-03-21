use clap::{Parser, Subcommand};
use std::cell::RefCell;
use std::fs;
use std::rc::Rc;
use crate::{lexer, parser, evaluator, environment, repl};

const VERSION: &str = "0.3.3a2a3d6-rc";
const ABOUT: &str = "Opl is a general purpose functional programming language.";
const ZEN: &str = "\n* Strive to be pure.\n* Simplicity over complexity.\n* Elegance over verbosity.\n";

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
    #[command(about = "Start an interactive REPL session. Optional --parse flat to parse the input without evaluating it.")]
    Repl {
        #[arg(short, long)]
        parse: bool,
    },
    #[command(about = "Execute a .opl file. Optional --eval flag to evaluate the input.")]
    Run {
        #[arg(name = "FILE")]
        file: String,
    },
    #[command(about = "Print our zen and exit.")]
    Zen,
}

pub fn run() {
    let cli = Cli::parse();

    match cli.command {
        None => {
            let _ = Cli::parse_from(&["opl", "--help"]);
        },
        Some(command) => match command {
            Commands::Repl { parse } => {
                println!("Starting OPL REPL (parse only: {})", parse);
                repl::start(parse);
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
                    println!("{}", result);
                }
            },
            Commands::Zen => {
                println!("{}", ZEN);
            },
        },
    }
} 