pub mod ast;
pub mod builtin;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod opl;
pub mod parser;
pub mod repl;

fn main() {
    opl::run();
}
