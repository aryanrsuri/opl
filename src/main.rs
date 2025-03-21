pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod object;
pub mod evaluator;
pub mod environment;
pub mod opl;
pub mod builtin;



fn main() {
    opl::run();
}
