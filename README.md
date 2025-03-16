## Implementation of a pure functional language interpreter in rust

Opl (pronounced "opal") is a purely functional languague focused on analytical types and pure functions. This focus is both the study of Function PL and building interpreters. Rust was chosen for is excellent compile times and ergonomics

### Codebase

#### docs

- candidates : grammar addition candidates
- grammar: syntax of language (simple form of specification)
- specification: lexicon
#### src

- lexer: tokens, parsing a stream of opl code into tokens
- ast: self explanetory
- parser: producing a interpreted program based on the source code and ast
- repl/main: execute parser
#### tests

- opl_by_example: mirroring the now famous go_by_example webpage
- (some_grammar).opl: Opl file containing examples for one specific file

### Usage

Currently the interpreter can take in opl source code and return the ast-program. The two ways of consuming source coud is through a repl or a *.opl file.

Run the repl 
```bash
cargo run
```

Parse a file (the file must live in a tests/ folder)
```bash
cargo run -- file_name.opl
```
