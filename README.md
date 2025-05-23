## Implementation of a pure functional language interpreter in rust

Opl (pronounced "opal") is a purely functional languague focused on analytical types and pure functions. This focus is both the study of Function PL and building interpreters. Rust was chosen excellent compile times and ergonomics

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

### The Opl Toolchain

```bash
Opl is a general purpose functional programming language.

Usage: opl [COMMAND]

Commands:
  repl  
  run   
  zen   
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```
