# WP

Parser, interpreter, and tools for working with the [Guarded Command Language](https://en.wikipedia.org/wiki/Guarded_Command_Language) (GCL) described in *A Discipline of Programming* by Edsger Dijkstra and the
corresponding [system of predicate transformers](https://en.wikipedia.org/wiki/Predicate_transformer_semantics).

## Build requirements

- [SML of New Jersey](https://www.smlnj.org) (SML/NJ)
- ML-Lex (distributed alongside with SML/NJ)
- ML-Yacc (distributed alongside with SML/NJ)

## Usage

### Parser

`driver.sml` is the main entry point for running the parser. From the shell, run:

    $ sml gcl/gcl.cm gcl_parser/gcl_parser.cm driver.sml

Once the SML/NJ REPL launches, GCL files can be parsed by calling the `Driver.parse` function. For example:

    Driver.parse "examples/skip.gcl";

This will parse and typecheck the given GCL source file.

### Weakest precondition tool

`wp.sml` is the main entry point for running the tool. From the shell, run:

    $ sml gcl/gcl.cm wp.sml

Once the SML/NJ REPL launches, the functions in `wp.sml` can be called.