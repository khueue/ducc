# Assignment 3 - Semantic Analysis

Compiler Project, VT11

2011-02-04

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

Project Repository at GitHub:
<https://github.com/khueue/ducc/tree/ass3-analyzer>

The source and executables are also available on the IT department server:
/home/emhe9781/src/ducc-XXX.tar.gz

(Note that the source won't compile and the executables won't run on the IT
department servers since they have an old release of OTP which doesn't
include Leex nor escript).

## Introduction

In this report, we will briefly discuss how we implemented an analyzer
(semantics checker) for the uC language (a subset of C) in Erlang.

## Environment

XXX

### Delimited Scopes

XXX

## Rules

XXX

## Running the Analyzer

### Multi-Step Compilation

The analyzer (and all the successive steps) has been implemented to read from
standard input and output to standard output.
As such it's possible to, for example, pipe the result from the lexer to the
parser, and from the parser to the analyzer:

    cat suite/quiet/semantic/s01.c | lexer | parser | analyzer

If the source file doesn't contain any semantic errors, the analyzer will
(hopefully) output the abstract syntax tree to standard output.
If the source file contains at least one semantic error, the analyzer will
(hopefully) output the first error to standard output.

### Single-Step Compilation

For a more conventional approach to compilation, a script called `ducc` can
be used. `ducc` takes a single file as argument and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

### Error Handling

When a script (such as `lexer`, `parser`, or `analyzer`) receives invalid
input, an error message is printed and execution stops.

A lexical error exhibited by the lexer looks like this:

    Lexical error on line 13, illegal: "%"

A syntax error exhibited by the parser looks like this:

    Syntax error on line 5, before: "')'"

A semantic error exhibited by the analyzer looks like this:

    XXX

If an unreadable file is given to `ducc`, the error looks like this:

    Error reading file: some_unreadable_file

## Questions and Issues

XXX
