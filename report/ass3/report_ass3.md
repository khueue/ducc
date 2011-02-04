# Assignment 3 - Semantic Analysis

Compiler Project, VT11

2011-02-04

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

Project Repository at GitHub:
<https://github.com/khueue/ducc/tree/ass3-analyzer>

The source and executables are also available on the IT department server:
/home/emhe9781/src/ducc-2011-02-04.tar.gz

(Note that the source won't compile and the executables won't run on the IT
department servers since they have an old release of OTP which doesn't
include Leex nor escript).


## Introduction

In this report, we will briefly discuss our attempt to implement a (hopefully)
working analyzer (semantics checker) for the uC language (a subset of C).


## Tools Used

Erlang provides a module `dict`, which implements a key-value dictionary. We 
use `dict` to implement the symbol table in our environment. 

`dict` documentation: <http://www.erlang.org/doc/man/dict.html>


## Environment

The environment is implemented in `src/analyzer/analyzer_env.erl`.

The environment is represented as a list of symbol tables wrapped in a tuple:

    {SymTabs}

The head of `SymTabs` is the current scope.

Each symbol table in `SymTabs` have the form:

    {ScopeName, SymTab}

`ScopeName` is either the name of a function represented as a string or by 
the atom `global`. `SymTab` is a dictionary as returned by `dict:new()`.

E.g. the environment may look like:

    {[{"main", SymTab1}, {global, SymTab0}]}

XXX more implementation notes?

When storing an identifier in the symbol table, we supply the entire AST 
node as the associated value.

### Delimited Scopes

Updates to the environment are local to the function. For example, analyzing 
a function definition will create a new scope when entering the function. 
When leaving the function, we restore the environment by returning the 
previous environment.

See e.g. `analyze_fundef/2` in `src/analyzer/analyzer.erl`.

## Representation of Types

As previously mentioned, we store AST nodes as the value associated with the 
key in the symbol table.

XXX


## Typing Rules for Expressions

Typing rules for expressions are enforced by the `eval_type/2` (and 
`widest_type/3`) function.

XXX


## Running the Analyzer

### Multi-Step Compilation

The analyzer (and all the successive steps) has been implemented to read from
standard input and output to standard output.
As such it's possible to, for example, pipe the result from the lexer to the
parser, and from the parser to the analyzer:

    cat suite/quiet/semantic/s01.c | lexer | parser | analyzer

If the source file is semantically correct, the analyzer will output 
output the abstract syntax tree to standard output.
If the source file contains at least one semantic error, the analyzer will
output the first error to standard output.

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
