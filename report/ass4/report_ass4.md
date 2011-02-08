# Assignment 4 - Intermediate Code

Compiler Project, VT11

2011-XX-XX

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT department server at:
/home/emhe9781/src/XXX

(Note that the source won't compile and the executables won't run on the IT
department servers. The servers have an old release of Erlang installed as
default which doesn't include Leex nor escript).

## Introduction

XXX

## Tools Used

Erlang provides a module `dict`, which implements a key-value dictionary. We
use `dict` to implement the symbol table in our environment.

Documentation for `dict`: <http://www.erlang.org/doc/man/dict.html>

## Translator Environment

XXX will probably be similar to the analyzer env?

The environment is implemented in `XXXXXXXXXXXX`.

The environment is represented as a stack of scopes wrapped in a tuple:

    {Scopes}

The head of the `Scopes` stack (which is just a list) is the current scope.
Each scope has the form:

    {ScopeName, SymTab}

`ScopeName` is either the name of a function represented as a string,
or the atom `global` if the translator happens to be looking at the top-level.
`SymTab` is a dictionary as returned by `dict:new()`.

For example, if the translator is currently investigating the function `main`,
the environment will look like:

    {[{"main", SymTab1}, {global, SymTab0}]}

The scope stack will never grow beyond two elements because the only
scope-introducing construct in uC is the function. Nevertheless, a stack
is a natural and convenient representation of scopes.

### Symbol Tables

XXX different kind of nodes compared to the analyzer?

Each symbol table is represented by an Erlang dictionary. When we encounter
a new declaration, its identifier is used as the key, and its entire AST node
is used as the associated value. This provides us with all the information we
need (and more), and we do not need to devise new data types. On the
downside, this probably involves more copying than necessary.

## RTL

XXX RTL design, datatypes, ...

## Translation Process

### Control Flow Statements

XXX

### Variable References

#### Global and Local Variables

XXX

#### Array and Scalar Variables

XXX

## Running the Translator

### Multi-Step Compilation

The translator (as all previous steps) has been implemented to read from
standard input and output to standard output.
As such it's possible to, for example, pipe the result from the lexer to the
parser, from the parser to the analyzer, and from the analyzer to the
translator:

    cat suite/quiet/rtl/r01.c | lexer | parser | analyzer | translator

If the translator encounters an error, it will print a error message to
standard output and stop evaluation. Otherwise, if successful, the translator
will output the RTL code to standard output.

### Single-Step Compilation

For a more conventional approach to compilation, a script called `ducc` can
be used. `ducc` takes a single file as argument and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

### Error Handling

When a script (such as `translator`) receives invalid input, an error message
is printed and execution stops.

An error exhibited by the translator looks like:

    XXX
