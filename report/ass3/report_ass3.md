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
use `dict` to implement the symbol tables in our environment.

`dict` documentation: <http://www.erlang.org/doc/man/dict.html>

## Analyzer Environment

The environment is implemented in `src/analyzer/analyzer_env.erl`.

The environment is represented as a stack of symbol tables wrapped in a tuple:

    {Scope}

The head of the `Scope` stack (which is just a list) is the current scope.
Each scope has the form:

    {ScopeName, SymTab}

`ScopeName` is either the name of a function represented as a string,
or the atom `global` if the analyzer happens to be looking at the top-level.
`SymTab` is a dictionary as returned by `dict:new()`.

For example, if the analyzer is currently investigating the function `main`,
the environment will look like this:

    {[{"main", SymTab1}, {global, SymTab0}]}

The scope stack will never grow beyond two elements because the only
scope-introducing construct in uC is the function. Nevertheless, a stack
is a natural and convenient representation of scopes.

### Symbol Tables

Each symbol table is represented by an Erlang dictionary. When we encounter
a new declaration, its identifier is used as the key, and its entire AST node
is used as the associated value. This provides us with all the information we
need (and more), and we do not need to devise new data types.

### Delimited Scopes

When the analyzer encounters a function definition (which is the only
scope-introducing construct in uC), a new scope is created
and pushed onto the environment stack. When the analysis of the function
completes, that scope is discarded. This is achieved by simply by passing
on the environment that was used before the scope was created (leaving the
actual destruction to the garbage collector).

For more information, see for example `analyze_fundef/2`
in `src/analyzer/analyzer.erl`.

## Typing Rules for Expressions

Typing rules for expressions are enforced by the `eval_type/2` (and
`widest_type/3`) function.

The function `eval_type(Node, Env)` tries to evaluate `Node` to a type tuple
represented as `{Tag, Type}`, using `Env` for lookups. It recurses down on
operations such as `binop` or `unop`, and directly evaluates other nodes
such as `ident` or `arrelem`. For binary operations, the resulting type is
the "widest" type of its operands, as defined by `widest_type/2`. This
function issues an error if the two types are incompatible:

    widest_type({_,_}, {arraydec,_})        -> throw(incompatible);
    widest_type({arraydec,_}, {_,_})        -> throw(incompatible);
    widest_type({_,_}, {formal_arraydec,_}) -> throw(incompatible);
    widest_type({formal_arraydec,_}, {_,_}) -> throw(incompatible);
    widest_type({_,int}, {_,int})           -> int;
    widest_type({_,int}, {_,char})          -> int;
    widest_type({_,char}, {_,char})         -> char;
    widest_type({_,char}, {_,int})          -> int;
    widest_type({_,_}, {_,_})               -> throw(incompatible).

For issues assignments xxx

    first_accepts_second({formal_arraydec, Type}, {arraydec, Type}) -> ok;
    first_accepts_second({formal_arraydec, Type}, {formal_arraydec, Type}) -> ok;
    first_accepts_second(_, {arraydec,_})    -> throw(incompatible);
    first_accepts_second({arraydec,_}, _)    -> throw(incompatible);
    first_accepts_second(_, {formal_arraydec,_})    -> throw(incompatible);
    first_accepts_second({_,void}, {_,void}) -> ok;
    first_accepts_second({_,int}, {_,int})   -> ok;
    first_accepts_second({_,int}, {_,char})  -> ok;
    first_accepts_second({_,char}, {_,char}) -> ok;
    first_accepts_second({_,char}, {_,int})  -> ok;
    first_accepts_second(_, _)               -> throw(incompatible).

XXXXXXXX dfs

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
