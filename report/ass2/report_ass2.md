# Assignment 2 - Parser

Compiler Project, VT11

2011-01-XX

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

Project Repository at GitHub:
<https://github.com/khueue/ducc/tree/ass2-parser>

The source and executables are also available on the IT department server:
/home/emhe9781/src/ducc.tar.gz

(Note that the source won't compile and the executables won't run on the IT
department servers since they have an old release of OTP which doesn't
include Leex and escript).

## Introduction

This assignment involves creating a parser (syntax analyser) for the uC
language (a subset of C), so this is what the report will focus on:
understanding the syntax part of uC, learning Yecc and arriving at a
(hopefully) working parser.

## Tools Used

### Yecc

Thankfully, the Erlang environment provides ports of both Lex and Yacc, namely
Leex and Yecc. Yecc documentation can be found here:
<http://www.erlang.org/doc/man/yecc.html>

#### Input File: Grammar

Yecc takes as input a file with a BNF grammar definition and produces an
Erlang module for a parser.

The grammar file has the following format:

Optional header section. For example:

    Header "%% Copyright (C)"
    "%% @private"
    "%% @Author John"

We don't have one.

Nonterminal categories to be used in the grammar rules. For example:

    Nonterminals program topdec_list topdec vardec scalardec.

Terminal categories, which correspond to the categories of tokens
produced by the lexer. For example:

    Terminals '&&' '||' '!' 'return' 'void' 'while'.

Start symbol of the grammar. For example:

    Rootsymbol program.

Optional declaration of the `end_of_input` symbol that the lexer is expected to
use. For example:

    Endsymbol '$end'.

We don't have one (which implies the default produced by Leex).

Optional declarations of operator precedence and associativity. For example:

    Right 100 '='.
    Nonassoc 200 '==' '=/='.
    Left 300 '+'.
    Left 400 '*'.
    Unary 500 '-'.

We don't use any operator precedence declarations since we resolve these
kind of ambiguities by modifying the grammar. See the section "The Grammar"
below.

Grammar rules. Each rule has the general form:

    Nonterminal -> MixOfTerminalsAndNonTerminals : ActionCode.

For example, this parses top-level declarations into a list:

    topdec_list -> '$empty' : [].
    topdec_list -> topdec topdec_list : ['$1'|'$2'].

Any function definitions used by the rules go into the `Erlang code.`
section. For example:

    Erlang code.

    type(Token) ->
        erlang:element(1, Token).

#### Output File: Parser

When feeding the input file to Yecc using `yecc:file(parser)`, where _parser_
is the name of the input file, Yecc generates a complete LALR(1) parser
according to the grammar definition. This file is placed beside the
grammar file but given a different file extension: ".erl".

## Grammar

See `src/parser/parser.yrl` for the complete grammar definition.

### Precedence and Associativity

Precedence of binary operators was handled by introducing new nonterminals,
splitting the productions from lower to higher precedence.

Instead of having:

    expr  -> expr binop expr
    binop -> '='
    binop -> '||'
    binop -> '&&'
    ...

... we introduced:

    expr -> rval
    rval -> lval '=' rval
    rval -> or
    or   -> or '||' and
    or   -> and
    and  -> and '&&' comp
    ...

Which forces the correct derivation of precedence between =, || and &&, etc.
We continued in a similar fashion for the rest of the binary operators.

Associativity was handled by preventing left associative operators from
producing itself on the right hand side, and right associative operators
from producing itself on the left hand side.

For example, '=' is right associative. The rule `rval -> lval '=' rval` in
the example above is only able to derive new '=' on the right hand side.
Resulting in a derivation of the form `lval = (lval = (lval = or))`.

The same goes for left associative operators, such as '||', which is only able
to derive new '||' on the left hand side. Resulting in a derivation of the
form `((and || and) || and) || and`.

### Top-Level Declarations

We eliminated the problem of parsing top-level declarations by using the
proposed solution of adding an additional production. Resulting in:

    funtypeandname -> typename 'identifier' : SomeAction.
    funtypeandname ->   'void' 'identifier' : SomeAction.

### Dangling Else

With our grammar, Yecc reports one shift/reduce warning because we
don't handle the dangling else ambiguity explicitly in the grammar.

Although we can "safely" ignore this warning since the parsing algorithm in
Yecc will produce the correct derivation since it favors shifting over
reducing. From Yecc's documentation:

_"Shift/reduce conflicts are resolved in favor of shifting if there are no
operator precedence declarations."_

## Abstract Syntax Tree

In our representation of the abstract syntax tree (AST), there are 16
different kinds of nodes. Each kind of node has an associated constructor.

### Meta Data

Each node has, as its first element, a tuple of meta data. The format of the
meta data is:

    {Line, Tag}

Line is the line number where Tag starts. Tag is the name of the node.

### Nodes

#### Node: program

Root node. The format is:

    {{Line, program}, Topdecs}

`Topdecs` is a list of top-level declarations: a mix of `scalardec`,
`arraydec`, `fundec` and `fundef`.

#### Node: fundec

Function declarations. The format is:

    {{Line, fundec}, Type, Name, Formals}

`Type` is the return type of the function. `Name` is the name of the
function. `Formals` is a list of formal arguments: a mix of `scalardec` and
`formal_arraydec`.

#### Node: fundef

Function definitions. The format is:

    {{Line, fundef}, Type, Name, Formals, Body}

`Type` is the return type of the function. `Name` is the name of the
function. `Formals` is a list of formal arguments: a mix of `scalardec`
and `formal_arraydec`. `Body` is the function's statement body.

#### Node: funtypeandname

Return type and name of function. The format is:

    {{Line, funtypeandname}, Type, Ident}

`Type` is either `int`, `char` or `void`.

Note that the resulting parse
tree does not contain this node anywhere. It is just used internally to
solve the problem of parsing top-levels. A function definition or declaration
will extract the type and name from this node an incorporate that data
directly.

#### Node: scalardec

The format is:

    {{Line, scalardec}, Type, Ident}

`Type` is `int` or `char`. `Ident` is the identifier value.

#### Node: arraydec

The format is:

    {{Line, arraydec}, Type, Ident, Size}

`Type` is `int` or `char`. `Ident` is the identifier value. `Size` is an
integer which indicates the size of the array.

#### Node: formal_arraydec

The format is:

    {{Line, formal_arraydec}, Type, Ident}

`Type` is `int` or `char`. `Ident` is the identifier value.

#### Node: if

If statements. The format is:

    {{Line, 'if'}, Cond, ThenStmts, ElseStmts}

`Cond` is an expression (see the rule `expr -> rval` in the grammar).
`ThenStmts` and `ElseStmts` is either a list of statements, or a single tuple
including a statement (see the multiple `stmt` rules in the grammar).

#### Node: while

While statements. The format is:

    {{Line, while}, Cond, Stmts}

`Cond` is an expression (see the rule `expr -> rval` in the grammar).
`Stmts` is either a list of statements, or a single tuple including a
statement (see the multiple `stmt` rules in the grammar).

#### Node: return

Return has two kinds of nodes, either:

    {{Line, return}}

... which indicates return void, or:

    {{Line, return}, Expr}

... where `Expr` is an expression (see the rule `expr -> rval` in the
grammar).

#### Node: function_call

Function calls. The format is:

    {{Line, funcall}, Ident, Actuals}

`Ident` is the identifier value.

`Actuals` is a list of expressions.

#### Node: arrelem

Array elements. The format is:

    {{Line, arrelem}, Ident, Index}

`Ident` is the identifier value. `Index` is an expression.

#### Node: binop

Binary operators. The format is:

    {{Line, binop}, Lhs, Op, Rhs}

`Lhs` and `Rhs` is an expression. `Op` is the operator value.

#### Node: unop

Unary operators. The format is:

    {{Line, unop}, Op, Rhs}

`Op` is the operator value. `Rhs` is an expression.

#### Node: ident

Alphanumeric identifiers. The format is:

    {{Line, ident}, Value}

`Value` is the identifier value.

#### Node: intconst

Integer constants. The format is:

    {{Line, intconst}, Value}

`Value` is the integer value.

#### Node: charconst

Character literals. The format is:

    {{Line, charconst}, Value}

`Value` is the character value (although not the integer constant whose value
is the representation code of the character).

## Running the Parser

### Multi-Step Compilation

The parser (and the lexer) has been implemented to read from standard input
and output to standard output. As such it's possible to, for example, pipe
the result from the lexer to the parser:

    cat suite/quiet/parser/p01.c | lexer | parser

The parser will output the resulting abstract syntax tree to standard output.

### Single-Step Compilation

For a more conventional approach to compilation, a script called `ducc` can
be used. `ducc` takes as argument a single file name and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

### Error Handling

When a script (such as `lexer` or `parser`) receives invalid input, an error
message is printed and execution stops.

A lexical error exhibited by the lexer looks like this:

    Lexical error on line 13, illegal: "%"

A syntax error exhibited by the parser looks like this:

    Syntax error on line 5, before: "')'"

If an unreadable file is given to `ducc`, the error looks like this:

    Error reading file: some_unreadable_file

## Questions and Issues

### AST

As of now, we have strictly separated most node types using tags.
For instance, we have variable declarations that look like this:

    {{1,scalardec},char,a},
    {{2,arraydec},int,b,42},

Would it be better (or merely just a different solution?) to merge them
into a "vardec" and use something like "nil" appropriately? Like this:

    {{1,vardec},char,a,nil},
    {{2,vardec},int,b,42},
