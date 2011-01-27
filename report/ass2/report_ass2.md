# Assignment 2 - Parser

Compiler Project, VT11

2011-01-XX

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

Project Repository at GitHub: 
<https://github.com/khueue/ducc/tree/ass2-parser>

The source and executables are also available on the IT department server:
/home/emhe9781/src/ducc/ 

(Note that the source won't compile on the IT department servers since they
have an old release of OTP which doesn't include Leex.)


## Introduction

XXX


## Tools Used

### Yecc

Thankfully, the Erlang environment provides ports of both Lex and Yacc, namely
Leex and Yecc. Yecc documentation can be found here:
<http://www.erlang.org/doc/man/yecc.html>

#### Input File: Grammar

Yecc takes as input a file with a BNF grammar definition and produces a 
Erlang module for a parser.

The grammar file has the following format:

Optional header section. For example:

    Header "%% Copyright (C)"
    "%% @private"
    "%% @Author John"

We don't have one.

Nonterminal categories to be used in the grammar rules. For example:

    Nonterminals program topdec_list topdec vardec scalardec.

Terminal categories, which are the categories of tokens produced by the 
lexer. For example:

    Terminals '&&' '||' '!' 'return' 'void' 'while'.

Start category of the grammar. For example:

    Rootsymbol program.

Optional declaration of the `end_of_input` symbol that the lexer is expected to
use. For example:

    Endsymbol '$end'.

We don't have one.

Optional declarations of operator precedences. For example:

    Right 100 '='.
    Nonassoc 200 '==' '=/='.
    Left 300 '+'.
    Left 400 '*'.
    Unary 500 '-'.

We don't use any operator precedence declarations since we resolve these 
kind of ambiguities be modifying the grammar. See the section "The Grammar"
below.

Grammar rules. Each rule has the general form:
    
    Left_Hand_Side -> Right_Hand_Side : Associated_Code.

For example:
    
    program          -> topdec_list : {program, '$1'}.

Function definitions goes in the `Erlang code.` section:

    Erlang code.

    type_of(Token) ->
        erlang:element(1, Token).


#### Output File: Parser

When feeding the input file to Yecc using `yecc:file(parser)`, where _parser_ 
is the name of the input file, Yecc generates a complete parser according to 
the grammar definition. This file is placed beside the grammar file but named 
with a different file extension: ".erl".


## The Grammar

### Precedence of binary operators

XXX


### Associativity of binary operators

XXX


### Top-level declarations

XXX


### Dangling else

Yecc reports one shift/reduce warning because we don't handle the dangling
else ambiguity explicitly in the grammar.

Although we can "safely" ignore this warning since the parsing algorithm in 
Yecc will produce the correct derivation since it favors shifting over 
reducing. 

Quoting Yecc's documentation:

"_Shift/reduce conflicts are resolved in favor of shifting if there are no 
operator precedence declarations._"


## The Abstract Syntax Tree (AST)

Representation of the abstract syntax tree. XXX

### Node XXX

### Node XXX


## Running the parser

The parser has been implemented to read from standard input. As such it's 
possible to, for example, pipe the result from the lexer to the parser:

    cat suite/quiet/parser/p01.c | lexer | parser

... and the parser will output the resulting abstract syntax tree to standard
output.

XXX
