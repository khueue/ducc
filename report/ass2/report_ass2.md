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

Our compiler will be written in Erlang. Neither of us has any previous 
experience with Erlang, so we hope this to be both challenging and fun.

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

Start symbol of the grammar. For example:

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
kind of ambiguities by modifying the grammar. See the section "The Grammar"
below.

Grammar rules. Each rule has the general form:
    
    Left_Hand_Side -> Right_Hand_Side : Associated_Code.

For example:
    
    program          -> topdec_list : {program, '$1'}.

Function definitions goes in the `Erlang code.` section. For example:

    Erlang code.

    type_of(Token) ->
        erlang:element(1, Token).


#### Output File: Parser

When feeding the input file to Yecc using `yecc:file(parser)`, where _parser_ 
is the name of the input file, Yecc generates a complete parser according to 
the grammar definition. This file is placed beside the grammar file but named 
with a different file extension: ".erl".


## The Grammar

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

Associativity was handled by preventing left associative operators to produce 
itself on the right hand side, and right associative operators to produce 
itself on the left hand side.

For example, '=' is right associative. The rule `rval -> lval '=' rval` in 
the example above is only able to derive new '=' on the right hand side.
Resulting in a derivation of the form `lval=(lval=(lval=or))`.

The same goes for left associative operators, such as '||', which is only able 
to derive new '||' on the left hand side. Resulting in a derivation of the
form `((and || and) || and) || and`.


### Top-Level Declarations

We eliminated the problem of parsing top-level declarations by using the 
proposed solution of adding an additional production. Resulting in:

    funtypeandname   -> typename 'identifier' : Associated_Code.
    funtypeandname   ->   'void' 'identifier' : Associated_Code.


### Dangling Else

Yecc reports one shift/reduce warning because we don't handle the dangling
else ambiguity explicitly in the grammar.

Although we can "safely" ignore this warning since the parsing algorithm in 
Yecc will produce the correct derivation since it favors shifting over 
reducing. 

Quoting Yecc's documentation:

"_Shift/reduce conflicts are resolved in favor of shifting if there are no 
operator precedence declarations._"


## Abstract Syntax Tree

In our representation of the abstract syntax tree (AST), there are 17 different
kinds of nodes. Each kind of node has an associated constructor. See 
description below.

### Node: program

This is the root node of every program.

Format: {{Line, `program`}, ListOfTopLevelDecl}

### Node: fundef

Function definitions.

Format: {{Line, `fundef`}, `funtypeandname`, Formals, `funbody`}

Where Formals is a list of `scalardec` and `formal_arraydec`.

### Node: funtypeandname

XXX

### Node: scalardec

XXX

### Node: arraydec

XXX

### Node: funbody

XXX

### Node: formal_arraydec

XXX

### Node: if

XXX

### Node: while

XXX

### Node: return

XXX

### Node: function_call

XXX

### Node: array_element

XXX

### Node: binop

XXX

### Node: unop

XXX

### Node: ident

Alphanumeric identifiers. 

Format: {{Line, `ident`}, Value}

### Node: intconst

Integer constants.

Format: {{Line, `intconst`}, Value}

### Node: charconst

Character literals.

Format: {{Line, `charconst`}, Value}


## Running the Parser

### Multi-Step Compilation

The parser (and the lexer) has been implemented to read from standard input
and output to standard output. As such it's possible to, for example, pipe
the result from the lexer to the parser:

    cat suite/quiet/parser/p01.c | lexer | parser

The parser will output the resulting abstract syntax tree to standard output.

### Single-Step Compilation

For a more conventionl approach to compilation, a script called `ducc` can
be used. `ducc` takes as argument a single file name and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

### Error Handling

When a script (such as `lexer` or `parser`) receives invalid input, an error
is printed and execution stops.

A lexical error exhibited by the lexer looks like this:

    Lexical error on line 13, illegal: "%"

A syntax error exhibited by the parser looks like this:

    Syntax error on line 5, before: "')'"

If an unreadable file is given to `ducc`, the following error appears:

    Error reading file: some_unreadable_file

# Questions

## AST

As of now, we have strictly separated most node types using tags.
For instance, we have variable declarations that look like this:

    {{1,scalardec},char,a},
    {{2,arraydec},int,b,42},

Would it be better (or just different?) to merge them into a "vardec"
and use something like "nil" appropriately? Like this:

    {{1,vardec},char,a,nil},
    {{2,vardec},int,b,42},
