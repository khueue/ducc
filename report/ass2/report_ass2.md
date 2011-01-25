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

Yecc takes as input a file (with file extension ".yrl") with a BNF grammar
definition and produces a Erlang module for a parser.

XXX


#### Output File: Parser

XXX


## Handling precedence and associativity of binary operators

### Precedence of binary operators

XXX


### Associativity of binary operators

XXX


## Handling top-level declarations

XXX


## Handling dangling else

XXX


## Running the parser

XXX
