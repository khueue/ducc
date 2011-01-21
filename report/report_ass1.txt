# Assignment 1 - Lexer

Compiler Project, VT11

Emil Hessman (emheXXXX)
Sebastian Lundström (selu7901)

2011-01-21

## Introduction

Our compiler will be written in Erlang. Neither of us has any previous
experience with Erlang, so we hope this to be both challenging and fun.

This assignment involves creating a lexer for the uC language (a subset of C),
so this is what the report will focus on: understanding the lexical part of
uC, learning Leex and arriving at a (hopefully) working lexer.

## Tools Used

### Leex

Thankfully, the Erlang environment provides ports of both Lex and Yacc, namely Leex and Yecc. Leex takes as input a file with token definitions and gives as
output a customized Erlang module, specifically molded from the token
definitions.

## Handling comments

### Single-line comments

Single-line comments, //, were handled by the simple regexp:

    {LineComment}(.*)

where LineComment is defined as //. The dot regexp does not match newline in
Leex, so we did not have to handle that specifically.

### Multi-line comments 

Coming soon. /Seb
