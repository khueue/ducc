# Assignment 1 - Lexer

Compiler Project, VT11

2011-01-21

Emil Hessman (emheXXXX)

Sebastian Lundström (selu7901)

Git repository: https://github.com/khueue/ XXXXXXX tag?

## Introduction

Our compiler will be written in Erlang. Neither of us has any previous
experience with Erlang, so we hope this to be both challenging and fun.

This assignment involves creating a lexer (lexical analyser) for the uC
language (a subset of C), so this is what the report will focus on:
understanding the lexical part of uC, learning Leex and arriving at a
(hopefully) working lexer.

## Tools Used

### Leex

Thankfully, the Erlang environment provides ports of both Lex and Yacc, namely
Leex and Yecc. Leex documentation can be found here:

    http://www.erlang.org/doc/man/leex.html

#### Input File

Leex takes as input a file with token definitions and gives as output a
customized Erlang module, specifically molded from the token definitions.

The definitions file has the following format:

    <Header>

    Definitions.
    <Macro Definitions>

    Rules.
    <Token Rules>

    Erlang code.
    <Erlang code>

We have no "Header" section, but the other sections are present. The sections
are defined as follows:

 * _Macro Definitions_. Named regular expressions, to make the rules more
   readable and avoid duplication.
 * _Rules_. Pairs of "Regexp : Erlang code". When Regexp matches input, the
   the corresponding Erlang code is executed, usually emitting a token based
   on the input matched.
 * _Erlang code_. Any additional code needed by the above.

## Handling Comments

### Single-Line Comments

Single-line comments, //, were handled by the simple regexp:

    {LineComment}(.*)

where LineComment is defined as `//`. The dot regexp does not match newline in
Leex, so we did not have to handle that specifically.

### Multi-Line Comments 

Coming soon. /Seb

## Links


