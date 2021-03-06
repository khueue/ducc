# Assignment 1 - Lexer

Compiler Project, VT11

2011-01-21

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

Project Repository at GitHub: <https://github.com/khueue/ducc/tree/ass1-lexer>

The source and executables are also available on the IT departments server:
/home/emhe9781/src/ducc/

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
<http://www.erlang.org/doc/man/leex.html>

#### Input File: Specification

Leex takes as input a file (with file extension ".xrl") with token definitions
and gives as output a customized Erlang module, specifically molded from
the token definitions.

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
     the corresponding Erlang code is executed, usually emitting a token for
     the input matched.
 * _Erlang code_. Any additional code needed by the above.

#### Output File: Lexer

When feeding the input file to Leex using `leex:file(lexer)`, where _lexer_
is the name of the input file, Leex generates a complete lexer according
to the specification. This file is placed beside the specification but named
with a different file extension: ".erl".

See section "main.erl" below for information on how to use the lexer.

## Handling Comments

### Single-Line Comments

Single-line comments are handled by the simple regexp:

    {LineComment}(.*)

where _LineComment_ is defined as `//`. The dot regexp does not match
newline in Leex, so we did not have to handle that specifically.

### Multi-Line Comments

Handling multi-line comments was the trickiest part of the lexer.
We eventually arrived at the following expression:

    {MultiCommentStart}(/*){InComment}*(\**){MultiCommentEnd}

where _MultiCommentStart_ is `/*` and _MultiCommentEnd_ is `*/`. The tricky
part is what is between: Immediately after the start, any number of slashes
may be present. Then comes a lot of stuff, and just before the closing
marker is any number of stars. The stuff in between that, _InComment_, is
defined as:

    ([^*/]|[^*]/|\*[^/])

This prohibits the sequences `/*` and `*/` from appearing inside the comment,
but anything else is allowed. This combined should be able to cope with
proper multi-line comments in uC (and C).

## Handling EOF

Since we are using Leex to create the lexical analyser we didn't need to
handle eof explicitly.

## Running the lexer

To perform lexical analysis on a stream, e.g. a uC source file, and to
output the result to the standard output we used the erlang file and io
modules.

The function start/1 in the module main takes a file name as an argument and
opens a stream. The stream is then passed to the function process_file/1.

The function process_file/1 uses the io:request/2 function which uses the
generated lexer in order to tokenize the input stream.
Upon seeing a valid token, the token and its value is printed to standard
output using the io:format/2 function. If it sees a lexical error, an
error message will be printed to standard output.
