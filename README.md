# ducc - Double uC Compile!!!

A pair-programmed toy compiler for the uC language (a subset of C) developed
in Erlang during two months of bliss.

Rationale: <http://www.youtube.com/watch?v=FJ_VAh-V0EE>

## Dependencies

Requires a recent enough Erlang to have eunit/escript/leex/yecc available
(or installed manually, I guess). SPIM is useful to make any interesting
sense of the generated assembly.

## Usage

The compiler takes a file written in uC and compiles it down to MIPS assembly
which can then be run through SPIM or just browsed with a cup of tea.

Simple usage: `bin/ducc file.c`

Also play around with: `cat file.c | bin/lexer | bin/parser | etc...`

See the tests in `/suite` for many examples on how the compiler operates
at every step. The steps are, in order of execution (and development):

 1. lexer (characters to lexemes)
 2. parser (lexemes to AST)
 3. analyzer (AST to AST (untouched))
 4. translator (AST to RTL)
 5. codegen (RTL to assembly structure)
 6. emitter (assembly structure to assembly text)

Although the reports in `/report` became a bit outdated with each subsequent
step, they still accurately convey most of the thoughts and details of the
design of the compiler.
