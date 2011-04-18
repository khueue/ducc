# ducc - Double uC Compile!!!

Rationale: <http://www.youtube.com/watch?v=FJ_VAh-V0EE>

## Usage

The compiler takes a file written in uC and compiles it down to MIPS assembly
which can then be run through SPIM.

Simple usage: `bin/ducc file.c`

Also play around with: `cat file.c | bin/lexer | bin/parser | etc...`

See the tests in `/suite` for many examples on how the compiler operates
at every step.

Although the reports in `/report` became a bit outdated with each subsequent
step, they still accurately convey most of the thoughts and details of the
design of the compiler.
