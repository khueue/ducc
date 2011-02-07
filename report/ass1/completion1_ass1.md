# Assignment 1 - Lexer - Completion 1

Compiler Project, VT11

2011-02-07

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT departments server:
/home/emhe9781/src/ducc-2011-02-07.tar.gz

(Note that the source won't compile and the executables won't run on the IT
department servers since they have an old release of Erlang which doesn't
include Leex nor escript).

## Completion

### Problem

We didn't handle comments of the form `/* * / */`.

New regexp looks like:

    BegCom           = (/\*)
    EndCom           = (\*/)
    NotStar          = [^*]
    NeitherStarSlash = [^*/]
    Star             = (\*)

    {BegCom}({NotStar}|{Star}+{NeitherStarSlash})*{Star}*{EndCom} :
        skip_token.
