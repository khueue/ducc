# Assignment 1 - Lexer - Completion 1

Compiler Project, VT11

2011-02-07

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are also available on the IT departments server:
/home/emhe9781/src/

## Completion

### Problem

We didn't handle comments on the form `/* * / */`.

New regexp looks like:

    BegCom           = (/\*)
    EndCom           = (\*/)
    NotStar          = [^*]
    NeitherStarSlash = [^*/]
    Star             = (\*)

    {BegCom}({NotStar}|{Star}+{NeitherStarSlash})*{Star}*{EndCom} :
        skip_token.
