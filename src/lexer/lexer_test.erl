-module(lexer_test).
-include_lib("eunit/include/eunit.hrl").

lexer_test_() ->
    Tests = all_tests(),
    Program = "lexer",
    ToolChain = ["lexer"],
    Fun = fun() -> test_tools:run_all_tests(Tests, Program, ToolChain) end,
    test_tools:test_tuple(Fun).

all_tests() ->
    [
        "suite/quiet/lexer/l01.c",
        "suite/quiet/lexer/l02.c",
        "suite/quiet/lexer/l03.c",
        "suite/quiet/lexer/l04.c",
        "suite/quiet/lexer/l05.c",
        "suite/quiet/lexer/l06.c",
        "suite/incorrect/lexer/good.c",
        "suite/noisy/simple/sim01.c",
        "suite/noisy/simple/sim02.c",
        "suite/noisy/simple/sim03.c",
        "suite/noisy/simple/sim04.c",
        "suite/noisy/simple/sim05.c",
        "suite/noisy/simple/sim06.c",
        "suite/noisy/simple/sim07.c",
        "suite/noisy/simple/sim08.c",
        "suite/noisy/simple/sim09.c",
        "suite/noisy/simple/sim10.c",
        "suite/noisy/simple/sim11.c",
        "suite/noisy/medium/circle.c",
        "suite/noisy/medium/fac-b.c",
        "suite/noisy/medium/fac.c",
        "suite/noisy/medium/fib.c",
        "suite/noisy/advanced/8queens.c",
        "suite/noisy/advanced/bubble.c",
        "suite/noisy/advanced/eval.c",
        "suite/noisy/advanced/primes.c",
        "suite/noisy/advanced/quick.c",
        "suite/incorrect/lexer/bad.c",
        "suite/incorrect/lexer/long-char.c",
        "suite/incorrect/lexer/ugly.c",
        dummy
    ].
