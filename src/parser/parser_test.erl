-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

parser_test_() ->
    {timeout, 1200, [{?LINE, fun() -> test_suite() end}]}.

test_suite() ->

    %% Tests which should pass

    ?assertCmd("cat suite/quiet/lexer/l04.c | lexer | parser"),
    ?assertCmd("cat suite/quiet/lexer/l05.c | lexer | parser"),

    ?assertCmd("cat suite/quiet/parser/p01.c | lexer | parser"),
    ?assertCmd("cat suite/quiet/parser/p02.c | lexer | parser"),
    ?assertCmd("cat suite/quiet/parser/p03.c | lexer | parser"),
    ?assertCmd("cat suite/quiet/parser/p04.c | lexer | parser"),
    ?assertCmd("cat suite/quiet/parser/p05.c | lexer | parser"),

    ?assertCmd("cat suite/noisy/simple/sim01.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim02.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim03.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim04.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim05.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim06.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim07.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim08.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim09.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim10.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/simple/sim11.c | lexer | parser"),

    ?assertCmd("cat suite/noisy/medium/circle.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/medium/fac-b.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/medium/fac.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/medium/fib.c | lexer | parser"),

    ?assertCmd("cat suite/noisy/advanced/8queens.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/advanced/bubble.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/advanced/eval.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/advanced/primes.c | lexer | parser"),
    ?assertCmd("cat suite/noisy/advanced/quick.c | lexer | parser"),

    %% Tests which should fail (generate status value 1)

    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe01.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe02.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe03.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe04.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe05.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe06.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe07.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe08.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe09.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe10.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe11.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe12.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe13.c | lexer | parser"),
    ?assertCmdStatus(1, "cat suite/incorrect/parser/pe14.c | lexer | parser").
