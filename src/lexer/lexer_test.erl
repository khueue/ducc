-module(lexer_test).
-include_lib("eunit/include/eunit.hrl").

lexer_test_() ->
    {timeout, 1200, [{?LINE, fun() -> test_suite() end}]}.

run_lexer_tests(Tests) ->
    test_tools:run_tests(
        Tests,
        fun(F) -> lexer_exp(F) end,
        fun(F) -> lexer_cmd(F) end).

lexer() ->
    "lexer".

lexer_exp(File) ->
    test_tools:expected_output(File, lexer()).

lexer_cmd(File) ->
    test_tools:command(File, [lexer()]).

test_suite() ->

    ValidTests =
    [
        "suite/quiet/lexer/l01.c",
        "suite/quiet/lexer/l02.c",
        "suite/quiet/lexer/l03.c",
        "suite/quiet/lexer/l04.c",
        "suite/quiet/lexer/l05.c",
        "suite/quiet/lexer/l06.c",
        dummy
    ],

    run_lexer_tests(ValidTests),

    ?assertCmd("cat suite/incorrect/lexer/good.c | lexer"),

    ?assertCmd("cat suite/noisy/simple/sim01.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim02.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim03.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim04.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim05.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim06.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim07.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim08.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim09.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim10.c | lexer"),
    ?assertCmd("cat suite/noisy/simple/sim11.c | lexer"),

    ?assertCmd("cat suite/noisy/medium/circle.c | lexer"),
    ?assertCmd("cat suite/noisy/medium/fac-b.c | lexer"),
    ?assertCmd("cat suite/noisy/medium/fac.c | lexer"),
    ?assertCmd("cat suite/noisy/medium/fib.c | lexer"),

    ?assertCmd("cat suite/noisy/advanced/8queens.c | lexer"),
    ?assertCmd("cat suite/noisy/advanced/bubble.c | lexer"),
    ?assertCmd("cat suite/noisy/advanced/eval.c | lexer"),
    ?assertCmd("cat suite/noisy/advanced/primes.c | lexer"),
    ?assertCmd("cat suite/noisy/advanced/quick.c | lexer"),

    ?assertCmdStatus(1, "cat suite/incorrect/lexer/bad.c | lexer"),
    ?assertCmdStatus(1, "cat suite/incorrect/lexer/long-char.c | lexer"),
    ?assertCmdStatus(1, "cat suite/incorrect/lexer/ugly.c | lexer").
