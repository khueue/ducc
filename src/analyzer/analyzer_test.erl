-module(analyzer_test).
-include_lib("eunit/include/eunit.hrl").

analyzer_test_() ->
    Tests = all_tests(),
    Program = "analyzer",
    ToolChain = ["lexer","parser","analyzer"],
    Fun = fun() -> test_tools:run_all_tests(Tests, Program, ToolChain) end,
    test_tools:test_tuple(Fun).

all_tests() ->
    filelib:fold_files("suite", ".+.c$", true, fun(F, Acc) -> [F|Acc] end, []).
