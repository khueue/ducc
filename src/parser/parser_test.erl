-module(parser_test).
-include_lib("eunit/include/eunit.hrl").

parser_test_() ->
    Tests = all_tests(),
    Program = "parser",
    ToolChain = ["lexer","parser"],
    Fun = fun() -> test_tools:run_all_tests(Tests, Program, ToolChain) end,
    test_tools:test_tuple(Fun).

all_tests() ->
    filelib:fold_files("suite", ".+.c$", true, fun(F, Acc) -> [F|Acc] end, []).
