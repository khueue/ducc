-module(emitter_test).
-include_lib("eunit/include/eunit.hrl").

emitter_test_() ->
    Tests = all_tests(),
    Program = "emitter",
    Fun = fun() -> test_tools:run_all_tests(Tests, Program) end,
    test_tools:test_tuple(Fun).

all_tests() ->
    filelib:fold_files("suite", ".+.c$", true, fun(F, Acc) -> [F|Acc] end, []).
