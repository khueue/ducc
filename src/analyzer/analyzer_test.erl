-module(analyzer_test).
-include_lib("eunit/include/eunit.hrl").

analyzer_test_() ->
    Tests = all_tests(),
    Program = "analyzer",
    ToolChain = ["lexer","parser","analyzer"],
    Fun = fun() -> test_tools:run_all_tests(Tests, Program, ToolChain) end,
    test_tools:test_tuple(Fun).

all_tests() ->
    [
        % "testfil4.c",
        "suite/incorrect/semantic/se01.c",
        "suite/incorrect/semantic/se02.c",
        "suite/incorrect/semantic/se03.c",
        "suite/incorrect/semantic/se04.c",
        "suite/incorrect/semantic/se05.c",
        "suite/incorrect/semantic/se06.c",
        "suite/incorrect/semantic/se07.c",
        "suite/incorrect/semantic/se08.c",
        "suite/incorrect/semantic/se09.c",
        "suite/incorrect/semantic/se10.c",
        "suite/incorrect/semantic/se11.c",
        "suite/incorrect/semantic/se12.c",
        "suite/incorrect/semantic/se13.c",
        "suite/incorrect/semantic/se14.c",
        "suite/incorrect/semantic/se15.c",
        "suite/incorrect/semantic/se16.c",
        "suite/incorrect/semantic/se17.c",
        "suite/incorrect/semantic/se18.c",
        "suite/incorrect/semantic/se19.c",
        "suite/incorrect/semantic/se20.c",
        "suite/incorrect/semantic/se21.c",
        "suite/incorrect/semantic/se22.c",
        "suite/incorrect/semantic/se23.c",
        "suite/incorrect/semantic/se24.c",
        "suite/incorrect/semantic/se25.c",
        "suite/incorrect/semantic/se26.c",
        "suite/incorrect/semantic/se27.c",
        "suite/incorrect/semantic/se28.c",
        "suite/incorrect/semantic/se29.c",
        "suite/incorrect/semantic/se30.c",
        "suite/incorrect/semantic/se31.c",
        "suite/incorrect/semantic/se32.c",
        "suite/incorrect/semantic/se33.c",
        "suite/incorrect/semantic/se34.c",
        dummy
    ].
