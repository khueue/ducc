-module(test_tools).
-include_lib("eunit/include/eunit.hrl").
-export([
    test_tuple/1,
    run_all_tests/2,
    run_tests/3,
    expected_output/2,
    command/2]).

test_tuple(TestFun) ->
    {timeout, 1200, [{?LINE, TestFun}]}.

run_all_tests(Tests, Program) ->
    ExpectFun  = fun(File) -> expected_output(File, Program) end,
    CommandFun = fun(File) -> command(File, Program) end,
    run_tests(Tests, ExpectFun, CommandFun).

run_tests([], _, _) ->
    ok;
run_tests([File|Files], ExpectedFun, CommandFun) ->
    run_test(File, ExpectedFun, CommandFun),
    run_tests(Files, ExpectedFun, CommandFun).

run_test(File, ExpectedFun, CommandFun) ->
    Expected = ExpectedFun(File),
    Command  = CommandFun(File),
    ?assertCmdOutput(Expected, Command).

command(File, Program) ->
    Ducc = "ducc ",
    case Program of
        "lexer" ->
            Ducc ++ "-l " ++ File;
        "parser" ->
            Ducc ++ "-p " ++ File;
        "analyzer" ->
            Ducc ++ "-a " ++ File;
        "translator" ->
            Ducc ++ "-t " ++ File;
        "codegen" ->
            Ducc ++ "-c " ++ File;
        _Default ->
            Ducc ++ File
    end.

expected_output(File, Suffix) ->
    OutputFile = output_file(File, Suffix),
    file_to_string(OutputFile).

output_file(File, Suffix) ->
    File ++ "." ++ Suffix.

file_to_string(File) ->
    {ok, Binary} = file:read_file(File),
    binary_to_list(Binary).
