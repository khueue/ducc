-module(test_tools).
-export([test_tuple/1, run_tests/3, expected_output/2, command/2]).
-include_lib("eunit/include/eunit.hrl").

test_tuple(Fun) ->
    {timeout, 1200, [{?LINE, Fun}]}.

run_tests([dummy], _, _) ->
    ok;
run_tests([File|Files], ExpectedFun, CommandFun) ->
    run_test(File, ExpectedFun, CommandFun),
    run_tests(Files, ExpectedFun, CommandFun).

run_test(File, ExpectedFun, CommandFun) ->
    Expected = ExpectedFun(File),
    Command = CommandFun(File),
    ?assertCmdOutput(Expected, Command).

command(File, Programs) ->
    Cat = "cat " ++ File,
    pipes([Cat|Programs]).

pipes([Program]) ->
    Program;
pipes([Program|Programs]) ->
    Program ++ " | " ++ pipes(Programs).

expected_output(File, Suffix) ->
    OutputFile = output_file(File, Suffix),
    file_to_string(OutputFile).

output_file(File, Suffix) ->
    File ++ "." ++ Suffix.

file_to_string(File) ->
    {ok, Binary} = file:read_file(File),
    binary_to_list(Binary).
