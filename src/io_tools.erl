-module(io_tools).
-export([
    string_from_input/0,
    string_from_stream/1,
    term_from_input/0,
    term_to_output/1,
    die/1]).

string_from_input() ->
    string_from_stream(standard_io).

string_from_stream(Stream) ->
    Prompt = '',
    case io:get_chars(Stream, Prompt, 8192) of
        eof ->
            [];
        Text ->
            Text ++ string_from_stream(Stream)
    end.

term_from_input() ->
    Prompt = '',
    io:read(Prompt).

term_to_output(Term) ->
    io:format('~p.~n', [Term]).

die(Message) ->
    io:format('~p~n', [{lexical_error, Message}]),
    erlang:halt().
