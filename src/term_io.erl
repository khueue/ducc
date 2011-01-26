-module(term_io).
-export([stdin_to_term/0, term_to_stdout/1]).

stdin_to_term() ->
    Prompt = '',
    io:read(Prompt).

term_to_stdout(Term) ->
    io:format('~p.~n', [Term]).
