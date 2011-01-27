-module(term_io).
-export([stdin_to_term/0, term_to_stdout/1]).

stdin_to_term() ->
    Prompt = '',
    io:read(Prompt).

term_to_stdout(Term) ->
    case Term of
        {_,parser,_} ->
            io:format('~p.~n', [Term]),
            erlang:halt(1);
        _ ->
            io:format('~p.~n', [Term])
    end.
