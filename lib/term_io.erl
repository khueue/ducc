-module(term_io).
-export([stdin_to_term/0, term_to_stdout/1]).

stdin_to_term() ->
    {ok, Term} = io:read(''),
    Term.

term_to_stdout(Term) ->
    io:format('~p.~n', [Term]).
