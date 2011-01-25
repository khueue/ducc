-module(driver_io).
-export([input_to_term/0, term_to_output/1]).

stdin_to_term() ->
    {ok, Term} = io:read(''),
    Term.

term_to_stdout(Term) ->
    io:format('~p.~n', [Term]).

% ->
%    {_, Tokens, _} = erl_scan:string(Str),
%    {_, Str2} = erl_parse:parse_term(Tokens),
%    Str2.
