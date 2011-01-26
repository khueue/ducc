-module(parser_driver).
-export([start/0]).

start() ->
    {_, Tokens} = term_io:stdin_to_term(),
    {_, ParseTree} = parser:parse(Tokens),
    term_io:term_to_stdout(ParseTree),
    init:stop().
