-module(parser_driver).
-export([parse/1]).

parse(Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} ->
            ParseTree;
        Error ->
            io_tools:die(Error)
    end.
