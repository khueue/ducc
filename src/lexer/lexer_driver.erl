-module(lexer_driver).
-export([tokenize/1]).

tokenize(String) ->
    case lexer:string(String) of
        {ok, Tokens, _} ->
            Tokens;
        Error ->
            io_tools:die(Error)
    end.
