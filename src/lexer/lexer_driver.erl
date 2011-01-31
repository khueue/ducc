-module(lexer_driver).
-export([tokenize/2]).

tokenize(Stream, String) ->
    case lexer:string(String) of
        {ok, Tokens, _} -> {ok, Tokens};
        Error           -> handle_error(Stream, Error)
    end.

handle_error(Stream, {error, {Line, _Module, {Message, Fault}}, _}) ->
    io:format('~s:~p: lexical error, ~p: ~p~n', [Stream, Line, Message, Fault]),
    {error, tokenizer};
handle_error(_Stream, Unknown) ->
    io:write(Unknown), io:nl(),
    {error, tokenizer}.
