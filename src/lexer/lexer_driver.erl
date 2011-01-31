-module(lexer_driver).
-export([tokenize/2, format_error/2]).

tokenize(Stream, String) ->
    case lexer:string(String) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        Error ->
            Message = format_error(Stream, Error),
            throw({lexer_exception, Message})
    end.

format_error(Stream, {error, {Line, _Module, {Message, Fault}}, _}) ->
    io_lib:format(
        '~s:~w: lexical error, ~s: ~s~n',
        [Stream, Line, Message, Fault]).
