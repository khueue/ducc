-module(parser_driver).
-export([parse/2]).

parse(Stream, Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} ->
            {ok, wrap(Stream, ParseTree)};
        Error ->
            Message = format_error(Stream, Error),
            throw({parser_exception, Message})
    end.

format_error(Stream, {error, {Line, _Module, [_Message,Fault|_]}}) ->
    io_lib:format(
        '~s:~p: syntax error: ~s~n',
        [Stream, Line, Fault]).

wrap(Stream, []) ->
    {{0,program}, Stream, []};
wrap(Stream, Topdecs = [Topdec|_]) ->
    {Line, _} = erlang:element(1, Topdec),
    {{Line,program}, Stream, Topdecs}.
