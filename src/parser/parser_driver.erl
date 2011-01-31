-module(parser_driver).
-export([parse/2]).

parse(Stream, Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} -> {ok, fix(Stream, ParseTree)};
        Error           -> handle_error(Stream, Error)
    end.

fix(Stream, []) ->
    {{0,Stream},{[]}};
fix(Stream, Topdecs) ->
    [{{Line,_},_}|_] = Topdecs,
    {{Line,Stream},{Topdecs}}.

handle_error(Stream, {error, {Line, _Module, [_Message,Fault|_]}}) ->
    io:format('~s:~p: syntax error: ~p~n', [Stream, Line, Fault]),
    {error, parser};
handle_error(_Stream, Unknown) ->
    io:write(Unknown), io:nl(),
    {error, parser}.
