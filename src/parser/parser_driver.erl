-module(parser_driver).
-export([parse/2]).

parse(Stream, Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} -> fix(Stream, ParseTree);
        Error           -> handle_error(Stream, Error)
    end.

handle_error(Stream, {error, {Line, _Module, [_Message,Fault|_]}}) ->
    tool_chain:die(
        '~s:~p: syntax error: ~p~n',
        [Stream, Line, Fault]);
handle_error(_Stream, Unknown) ->
    tool_chain:die(Unknown).

fix(Stream, Topdecs) ->
    [{{Line,_},_}|_] = Topdecs,
    {{Line,Stream},{Topdecs}}.
