-module(parser_driver).
-export([parse/1]).

parse(Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} ->
            ParseTree;
        Error ->
            handle_error(Error)
    end.

handle_error({error, {Line, _Module, [_Message,Fault|_]}}) ->
    io_tools:die(
        'Syntax error on line ~p, before: ~p~n',
        [Line, Fault]);
handle_error(Unknown) ->
    io_tools:die(Unknown).
