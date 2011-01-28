-module(parser_driver).
-export([parse/1]).

parse(Tokens) ->
    case parser:parse(Tokens) of
        {ok, ParseTree} -> ParseTree;
        Error           -> handle_error(Error)
    end.

handle_error({error, {Line, _Module, [_Message,Fault|_]}}) ->
    tool_chain:die(
        'Syntax error on line ~p, before: ~p~n',
        [Line, Fault]);
handle_error(Unknown) ->
    tool_chain:die(Unknown).
