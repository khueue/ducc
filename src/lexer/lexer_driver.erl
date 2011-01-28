-module(lexer_driver).
-export([tokenize/1]).

tokenize(String) ->
    case lexer:string(String) of
        {ok, Tokens, _} -> Tokens;
        Error           -> handle_error(Error)
    end.

handle_error({error, {Line, _Module, {Message, Fault}}, _}) ->
    tool_chain:die(
        'Lexical error on line ~p, ~p: ~p~n',
        [Line, Message, Fault]);
handle_error(Unknown) ->
    tool_chain:die(Unknown).
