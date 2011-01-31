-module(lexer_driver).
-export([tokenize/2]).

tokenize(File, String) ->
    case lexer:string(String) of
        {ok, Tokens, _} -> Tokens;
        Error           -> handle_error(File, Error)
    end.

handle_error(File, {error, {Line, _Module, {Message, Fault}}, _}) ->
    tool_chain:die(
        '~s:~p: lexical error, ~p: ~p~n',
        [File, Line, Message, Fault]);
handle_error(_File, Unknown) ->
    tool_chain:die(Unknown).
