-module(translator_driver).
-export([translate/2]).

translate(Stream, ParseTree) ->
    Result = (catch translator:translate(ParseTree)),
    case Result of
        ParseTree ->
            {ok, ParseTree};
        Error ->
            Message = format_error(Stream, Error),
            throw({translator_exception, Message})
    end.

format_error(Stream, {translator_exception, {Line, Message}}) ->
    io_lib:format(
        '~s:~p: translation error, ~s~n',
        [Stream, Line, Message]);
format_error(Stream, UnknownError) ->
    io_lib:format(
        '~s: unknown translation error, ~n~p~n',
        [Stream, UnknownError]).
