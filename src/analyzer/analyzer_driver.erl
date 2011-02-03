-module(analyzer_driver).
-export([analyze/2]).

analyze(Stream, ParseTree) ->
    Result = (catch analyzer:analyze(ParseTree)),
    case Result of
        ok ->
            ok;
        Error ->
            Message = format_error(Stream, Error),
            throw({analyzer_exception, Message})
    end.

format_error(Stream, {analyzer_exception, {Line, Message}}) ->
    io_lib:format(
        '~s:~p: semantic error, ~s~n',
        [Stream, Line, Message]);
format_error(Stream, UnknownError) ->
    io_lib:format(
        '~s: unknown semantic error, ~n~p~n',
        [Stream, UnknownError]).
