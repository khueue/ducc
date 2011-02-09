-module(analyzer_driver).
-export([analyze/2]).

analyze(Stream, ParseTree) ->
    try analyzer:analyze(ParseTree)
    catch
        {analyzer_exception, {Line, Message}} ->
            Message1 = format_error(Stream, Line, Message),
            throw({analyzer_exception, Message1})
    end,
    {ok, ParseTree}.

format_error(Stream, Line, Message) ->
    io_lib:format('~s:~p: semantic error, ~s~n', [Stream, Line, Message]).
