-module(translator_driver).
-export([translate/3]).

translate(Stream, ParseTree, Lines) ->
    RtlCode = try translator:translate(ParseTree, Lines)
    catch
        {translator_exception, {Line, Message}} ->
            Message1 = format_error(Stream, Line, Message),
            throw({translator_exception, Message1})
    end,
    {ok, RtlCode}.

format_error(Stream, Line, Message) ->
    io_lib:format('~s:~p: translation error, ~s~n', [Stream, Line, Message]).
