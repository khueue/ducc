-module(translator_driver).
-export([translate/2]).

translate(Stream, ParseTree) ->
    Instructions = try translator:translate(ParseTree)
    catch
        {translator_exception, {Line, Message}} ->
            Message1 = format_error(Stream, Line, Message),
            throw({translator_exception, Message1})
    end,
    {ok, Instructions}.

format_error(Stream, Line, Message) ->
    io_lib:format('~s:~p: translation error, ~s~n', [Stream, Line, Message]).
