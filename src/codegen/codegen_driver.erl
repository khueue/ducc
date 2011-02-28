-module(codegen_driver).
-export([generate_code/3]).

generate_code(Stream, RtlCode, Lines) ->
    AsmCode = try codegen:generate_code(RtlCode, Lines)
    catch
        {codegen_exception, {Line, Message}} ->
            Message1 = format_error(Stream, Line, Message),
            throw({codegen_exception, Message1})
    end,
    {ok, AsmCode}.

format_error(Stream, Line, Message) ->
    io_lib:format('~s:~p: codegen error, ~s~n', [Stream, Line, Message]).
