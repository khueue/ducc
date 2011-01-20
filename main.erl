-module(main).
-export([start/1]).

start(FileName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    loop(FileDescriptor),
    file:close(FileDescriptor),
    init:stop().

loop(FileDescriptor) ->
    case io:request(FileDescriptor, {get_until, prompt, yamucc_lexer, token, [1]}) of
        {ok, Token = {Type, _, Value}, _} ->
            io:write({Type, Value}),
            io:nl(),
            loop(FileDescriptor);
        {eof, _} ->
            ok;
        %{error, _ErrorMessage = {_, _, Msg}, _} ->
        %    io:write({lexer_error, Msg}),
        %    io:nl(),
        %    init:stop();
        ErrorInfo ->
            io:write({lexer_error, ErrorInfo}),
            io:nl(),
            halt()
    end.
