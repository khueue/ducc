-module(main).
-export([start/1]).

start(FileName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    process(FileDescriptor),
    file:close(FileDescriptor),
    init:stop().

process(FileDescriptor) ->
    case io:request(FileDescriptor, {get_until, prompt, lexer, token, [1]}) of
        {ok, {Type, _, Value}, _} ->
            print_line({Type, Value}),
            process(FileDescriptor);
        {eof, _} ->
            ok;
        %{error, _ErrorMessage = {_, _, Msg}, _} ->
        %    print_line({lexer_error, Msg}),
        %    halt();
        ErrorInfo ->
            print_line({lexer_error, ErrorInfo}),
            halt()
    end.

print_line(Stuff) ->
    io:write(Stuff),
    io:nl().
