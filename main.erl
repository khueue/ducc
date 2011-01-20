-module(main).
-export([start/1]).

start(FileName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    process(FileDescriptor),
    file:close(FileDescriptor),
    erlang:halt().

process(FileDescriptor) ->
    case io:request(FileDescriptor, {get_until, prompt, lexer, token, [1]}) of
        {ok, {Type, _, Value}, _} ->
            print_line({Type, Value}),
            process(FileDescriptor);
        {eof, _} ->
            ok;
        ErrorInfo ->
            print_line({lexical_error, ErrorInfo}),
            erlang:halt()
    end.

print_line(Stuff) ->
    io:write(Stuff),
    io:nl().
