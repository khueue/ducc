-module(main).
-export([start/1]).

start(FileName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    TokenList = process_file(FileDescriptor, []),
    io:format("~p.~n", [TokenList]),
    {_, ParseTree} = parser:parse(TokenList),
    io:format("~12p.~n", [ParseTree]),
    file:close(FileDescriptor),

    erlang:halt().

process_file(FileDescriptor, Acc) ->
    case io:request(FileDescriptor, {get_until, prompt, lexer, token, [1]}) of
        {ok, Token, _} ->
            %io:format('~p~n', [{Type, Value}]),
            process_file(FileDescriptor, Acc ++ [Token]);
        {eof, _} ->
            Acc;
        ErrorInfo ->
            io:format('~p~n', [{lexical_error, ErrorInfo}]),
            erlang:halt()
    end.
