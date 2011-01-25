-module(main).
-export([start/1]).

start(FileName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    TokenList = process_file(FileDescriptor),
    io:write(TokenList),
    io:nl(),
    {_, ParseTree} = parser:parse(TokenList),
    io:write(ParseTree),
    io:nl(),
    file:close(FileDescriptor),
    erlang:halt().

process_file(Stream) ->
    case next_token(Stream) of
        {ok, Token, _} ->
            [Token | process_file(Stream)];
        {eof, _} ->
            [];
        ErrorInfo ->
            die(ErrorInfo)
    end.

next_token(Stream) ->
    Options = {get_until, '', lexer, token, [1]},
    io:request(Stream, Options).

die(Message) ->
    io:format('~p~n', [{lexical_error, Message}]),
    erlang:halt().
