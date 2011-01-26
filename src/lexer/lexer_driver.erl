-module(lexer_driver).
-export([start/0]).

start() ->
    %Tokens = process_file(standard_io),
    String = getmamma(),
    {ok, Tokens, _} = lexer:string(String),
    term_io:term_to_stdout(Tokens),
    init:stop().

getmamma() ->
    case io:get_chars('', 8192) of
        eof ->
            [];
        Text ->
            Text ++ getmamma()
    end.

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
