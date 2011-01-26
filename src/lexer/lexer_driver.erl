-module(lexer_driver).
-export([start/0]).

start() ->
    String = stdin_to_string(),
    Tokens = tokenize(String),
    term_io:term_to_stdout(Tokens),
    init:stop().

stdin_to_string() ->
    case io:get_chars('', 8192) of
        eof ->
            [];
        Text ->
            Text ++ stdin_to_string()
    end.

tokenize(String) ->
    case lexer:string(String) of
        {ok, Tokens, _} ->
            Tokens;
        Error ->
            die(Error)
    end.

die(Message) ->
    io:format('~p~n', [{lexical_error, Message}]),
    erlang:halt().
