-module(tool_chain).
-export([
    process_term_or_echo/3,
    string_from_input/0,
    string_from_file/1,
    string_from_stream/1,
    term_from_string/1,
    term_to_output/1,
    die/1,
    die/2]).

% Invalid term means that the previous step failed, so just it pass along.
% Should for instance the lexer fail, the piping will still continue:
% $ cat file.c | lexer | parser
process_term_or_echo(_Processor, {error, invalid_term}, String) ->
    tool_chain:die(String);
process_term_or_echo(Processor, Term, _String) ->
    Processor(Term).

string_from_input() ->
    string_from_stream(standard_io).

string_from_file(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            binary_to_list(Binary);
        _Error ->
            die('Error reading file: ~s~n', [File])
    end.

string_from_stream(Stream) ->
    case io:get_chars(Stream, _Prompt = '', _Count = 8192) of
        eof  -> [];
        Text -> Text ++ string_from_stream(Stream)
    end.

term_from_string(String) ->
    to_term(to_tokens(String)).

to_tokens(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _} -> Tokens;
        _Error          -> {error, invalid_term}
    end.

to_term(Tokens) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Term} -> Term;
        _Error     -> {error, invalid_term}
    end.

term_to_output(Term) ->
    io:fwrite('~p.~n', [Term]).

die(Message) ->
    die(Message, []).

die(Format, Args) ->
    io:fwrite(Format, Args),
    erlang:halt(1).
