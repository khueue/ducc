-module(io_tools).
-export([
    string_from_input/0,
    string_from_file/1,
    string_from_stream/1,
    term_from_string/1,
    term_to_output/1,
    die/1,
    die/2]).

string_from_input() ->
    string_from_stream(standard_io).

string_from_file(File) ->
    {ok, Stream} = file:open(File, [read]),
    String = string_from_stream(Stream),
    file:close(Stream),
    String.

string_from_stream(Stream) ->
    case io:get_chars(Stream, _Prompt = '', 8192) of
        eof ->
            [];
        Text ->
            Text ++ string_from_stream(Stream)
    end.

% Ugly:
term_from_string(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    Term;
                _Error ->
                    {error, invalid_term}
            end;
        _Error ->
            {error, invalid_term}
    end.

term_to_output(Term) ->
    io:fwrite('~p.~n', [Term]).

die(Message) ->
    die(Message, []).

die(Format, Args) ->
    io:fwrite(Format, Args),
    erlang:halt(1).
