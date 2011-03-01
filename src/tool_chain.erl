-module(tool_chain).
-export([
    read_lines_from_file/1,
    read_lines_from_stream/1,
    string_from_input/0,
    string_from_lines/1,
    string_from_file/1,
    string_from_stream/1,
    term_from_string/1,
    raw_string_to_output/1,
    term_to_output/1,
    die/1,
    die/2]).

read_lines_from_file(File) ->
    case file:open(File, [read]) of
        {ok, Stream} ->
            Lines = read_lines_from_stream(Stream),
            file:close(Stream),
            Lines;
        _Error ->
            Message = io_lib:format('Error reading file: ~s~n', [File]),
            throw({file_exception, Message})
    end.

read_lines_from_stream(Stream) ->
    read_lines_from_stream(Stream, 1).

read_lines_from_stream(Stream, LineNum) ->
    case file:read_line(Stream) of
        eof ->
            [];
        {ok, LineWithNewline} ->
            Line = string:strip(LineWithNewline, right, $\n),
            [{LineNum, Line} | read_lines_from_stream(Stream, LineNum+1)];
        {error, Reason} ->
            io:format("~p~n", [Reason])
    end.

string_from_lines([]) -> [];
string_from_lines([{_Num,Line}|Lines]) ->
    Line ++ "\n" ++
    string_from_lines(Lines).

string_from_input() ->
    string_from_stream(standard_io).

string_from_file(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            {ok, binary_to_list(Binary)};
        _Error ->
            Message = io_lib:format('Error reading file: ~s~n', [File]),
            throw({file_exception, Message})
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
        {ok, Tokens, _} ->
            Tokens;
        _Error ->
            throw({tool_chain_exception, invalid_term})
    end.

to_term(Tokens) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Term} ->
            Term;
        _Error ->
            throw({tool_chain_exception, invalid_term})
    end.

raw_string_to_output(Str) ->
    io:fwrite('~s~n', [Str]).

term_to_output(Term) ->
    io:fwrite('~100p.~n', [Term]).

die(Message) ->
    die(Message, []).

die(Format, Args) ->
    io:fwrite(Format, Args),
    erlang:halt(1).
