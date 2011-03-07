-module(translator_helpers).
-export([
    get_tag/1,
    get_line/1,
    size_of/1,
    arg_list/1,
    combine_instrs/1,
    combine_temps/1,
    get_return_temp/1,
    type_size/1,
    round4/1,
    source_line_header/2]).

-define(ENV, translator_env).

get_meta(Node) ->
    erlang:element(1, Node).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

get_line(Node) ->
    {Line, _Tag} = get_meta(Node),
    Line.

type_size(int)  -> long;
type_size(char) -> byte.

size_of(long) -> 4;
size_of(byte) -> 1.

arg_list([]) -> [];
arg_list([{_Env, _Instrs, Temps}|R]) ->
    RetTemp = get_return_temp(Temps),
    [RetTemp|arg_list(R)].

combine_instrs([]) -> [];
combine_instrs([{_Env, Ins, _Temps}|Xs]) ->
    Ins ++ combine_instrs(Xs).

combine_temps([]) -> [];
combine_temps([{_Env, _Ins, Temps}|Xs]) ->
    Temps ++ combine_temps(Xs).

get_return_temp(Temps) ->
    lists:last(Temps).

round4(N) ->
    round4(0, N).

round4(MultipleOfFour, N) ->
    case MultipleOfFour >= N of
        true  -> MultipleOfFour;
        false -> round4(MultipleOfFour+4, N)
    end.

source_line_header(Node, Env) ->
    Tag = get_tag(Node),
    LineNum = get_line(Node),
    SourceLine = case LineNum of
        0 -> "<no correspondence in source>";
        _ -> ?ENV:get_source_line(LineNum, Env)
    end,
    {'- SOURCE -', LineNum, Tag, SourceLine}.
