-module(translator_helpers).
-export([
    get_tag/1,
    get_line/1,
    ducc_byte_size/1,
    arg_list/1,
    combine_instrs/1,
    combine_temps/1,
    get_return_temp/1,
    type_size/1,
    round4/1,
    char_to_int/1]).

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

ducc_byte_size(long) -> 4;
ducc_byte_size(byte) -> 1.

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

% xxx move to emitter step.
char_to_int(Value) ->
    case atom_to_list(Value) of
        [$\\,$n] -> 10; % ASCII newline.
        [Int]    -> Int
    end.
