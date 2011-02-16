-module(translator_helpers).
-export([get_tag/1,
         assign_scalar_location/1,
         create_scalar_data/2,
         assign_array_location/1,
         create_array_data/4,
         ducc_byte_size/1,
         arg_list/1,
         conc_instrs/1,
         get_return_temp/1]).

-define(ENV, translator_env).

get_meta(Node) ->
    erlang:element(1, Node).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

assign_scalar_location(Env0) ->
    case ?ENV:scope(Env0) of
        global ->
            {Env1, Label} = ?ENV:get_new_label(Env0),
            {Env1, {global, Label}};
        local ->
            {Env1, Temp} = ?ENV:get_new_temp(Env0),
            {Env1, {local, Temp}}
    end.

create_scalar_data({global, {label, _}}, Type) -> {type_size(Type)};
create_scalar_data({local, {temp, _}}, Type)   -> {type_size(Type)}.

assign_array_location(Env0) ->
    case ?ENV:scope(Env0) of
        global ->
            {Env1, Label} = ?ENV:get_new_label(Env0),
            {Env1, {global, Label}};
        local ->
            {Env0, {local, stack}}
    end.

create_array_data(_Env0, {global, {label, _}}, Type, Count) ->
    {type_size(Type), Count};
create_array_data(Env0, {local, stack}, Type, Count) ->
    Offset = ?ENV:get_frame_size(Env0),
    {type_size(Type), Count, Offset}.

type_size(int)  -> long;
type_size(char) -> byte.

ducc_byte_size(long) -> 4;
ducc_byte_size(byte) -> 1.

arg_list([]) -> [];
arg_list([{_Env, _Instrs, Temps}|R]) ->
    RetTemp = get_return_temp(Temps),
    [RetTemp|arg_list(R)].

conc_instrs([]) -> [];
conc_instrs([{_Env, Instrs, _Temps}|R]) ->
    Instrs ++ conc_instrs(R).

get_return_temp(Temps) ->
    lists:last(Temps).
