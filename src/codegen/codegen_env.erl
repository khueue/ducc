-module(codegen_env).
-export([
    new/3,
    lookup/2,
    get_arrays_size/1]).

new(Formals, Locals, ArraysSize) ->
    SymTab = dict:new(),
    SpOffset = 0,
    FpOffset = 0,
    env(SymTab, Formals, Locals, SpOffset, FpOffset, ArraysSize).

get_arrays_size({_,_,_,_,_,ArraysSize}) ->
    ArraysSize.

env(SymTab, Formals, Locals, SpOffset, FpOffset, ArraysSize) ->
    {SymTab, Formals, Locals, SpOffset, FpOffset, ArraysSize}.

lookup(Temp, Env0={SymTab,_Formals,_Locals,_,_,_}) ->
    case dict:find(Temp, SymTab) of
        {ok, Value} ->
            {Value,Env0};
        error ->
            create_value(Temp, Env0)
    end.

list_pos(X, [X|_]) ->
    0;
list_pos(X, [_|L]) ->
    1 + list_pos(X, L).

create_value(Temp, Env0={_SymTab,Formals,_Locals,SpOffset,_FpOffset,_}) ->
    case lists:member(Temp, Formals) of
        true  ->
            Pos = list_pos(Temp, Formals),
            Value = {fp, Pos*4},
            {Value,Env0};
        false ->
            Value = {sp, SpOffset},
            Env1 = set_sp_symbol(Temp, Value, Env0),
            {Value,Env1}
    end.

set_sp_symbol(Key, Value, {SymTab,Formals,Locals,SpOffset,FpOffset,ArraysSize}) ->
    Updated = dict:store(Key, Value, SymTab),
    env(Updated, Formals, Locals, SpOffset+4, FpOffset, ArraysSize).
