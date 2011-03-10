-module(codegen_env).
-export([
    new/3,
    lookup/2,
    get_arrays_size/1]).

new(Formals, Locals, ArraysSize) ->
    SymTab = dict:new(),
    SpOffset = 0,
    env(SymTab, Formals, Locals, SpOffset, ArraysSize).

get_arrays_size({_,_,_,_,ArraysSize}) ->
    ArraysSize.

env(SymTab, Formals, Locals, SpOffset, ArraysSize) ->
    {SymTab, Formals, Locals, SpOffset, ArraysSize}.

lookup(Temp, Env0={SymTab,_,_,_,_}) ->
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

create_value(Temp, Env0={_,Formals,_,SpOffset,_}) ->
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

set_sp_symbol(Key, Value, {SymTab,Formals,Locals,SpOffset,ArraysSize}) ->
    UpdatedSymTab = dict:store(Key, Value, SymTab),
    env(UpdatedSymTab, Formals, Locals, SpOffset+4, ArraysSize).
