-module(codegen_env).
-export([
    new/3,
    lookup/2]).

new(_Lines, Formals, Locals) ->
    SymTab = dict:new(),
    SpOffset = 0,
    FpOffset = 0,
    env(SymTab, Formals, Locals, SpOffset, FpOffset).

env(SymTab, Formals, Locals, SpOffset, FpOffset) ->
    {SymTab, Formals, Locals, SpOffset, FpOffset}.

lookup(Temp, Env0={SymTab,_Formals,_Locals,_,_}) ->
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

create_value(Temp, Env0={_SymTab,Formals,_Locals,SpOffset,_FpOffset}) ->
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

set_sp_symbol(Key, Value, {SymTab,Formals,Locals,SpOffset,FpOffset}) ->
    Updated = dict:store(Key, Value, SymTab),
    env(Updated, Formals, Locals, SpOffset+4, FpOffset).
