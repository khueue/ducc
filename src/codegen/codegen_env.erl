-module(codegen_env).
-export([
    new/1,
    set_symbol/3,
    lookup/2]).

new(_Lines) ->
    SymTab = dict:new(),
    SpOffset = 0,
    env(SymTab, SpOffset).

env(SymTab, SpOffset) ->
    {SymTab, SpOffset}.

lookup(Temp, Env0={SymTab,SpOffset}) ->
    case dict:find(Temp, SymTab) of
        {ok, Value} ->
            {Value,Env0};
        error ->
            Value = {sp,SpOffset},
            Env1 = set_symbol(Temp, Value, Env0),
            {Value,Env1}
    end.

set_symbol(Key, Value, {SymTab,SpOffset}) ->
    Updated = dict:store(Key, Value, SymTab),
    env(Updated, SpOffset+4).
