-module(translator_env).
-export([
    new/0,
    enter_scope/2,
    leave_scope/1,
    scope_name/1,
    set_symbol/3,
    lookup/3,
    lookup_or_throw/4,
    scope/1,
    get_rv/0,
    get_fp/0,
    get_new_labels/2,
    get_new_label/1,
    get_new_temps/2,
    get_new_temp/1,
    get_current_label/1,
    get_current_temp/1,
    get_frame_size/1,
    increment_frame_size/2,
    set_function_labels/3,
    get_function_labels/1]).

scope(Env) ->
    case scope_name(Env) of
        global -> global;
        _Fun   -> local
    end.

get_frame_size({_T,_L,{_,_,FS},_SymTabs}) ->
    FS.

increment_frame_size({T,L,{Start,Stop,FS},SymTabs}, Bytes) ->
    NewFS = FS + Bytes,
    env(T, L, {Start,Stop,NewFS}, SymTabs).

new() ->
    LastUsedTemp = get_fp(),
    LastUsedLabel = label(99),
    StartLabel = nil,
    StopLabel = nil,
    FrameSize = 0,
    Function = {StartLabel, StopLabel, FrameSize},
    SymTabs = [],
    Env = env(LastUsedTemp, LastUsedLabel, Function, SymTabs),
    enter_scope(global, Env).

enter_scope(Scope, {T,L,_F,SymTabs}) ->
    StartLabel = nil,
    StopLabel = nil,
    FrameSize = 0,
    Function = {StartLabel, StopLabel, FrameSize},
    env(T, L, Function, stack_push({Scope,dict:new()}, SymTabs)).

leave_scope({T,L,_F,[_SymTab|SymTabs]}) ->
    StartLabel = nil,
    StopLabel = nil,
    FrameSize = 0,
    Function = {StartLabel, StopLabel, FrameSize},
    env(T, L, Function, SymTabs).

scope_name({_T,_L,_F,[{Scope,_}|_]}) ->
    Scope.

lookup_or_throw(Name, Node, Env, Exception) ->
    case lookup(Name, Node, Env) of
        not_found -> throw(Exception);
        FoundNode -> FoundNode
    end.

lookup(_Name, _Node, {_T,_L,_F,[]}) ->
    not_found;
lookup(Name, Node, {T,L,F,[{_Scope,SymTab}|SymTabs]}) ->
    case dict:find(Name, SymTab) of
        {ok, Val} -> Val;
        error     -> lookup(Name, Node, {T,L,F,SymTabs})
    end.

set_symbol(Key, Value, {T,L,F,SymTabs}) ->
    {{Scope,Current}, Rest} = stack_peek(SymTabs),
    Updated = dict:store(Key, Value, Current),
    env(T,L,F,stack_push({Scope,Updated}, Rest)).

set_function_labels({T,L,{_Start,_Stop,FS},Symtabs}, NewStartLabel, NewStopLabel) ->
    env(T, L, {NewStartLabel,NewStopLabel,FS}, Symtabs).

get_function_labels({_T,_L,{StartLabel,StopLabel,_FS},_Symtabs}) ->
    {StartLabel, StopLabel}.

stack_push(X, Stack) ->
    [X|Stack].

stack_peek([]) ->
    throw(peek_empty_stack);
stack_peek([Top|Stack]) ->
    {Top, Stack}.

label(Id) ->
    {label, Id}.

temp(Id) ->
    {temp, Id}.

env(Temp, Label, F, SymTabs) ->
    {Temp, Label, F, SymTabs}.

get_rv() ->
    temp(0).

get_fp() ->
    temp(1).

get_new_labels(0, Env) ->
    {Env, []};
get_new_labels(N, Env0) ->
    {Env1, Label} = get_new_label(Env0),
    {Env2, Labels} = get_new_labels(N-1, Env1),
    {Env2, [Label|Labels]}.

get_new_label({LastTemp, {label, LastLabelId}, F, SymTabs}) ->
    NewLabel = label(LastLabelId + 1),
    {env(LastTemp,NewLabel,F,SymTabs), NewLabel}.

get_new_temps(0, Env) ->
    {Env, []};
get_new_temps(N, Env0) ->
    {Env1, Temp} = get_new_temp(Env0),
    {Env2, Temps} = get_new_temps(N-1, Env1),
    {Env2, [Temp|Temps]}.

get_new_temp({{temp, LastTempId}, LastLabel, F, SymTabs}) ->
    NewTemp = temp(LastTempId + 1),
    {env(NewTemp,LastLabel,F,SymTabs), NewTemp}.

get_current_temp({LastTemp, _LastLabel, _F, _SymTabs}) ->
    LastTemp.

get_current_label({_LastTemp, LastLabel, _F, _SymTabs}) ->
    LastLabel.
