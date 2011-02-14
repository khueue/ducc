-module(translator_env).
-export([
    new/0,
    enter_scope/2,
    scope_name/1,
    set_symbol/3,
    lookup/3,
    lookup_or_throw/4,
    scope/1,
    get_new_label/1,
    get_new_temp/1,
    get_current_label/1,
    get_current_temp/1]).

scope(Env) ->
    case scope_name(Env) of
        global -> global;
        _Fun   -> local
    end.

get_frame_size({_T, _L, FS, _SymTabs}) ->
    FS.

increment_frame_size({T, L, FS, SymTabs}, Bytes) ->
    env(T, L, FS+Bytes, SymTabs).

new() ->
    FrameSize = 0,
    LastUsedTemp = 1,
    LastUsedLabel = 99,
    SymTabs = [],
    Env = {LastUsedTemp, LastUsedLabel, FrameSize, SymTabs},
    enter_scope(global, Env).

enter_scope(Scope, _Env = {T,L,FS,SymTabs}) ->
    {T, L, FS, stack_push({Scope,dict:new()}, SymTabs)}.

scope_name({_T, _L, _FS, [{Scope,_}|_]}) ->
    Scope.

lookup_or_throw(Name, Node, Env, Exception) ->
    case lookup(Name, Node, Env) of
        not_found -> throw(Exception);
        FoundNode -> FoundNode
    end.

lookup_current_scope(Name, Node, {T,L,FS,[SymTab|_]}) ->
    lookup(Name, Node, {T,L,FS,[SymTab]}).

lookup(_Name, _Node, {_T,_L,_FS,[]}) ->
    not_found;
lookup(Name, Node, {T,L,FS,[{_Scope,SymTab}|SymTabs]}) ->
    case dict:find(Name, SymTab) of
        {ok, Val} -> Val;
        error     -> lookup(Name, Node, {T,L,FS,SymTabs})
    end.

set_symbol(Key, Value, {T,L,FS,SymTabs}) ->
    {{Scope,Current}, Rest} = stack_peek(SymTabs),
    Updated = dict:store(Key, Value, Current),
    {T,L,FS,stack_push({Scope,Updated}, Rest)}.

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

env(TempId, LabelId, FS, SymTabs) ->
    {TempId, LabelId, FS, SymTabs}.

get_new_label({LastTempId, LastLabelId, FS, SymTabs}) ->
    NewLabelId = LastLabelId + 1,
    {env(LastTempId,NewLabelId,FS,SymTabs), label(NewLabelId)}.

get_new_temp({LastTempId, LastLabelId, FS, SymTabs}) ->
    NewTempId = LastTempId + 1,
    {env(NewTempId,LastLabelId,FS,SymTabs), temp(NewTempId)}.

get_current_temp({LastTempId, _LastLabelId, _FS, _SymTabs}) ->
    temp(LastTempId).

get_current_label({_LastTempId, LastLabelId, _FS, _SymTabs}) ->
    label(LastLabelId).
