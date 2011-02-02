-module(analyzer_env).
-export([new/0, enter_scope/1, add_symbol/3]).

new() ->
    Env = {[]},
    enter_scope(Env).

enter_scope(_Env = {SymTabs}) ->
    NewSymTab = dict:new(),
    {stack_push(NewSymTab, SymTabs)}.

add_symbol(Key, Value, _Env = {SymTabs}) ->
    {Current, Rest} = stack_peek(SymTabs),
    Updated = dict:store(Key, Value, Current),
    {stack_push(Updated, Rest)}.

stack_push(X, Stack) ->
    [X|Stack].

stack_peek([]) ->
    throw(peek_empty_stack);
stack_peek([Top|Stack]) ->
    {Top, Stack}.
