-module(analyzer_env).
-export([
    new/0,
    enter_scope/2,
    scope_name/1,
    add_symbol/3,
    lookup/3,
    lookup_or_throw/4,
    lookup_first_scope/3,
    print_symtabs/1]).

new() ->
    Env = {[]},
    enter_scope(global, Env).

enter_scope(Scope, _Env = {SymTabs}) ->
    {stack_push({Scope,dict:new()}, SymTabs)}.

scope_name({[{Scope,_}|_]}) ->
    Scope.

lookup_or_throw(Name, Node, Env, Exception) ->
    case lookup(Name, Node, Env) of
        not_found -> throw(Exception);
        FoundNode -> FoundNode
    end.

lookup(_Name, _Node, {[]}) ->
    not_found;
lookup(Name, Node, {[{_Scope,SymTab}|SymTabs]}) ->
    case dict:find(Name, SymTab) of
        {ok, Val} -> Val;
        error     -> lookup(Name, Node, {SymTabs})
    end.

lookup_first_scope(Name, Node, {[SymTab|_]}) ->
    lookup(Name, Node, {[SymTab]}).

add_symbol(Key, Value, _Env = {SymTabs}) ->
    {{Scope,Current}, Rest} = stack_peek(SymTabs),
    Updated = dict:store(Key, Value, Current),
    {stack_push({Scope,Updated}, Rest)}.

stack_push(X, Stack) ->
    [X|Stack].

stack_peek([]) ->
    throw(peek_empty_stack);
stack_peek([Top|Stack]) ->
    {Top, Stack}.

print_symtabs({[{Scope,S}]}) ->
    io:format('~s: ~p~n', [Scope,dict:to_list(S)]);
print_symtabs({[{Scope,S}|Ss]}) ->
    io:format('~s: ~p~n', [Scope,dict:to_list(S)]),
    print_symtabs({Ss}).
