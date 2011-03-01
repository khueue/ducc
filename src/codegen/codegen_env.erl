-module(codegen_env).
-export([
    new/1,
    get_source_line/2,
    set_symbol/3,
    lookup/3,
    lookup_or_throw/4]).

new(Lines) ->
    SymTabs = [],
    LineDict = line_dict(Lines),
    env(SymTabs, LineDict).

line_dict(Lines) ->
    Dict = dict:new(),
    insert_lines(Lines, Dict).

insert_lines([], Dict) ->
    Dict;
insert_lines([{Num,Line}|Lines], Dict0) ->
    Dict1 = dict:store(Num, Line, Dict0),
    insert_lines(Lines, Dict1).

get_source_line(LineNum, {_S,Dict}) ->
    {ok, Line} = dict:find(LineNum, Dict),
    Line.

lookup_or_throw(Name, Node, Env, Exception) ->
    case lookup(Name, Node, Env) of
        not_found -> throw(Exception);
        FoundNode -> FoundNode
    end.

lookup(_Name, _Node, {[],_LineDict}) ->
    not_found;
lookup(Name, Node, {[{_Scope,SymTab}|SymTabs],LineDict}) ->
    case dict:find(Name, SymTab) of
        {ok, Val} -> Val;
        error     -> lookup(Name, Node, {SymTabs,LineDict})
    end.

set_symbol(Key, Value, {SymTabs,LineDict}) ->
    {{Scope,Current}, Rest} = stack_peek(SymTabs),
    Updated = dict:store(Key, Value, Current),
    env(stack_push({Scope,Updated}, Rest), LineDict).

stack_push(X, Stack) ->
    [X|Stack].

stack_peek([]) ->
    throw(peek_empty_stack);
stack_peek([Top|Stack]) ->
    {Top, Stack}.

env(SymTabs, LineDict) ->
    {SymTabs, LineDict}.
