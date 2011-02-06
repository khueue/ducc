-module(analyzer_rules).
-export([check_actuals/3,
         must_be_lval/2,
         must_be_tag_member/3,
         must_not_exist_in_same_scope/3,
         same_formals/2,
         same_return_type/2]).

-define(HELPER, analyzer_helpers).

check_actuals(Function, Funcall, Env) ->
    Formals = erlang:element(4, Function),
    Actuals = erlang:element(3, Funcall),
    same_arity(Formals, Actuals, Funcall),
    ?HELPER:convertible_types(Formals, Actuals, Env).

identical_types([], [], _CurrentNode) -> ok;
identical_types([F1|Formals1], [F2|Formals2], CurrentNode) ->
    same_tag_and_type(F1, F2, CurrentNode),
    identical_types(Formals1, Formals2, CurrentNode).

must_be_lval(Node = {{_,ident},Name}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    case ?HELPER:get_tag(FoundNode) of
        scalardec -> ok;
        _Other    -> throw(?HELPER:exception(Node, 'not an l-value', []))
    end;
must_be_lval({{_,arrelem},_Name,_Index}, _Env) -> ok;
must_be_lval(Node, _Env) ->
    throw(?HELPER:exception(Node, 'not an l-value', [])).

must_be_tag_member(Node, Tags, Exception) ->
    case ?HELPER:tag_member(Node, Tags) of
        true  -> ok;
        false -> throw(Exception)
    end.

must_not_exist_in_same_scope(Name, Node, Env) ->
    case analyzer_env:lookup_current_scope(Name, Node, Env) of
        not_found   -> ok;
        _SymbolInfo -> throw(?HELPER:exception(Node, 'already defined', []))
    end.

same_arity(Formals, FoundFormals, Node) ->
    try same_arity(Formals, FoundFormals)
    catch
        different_arity ->
            throw(?HELPER:exception(Node, 'wrong number of arguments', []))
    end.

same_arity(Formals, FoundFormals) ->
    case erlang:length(Formals) == erlang:length(FoundFormals) of
        true  -> ok;
        false -> throw(different_arity)
    end.

same_formals(Node, FoundNode) ->
    NodeFormals = element(4, Node),
    FoundNodeFormals = element(4, FoundNode),
    same_arity(NodeFormals, FoundNodeFormals, Node),
    identical_types(NodeFormals, FoundNodeFormals, Node).

same_return_type(Node, FoundNode) ->
    case ?HELPER:get_type(Node) == ?HELPER:get_type(FoundNode) of
        true ->
            ok;
        false ->
            throw(?HELPER:exception(Node, 'wrong return type', []))
    end.

% BUG: We are lucky enough to have the same structure for all possible
% formals (scalardec and formal_arraydec), but this simple solution would
% not necessarily work if other types (pointers, fixed size array, ...)
% are introduced.
same_tag_and_type({{_,Tag},Type,_}, {{_,Tag},Type,_}, _CurrentNode) ->
    ok;
same_tag_and_type(_, _, CurrentNode) ->
    throw(?HELPER:exception(CurrentNode, 'parameters must match exactly', [])).
