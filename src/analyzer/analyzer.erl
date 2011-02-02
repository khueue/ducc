-module(analyzer).
-export([analyze/1]).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

get_line(Node) ->
    {Line, _Tag} = get_meta(Node),
    Line.

get_meta(Node) ->
    erlang:element(1, Node).

analyze(ParseTree) ->
    SymTab = dict:new(),
    Env = {[SymTab]},
    try analyze(ParseTree, Env)
    catch
        Details = {_Line, _Message} ->
            throw({analyzer_exception, Details})
    end,
    ok.

analyze([], Env) ->
    Env;
analyze([X|Xs], Env) ->
    Env1 = analyze(X, Env),
    Env2 = analyze(Xs, Env1),
    Env2;
analyze(Node, Env) when erlang:is_tuple(Node) ->
    Tag = get_tag(Node),
    Env1 = analyze_node(Tag, Node, Env),
    Env1;
analyze(Unhandled, Env) ->
    io:format('Unhandled: ~p~n', [Unhandled]),
    Env.

analyze_node(program, Node, Env)         -> analyze_program(Node, Env);
analyze_node(scalardec, Node, Env)       -> analyze_scalardec(Node, Env);
analyze_node(arraydec, Node, Env)        -> analyze_arraydec(Node, Env);
analyze_node(fundec, Node, Env)          -> analyze_fundec(Node, Env);
analyze_node(fundef, Node, Env)          -> analyze_fundef(Node, Env);
analyze_node(formal_arraydec, Node, Env) -> analyze_formal_arraydec(Node, Env);
analyze_node('if', Node, Env)            -> analyze_if(Node, Env);
analyze_node(while, Node, Env)           -> analyze_while(Node, Env);
analyze_node(return, Node, Env)          -> analyze_return(Node, Env);
analyze_node(funcall, Node, Env)         -> analyze_funcall(Node, Env);
analyze_node(arrelem, Node, Env)         -> analyze_arrelem(Node, Env);
analyze_node(binop, Node, Env)           -> analyze_binop(Node, Env);
analyze_node(ident, Node, Env)           -> analyze_ident(Node, Env);
analyze_node(intconst, Node, Env)        -> analyze_intconst(Node, Env);
analyze_node(charconst, Node, Env)       -> analyze_charconst(Node, Env);
analyze_node(unop, Node, Env)            -> analyze_unop(Node, Env);
analyze_node(Tag, _, Env) ->
    io:format('Unhandled tag: ~p~n', [Tag]),
    Env.

push_symtab(SymTabNew, {SymTabs}) ->
    {[SymTabNew|SymTabs]}.

peek_symtab({[]}) ->
    throw(oooooooooooooooooooj);
peek_symtab({[SymTab|SymTabs]}) ->
    {SymTab, SymTabs}.

enter_scope(Env) ->
    NewScope = dict:new(),
    push_symtab(NewScope, Env).

process(X) ->
    io:format('~p~n', [X]).

add_symbol(Key, Value, {SymTabs}) ->
    {CurrentSymTab, Rest} = peek_symtab({SymTabs}),
    CurrentSymTab1 = dict:store(Key, Value, CurrentSymTab),
    {[CurrentSymTab1|Rest]}.

analyze_program({Meta, _File, Topdecs}, Env) ->
    process(Meta),
    Env1 = analyze(Topdecs, Env),
    Env1.

analyze_scalardec(Node = {Meta, _Type, Name}, Env0) ->
    process(Meta),
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = add_symbol(Name, Node, Env0),
    Env1.

must_not_exist_in_same_scope(Name, Node, Env) ->
    case lookup_first_scope(Name, Node, Env) of
        not_found ->
            ok;
        _SymbolInfo ->
            throw({get_line(Node), 'already defined'})
    end.

analyze_arraydec(Node = {Meta, _Type, Name, Size}, Env0) ->
    process(Meta),
    {CurrentSymTab, _Rest} = peek_symtab(Env0),
    Defined = dict:is_key(Name, CurrentSymTab),
    Env1 = case Defined of
        true  -> throw({get_line(Node), 'already defined'});
        false -> Env0
    end,
    Env2 = case erlang:is_integer(Size) andalso Size >= 0 of
        true  -> add_symbol(Name, Node, Env1);
        false -> throw({get_line(Node), 'invalid size'})
    end,
    Env2.

analyze_fundec(Node = {Meta, _Type, Name, Formals}, Env0) ->
    process(Meta),
    function_must_not_exist(Name, Node, Env0),
    Env1 = add_symbol(Name, Node, Env0),
    Env2 = enter_scope(Env1),
    _Env3 = analyze(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    process(Meta),
    Redefinition = {get_line(Node), 'already defined'},
    Conflicts    = {get_line(Node), 'conflicting types'},

    Env1 = case lookup(Name, Node, Env0) of
        not_found ->
            add_symbol(Name, Node, Env0);
        Val ->
            case get_tag(Val) of
                fundec ->
                    case check_formals(Node, Val) of
                        true ->
                            add_symbol(Name, Node, Env0);
                        false ->
                            throw(Conflicts)
                    end;
                _Other ->
                    throw(Redefinition)
            end
    end,

    Env2 = enter_scope(Env1),
    Env3 = analyze(Formals, Env2),
    Env4 = analyze(Locals, Env3),
    _Env5 = analyze(Stmts, Env4),
    Env1. % Updates to the environment are local to the function!

check_formals(Node1, Node2) ->
    Node1Formals = element(4, Node1),
    Node2Formals = element(4, Node2),
    io:format('~p~n', [Node1Formals]),
    io:format('~p~n', [Node2Formals]),
    same_arity(Node1Formals, Node2Formals) andalso
    identical_types(Node1Formals, Node2Formals).

same_arity(Formals1, Formals2) ->
    erlang:length(Formals1) =:= erlang:length(Formals2).

identical_types([], []) -> true;
identical_types([F1|Formals1], [F2|Formals2]) ->
    same_tag_and_type(F1, F2) andalso
    identical_types(Formals1, Formals2).

% BUG: We are lucky enough to have the same structure for all possible
% formals (scalardec and formal_arraydec), but this simple solution would
% not necessarily work if other types (pointers, fixed size array, ...)
% are introduced.
same_tag_and_type({{_,Tag},Type,_}, {{_,Tag},Type,_}) -> true;
same_tag_and_type(_, _) -> false.

analyze_formal_arraydec({Meta, _Type, _Name}, Env) ->
    process(Meta),
    Env.

analyze_if({Meta, Cond, Then, Else}, Env) ->
    process(Meta),
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Then, Env1),
    Env3 = analyze(Else, Env2),
    Env3.

analyze_while({Meta, Cond, Stmt}, Env) ->
    process(Meta),
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Stmt, Env1),
    Env2.

analyze_return({Meta, Expr}, Env) ->
    process(Meta),
    Env1 = analyze(Expr, Env),
    Env1.

analyze_funcall(Node = {Meta, Name, Actuals}, Env0) ->
    Env1 = analyze(Actuals, Env0),
    function_must_exist(Name, Node, Env1),
    print_symtabs(Env1), % temporary
    process(Meta),
    Env1.

print_symtabs({[S]}) ->
    io:format('Global: ~p~n', [dict:to_list(S)]);
print_symtabs({[S|Ss]}) ->
    io:format('Local: ~p~n', [dict:to_list(S)]),
    print_symtabs({Ss}).

function_must_exist(Name, Node, Env) ->
    NotFound = {get_line(Node), 'function not found'},
    case lookup(Name, Node, Env) of
        not_found ->
            throw(NotFound);
        SymbolInfo ->
            case get_tag(SymbolInfo) of
                fundec -> ok;
                fundef -> ok;
                _Other -> throw(NotFound)
            end
    end.

function_must_not_exist(Name, Node, Env) ->
    Found = {get_line(Node), 'function already defined'},
    case lookup(Name, Node, Env) of
        not_found ->
            ok;
        SymbolInfo ->
            case get_tag(SymbolInfo) of
                fundec -> throw(Found);
                fundef -> throw(Found);
                _Other -> ok
            end
    end.

lookup(_Name, _Node, {[]}) ->
    not_found;
lookup(Name, Node, {[SymTab|SymTabs]}) ->
    case dict:find(Name, SymTab) of
        {ok, Val} -> Val;
        error     -> lookup(Name, Node, {SymTabs})
    end.

lookup_first_scope(Name, Node, {[SymTab|_SymTabs]}) ->
    lookup(Name, Node, {[SymTab]}).

analyze_arrelem({Meta, _Name, Index}, Env) ->
    process(Meta),
    Env1 = analyze(Index, Env),
    Env1.

analyze_binop({Meta, Lhs, _Op, Rhs}, Env) ->
    process(Meta),
    Env1 = analyze(Lhs, Env),
    Env2 = analyze(Rhs, Env1),
    Env2.

analyze_ident({Meta, _Name}, Env) ->
    process(Meta),
    Env.

analyze_intconst({Meta, _Value}, Env) ->
    process(Meta),
    Env.

analyze_charconst({Meta, _Char}, Env) ->
    process(Meta),
    Env.

analyze_unop({Meta, _Op, Rhs}, Env) ->
    process(Meta),
    Env1 = analyze(Rhs, Env),
    Env1.
