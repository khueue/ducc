-module(analyzer).
-export([analyze/1]).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

get_line(Node) ->
    {Line, _Tag} = get_meta(Node),
    Line.

get_type(Node) ->
    erlang:element(2, Node).

get_meta(Node) ->
    erlang:element(1, Node).

analyze(ParseTree) ->
    Env = analyzer_env:new(),
    try analyze(ParseTree, Env)
    catch
        Details ->
            throw({analyzer_exception, Details})
    end,
    ok.

analyze(Node, Env) when erlang:is_tuple(Node) ->
    Tag = get_tag(Node),
    Env1 = analyze_node(Tag, Node, Env),
    Env1;
analyze(nil, Env) ->
    Env;
analyze([], Env) ->
    Env;
analyze([X|Xs], Env) ->
    Env1 = analyze(X, Env),
    Env2 = analyze(Xs, Env1),
    Env2;
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
process(X) ->
    io:format('~p~n', [X]).

analyze_program({Meta, _File, Topdecs}, Env) ->
    process(Meta),
    Env1 = analyze(Topdecs, Env),
    Env1.

analyze_scalardec(Node = {Meta, _Type, Name}, Env0) ->
    process(Meta),
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

must_not_exist_in_same_scope(Name, Node, Env) ->
    case analyzer_env:lookup_first_scope(Name, Node, Env) of
        not_found ->
            ok;
        _SymbolInfo ->
            throw({get_line(Node), 'already defined'})
    end.

analyze_arraydec(Node = {Meta, _Type, Name, _Size}, Env0) ->
    process(Meta),
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

analyze_fundec(Node = {Meta, _Type, Name, Formals}, Env0) ->
    process(Meta),

    Redefinition = {get_line(Node), 'already defined'},
    Conflicts = {get_line(Node), 'conflicting types'},

    case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            ok;
        Val ->
            case get_tag(Val) of
                fundec ->
                    case check_formals(Node, Val) of
                        true ->
                            ok;
                        false ->
                            throw(Conflicts)
                    end;
                fundef ->
                    case check_formals(Node, Val) of
                        true ->
                            ok;
                        false ->
                            throw(Conflicts)
                    end;
                _Other ->
                    throw(Redefinition)
            end
    end,

    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env2 = analyzer_env:enter_scope(Name, Env1),
    _Env3 = analyze(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    process(Meta),
    Redefinition = {get_line(Node), 'already defined'},
    Conflicts    = {get_line(Node), 'conflicting types'},

    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:add_symbol(Name, Node, Env0);
        Val ->
            case get_tag(Val) of
                fundec ->
                    case check_formals(Node, Val) of
                        true ->
                            analyzer_env:add_symbol(Name, Node, Env0);
                        false ->
                            throw(Conflicts)
                    end;
                _Other ->
                    throw(Redefinition)
            end
    end,

    Env2 = analyzer_env:enter_scope(Name, Env1),
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
    same_type(Node1, Node2) andalso
    identical_types(Node1Formals, Node2Formals).

same_type(Node1, Node2) ->
    get_type(Node1) =:= get_type(Node2).

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

analyze_formal_arraydec(Node = {Meta, _Type, Name}, Env0) ->
    process(Meta),
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

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

analyze_return(Node = {Meta, Expr}, Env) ->
    process(Meta),
    Env1 = analyze(Expr, Env),
    ScopeName = analyzer_env:scope_name(Env1),
    FunInfo = analyzer_env:lookup(ScopeName, Node, Env1),
    convertible_to(eval_type(FunInfo, Env1), eval_type(Expr, Env1)),
    Env1.

analyze_funcall(Node = {Meta, Name, Actuals}, Env0) ->
    process(Meta),
    Env1 = analyze(Actuals, Env0),
    must_be_tag(Name, Node, Env1, [fundec,fundef]),
    analyzer_env:print_symtabs(Env1), % temporary
    Env1.

analyze_arrelem(Node = {Meta, Name, Index}, Env) ->
    process(Meta),
    must_be_tag(Name, Node, Env, [arraydec,formal_arraydec]),
    Env1 = analyze(Index, Env),
    % xxx convertible_to_int(Index, Env)
    Env1.

must_be_tag(Name, Node, Env, Tags) ->
    case analyzer_env:lookup(Name, Node, Env) of
        not_found ->
            throw({get_line(Node), 'not defined'});
        SymbolInfo ->
            Tag = get_tag(SymbolInfo),
            case lists:member(Tag, Tags) of
                true  -> ok;
                false -> throw({get_line(Node), 'not proper type'})
            end
    end.

% just for fun right now xxx
analyze_binop(_Node = {Meta, Lhs, '=', Rhs}, Env0) ->
    process(Meta),
    Env1 = analyze(Lhs, Env0),
    Env2 = analyze(Rhs, Env1),
    must_be_lval(Lhs, Env2),
    convertible_to(eval_type(Lhs, Env2), eval_type(Rhs, Env2)),
    Env2;
analyze_binop(Node = {Meta, Lhs, _Op, Rhs}, Env0) ->
    process(Meta),
    Env1 = analyze(Lhs, Env0),
    Env2 = analyze(Rhs, Env1),
    _TypeTuple = eval_type(Node, Env2),
    Env2.

%%% xxxxxxx
must_be_lval(Node = {{_,ident},Name}, Env) ->
    SymbolInfo = analyzer_env:lookup(Name, Node, Env),
    case get_tag(SymbolInfo) of
        fundef -> throw({get_line(Node), 'not an l-value'});
        fundec -> throw({get_line(Node), 'not an l-value'});
        _      -> ok
    end;
must_be_lval({{_,arrelem},_Name,_Index}, _Env) -> ok;
must_be_lval(Node, _Env) ->
    throw({get_line(Node), 'not an l-value'}).

analyze_ident(Node = {Meta, Name}, Env) ->
    process(Meta),
    must_be_defined(Name, Node, Env),
    Env.

must_be_defined(Name, Node, Env) ->
    case analyzer_env:lookup(Name, Node, Env) of
        not_found ->
            throw({get_line(Node), 'not defined'});
        _SymbolInfo ->
            ok
    end.

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

% make sure all exprs are covered xxxx
eval_type(nil, _Env) -> % return ;
    {returnxxxxx, void};
eval_type(_Node = {{_, binop}, Lhs, _Op, Rhs}, Env) ->
    {_, Type} = widest_type(eval_type(Lhs, Env), eval_type(Rhs, Env)),
    {binop, Type};
eval_type(_Node = {{_, unop}, _Op, Rhs}, Env) ->
    {_, Type} = eval_type(Rhs, Env),
    {unop, Type};
eval_type(Node = {{_, ident}, Name}, Env) ->
    SymbolInfo = analyzer_env:lookup(Name, Node, Env),
    {get_tag(SymbolInfo), get_type(SymbolInfo)};
eval_type(_Node = {{_, intconst}, _Name}, _Env) ->
    {intconst, int};
eval_type(_Node = {{_, charconst}, _Name}, _Env) ->
    {charconst, char};
eval_type(Node = {{_, arrelem}, Name, _Index}, Env) ->
    SymbolInfo = analyzer_env:lookup(Name, Node, Env),
    {arrelem, get_type(SymbolInfo)};
eval_type(Node = {{_, funcall}, Name, _Actuals}, Env) ->
    SymbolInfo = analyzer_env:lookup(Name, Node, Env),
    {funcall, get_type(SymbolInfo)};
eval_type(_Node = {{_, fundec}, Type, _Name, _Formals}, _Env) ->
    {fundec, Type};
eval_type(_Node = {{_, fundef}, Type, _Name, _Formals, _Locals, _Stmts}, _Env) ->
    {fundef, Type}.
%% formal array

widest_type(_, {arraydec,_})        -> throw({444, incompatible});
widest_type({arraydec,_}, _)        -> throw({444, incompatible});
widest_type(_, {formal_arraydec,_})        -> throw({444, incompatible});
widest_type({formal_arraydec,_}, _)        -> throw({444, incompatible});
widest_type(W = {_,int}, {_,int})   -> W;
widest_type(W = {_,int}, {_,char})  -> W;
widest_type(W = {_,char}, {_,char}) -> W;
widest_type({_,char}, W = {_,int})  -> W;
widest_type(_, _)                   -> throw({444, incompatible}).

convertible_to(ExpectedTuple, ActualTuple) ->
    io:format('exp ~p~n', [ExpectedTuple]),
    io:format('act ~p~n', [ActualTuple]),
    try first_accepts_second(ExpectedTuple, ActualTuple)
    catch
        incompatible ->
            throw({666, 'incomp xxx'})
    end.

first_accepts_second(_, {arraydec,_})    -> throw(incompatible);
first_accepts_second(_, {formal_arraydec,_})    -> throw(incompatible);
first_accepts_second({_,void}, {_,void}) -> ok;
first_accepts_second({_,int}, {_,int})   -> ok;
first_accepts_second({_,int}, {_,char})  -> ok;
first_accepts_second({_,char}, {_,char}) -> ok;
first_accepts_second({_,char}, {_,int})  -> ok;
first_accepts_second(_, _)               -> throw(incompatible).

% convertible_to_int: intconst, charconst, arrelem(int), arrelem(char),
% scalardec(int)
