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

exception(Node, Format, Args) ->
    Message = io_lib:format(Format, Args),
    {analyzer_exception, {get_line(Node), Message}}.

analyze(ParseTree) ->
    Env = analyzer_env:new(),
    analyze(ParseTree, Env),
    ParseTree.

analyze(Node, Env0) when erlang:is_tuple(Node) ->
    Tag = get_tag(Node),
    Env1 = analyze_node(Tag, Node, Env0),
    Env1;
analyze(nil, Env0) ->
    Env0;
analyze([], Env0) ->
    Env0;
analyze([Node|Nodes], Env0) ->
    Env1 = analyze(Node, Env0),
    Env2 = analyze(Nodes, Env1),
    Env2.

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
analyze_node(unop, Node, Env)            -> analyze_unop(Node, Env).

analyze_program({_Meta, _File, Topdecs}, Env) ->
    Env1 = analyze(Topdecs, Env),
    Env1.

analyze_scalardec(Node = {_Meta, _Type, Name}, Env0) ->
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

must_not_exist_in_same_scope(Name, Node, Env) ->
    case analyzer_env:lookup_first_scope(Name, Node, Env) of
        not_found ->
            ok;
        _SymbolInfo ->
            throw(exception(Node, 'already defined', []))
    end.

analyze_arraydec(Node = {_Meta, _Type, Name, _Size}, Env0) ->
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    % Parser makes sure that Size is a natural number.
    Env1.

analyze_fundec(Node = {_Meta, _Type, Name, Formals}, Env0) ->
    Redefinition = exception(Node, 'already defined', []),
    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:add_symbol(Name, Node, Env0);
        FoundNode ->
            case get_tag(FoundNode) of
                fundec ->
                    same_return_type(Node, FoundNode),
                    same_formals(Node, FoundNode),
                    Env0;
                fundef ->
                    same_return_type(Node, FoundNode),
                    same_formals(Node, FoundNode),
                    Env0;
                _Other ->
                    throw(Redefinition)
            end
    end,
    Env2 = analyzer_env:enter_scope(Name, Env1),
    _Env3 = analyze(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    Redefinition = exception(Node, 'already defined', []),
    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:add_symbol(Name, Node, Env0);
        FoundNode ->
            case get_tag(FoundNode) of
                fundec ->
                    same_return_type(Node, FoundNode),
                    same_formals(Node, FoundNode),
                    analyzer_env:add_symbol(Name, Node, Env0);
                _Other ->
                    throw(Redefinition)
            end
    end,
    Env2 = analyzer_env:enter_scope(Name, Env1),
    Env3 = analyze(Formals, Env2),
    Env4 = analyze(Locals, Env3),
    _Env5 = analyze(Stmts, Env4),
    Env1. % Updates to the environment are local to the function!

same_formals(Node, FoundNode) ->
    NodeFormals = element(4, Node),
    FoundNodeFormals = element(4, FoundNode),
    same_arity(NodeFormals, FoundNodeFormals, Node),
    identical_types(NodeFormals, FoundNodeFormals, Node).

same_return_type(Node, FoundNode) ->
    case get_type(Node) =:= get_type(FoundNode) of
        true ->
            ok;
        false ->
            throw(exception(Node, 'wrong return type', []))
    end.

same_arity(Formals, FoundFormals, Node) ->
    try same_arity(Formals, FoundFormals)
    catch
        different_arity ->
            throw(exception(Node, 'wrong number of arguments', []))
    end.

same_arity(Formals, FoundFormals) ->
    case erlang:length(Formals) =:= erlang:length(FoundFormals) of
        true  -> ok;
        false -> throw(different_arity)
    end.

identical_types([], [], _CurrentNode) -> ok;
identical_types([F1|Formals1], [F2|Formals2], CurrentNode) ->
    same_tag_and_type(F1, F2, CurrentNode),
    identical_types(Formals1, Formals2, CurrentNode).

% BUG: We are lucky enough to have the same structure for all possible
% formals (scalardec and formal_arraydec), but this simple solution would
% not necessarily work if other types (pointers, fixed size array, ...)
% are introduced.
same_tag_and_type({{_,Tag},Type,_}, {{_,Tag},Type,_}, _CurrentNode) ->
    ok;
same_tag_and_type(_, _, CurrentNode) ->
    throw(exception(CurrentNode, 'parameters must match exactly', [])).

analyze_formal_arraydec(Node = {_Meta, _Type, Name}, Env0) ->
    must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

analyze_if(Node = {_Meta, Cond, Then, Else}, Env) ->
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Then, Env1),
    Env3 = analyze(Else, Env2),
    convertible_to({if_cond, int}, eval_type(Cond, Env3), Node),
    Env3.

analyze_while(Node = {_Meta, Cond, Stmt}, Env) ->
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Stmt, Env1),
    convertible_to({while_cond, int}, eval_type(Cond, Env2), Node),
    Env2.

analyze_return(Node = {_Meta, Expr}, Env) ->
    Env1 = analyze(Expr, Env),
    FunName = analyzer_env:scope_name(Env1),
    FunNode = analyzer_env:lookup(FunName, Node, Env1),
    convertible_to(eval_type(FunNode, Env1), eval_type(Expr, Env1), Node),
    Env1.

analyze_funcall(Node = {_Meta, Name, Actuals}, Env0) ->
    Env1 = analyze(Actuals, Env0),
    must_be_tag(Name, Node, Env1, [fundec,fundef]),
    FoundNode = analyzer_env:lookup(Name, Node, Env1),
    check_actuals(FoundNode, Node, Env1),
    Env1.

check_actuals(Function, Funcall, Env) ->
    Formals = erlang:element(4, Function),
    Actuals = erlang:element(3, Funcall),
    same_arity(Formals, Actuals, Funcall),
    convertible_types(Formals, Actuals, Env).

convertible_types([], [], _Env) -> ok;
convertible_types([F|Formals], [A|Actuals], Env) ->
    convertible_to(eval_type(F, Env), eval_type(A, Env), A),
    convertible_types(Formals, Actuals, Env).

analyze_arrelem(Node = {_Meta, Name, Index}, Env) ->
    must_be_tag(Name, Node, Env, [arraydec,formal_arraydec]),
    Env1 = analyze(Index, Env),
    convertible_to({dontcare,int}, eval_type(Index, Env1), Node),
    Env1.

must_be_tag(Name, Node, Env, Tags) ->
    case analyzer_env:lookup(Name, Node, Env) of
        not_found ->
            throw(exception(Node, 'not defined', []));
        FoundNode ->
            Tag = get_tag(FoundNode),
            case lists:member(Tag, Tags) of
                true  -> ok;
                false -> throw(exception(Node, 'not proper type', []))
            end
    end.

analyze_binop(Node = {_Meta, Lhs, Op, Rhs}, Env0) ->
    Env1 = analyze(Lhs, Env0),
    Env2 = analyze(Rhs, Env1),
    case Op of
        '=' ->
            must_be_lval(Lhs, Env2),
            convertible_to(eval_type(Lhs, Env2), eval_type(Rhs, Env2), Node);
        _Other ->
            eval_type(Node, Env2)
    end,
    Env2.

must_be_lval(Node = {{_,ident},Name}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    case get_tag(FoundNode) of
        scalardec -> ok;
        _Other    -> throw(exception(Node, 'not an l-value', []))
    end;
must_be_lval({{_,arrelem},_Name,_Index}, _Env) -> ok;
must_be_lval(Node, _Env) ->
    throw(exception(Node, 'not an l-value', [])).

analyze_ident(Node = {_Meta, Name}, Env) ->
    must_be_defined(Name, Node, Env),
    Env.

must_be_defined(Name, Node, Env) ->
    case analyzer_env:lookup(Name, Node, Env) of
        not_found ->
            throw(exception(Node, 'not defined', []));
        _FoundNode ->
            ok
    end.

analyze_intconst({_Meta, _Value}, Env) ->
    Env.

analyze_charconst({_Meta, _Char}, Env) ->
    Env.

analyze_unop(Node = {_Meta, _Op, Rhs}, Env) ->
    Env1 = analyze(Rhs, Env),
    convertible_to({dontcare,int}, eval_type(Rhs, Env1), Node),
    Env1.

% make sure all exprs are covered xxxx
eval_type(nil, _Env) ->
    {return_expr, void};
eval_type(Node = {{_, binop}, Lhs, _Op, Rhs}, Env) ->
    Type = widest_type(eval_type(Lhs, Env), eval_type(Rhs, Env), Node),
    {binop, Type};
eval_type(_Node = {{_, unop}, _Op, Rhs}, Env) ->
    {_, Type} = eval_type(Rhs, Env),
    {unop, Type};
eval_type(Node = {{_, ident}, Name}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {get_tag(FoundNode), get_type(FoundNode)};
eval_type(_Node = {{_, intconst}, _Name}, _Env) ->
    {intconst, int};
eval_type(_Node = {{_, charconst}, _Name}, _Env) ->
    {charconst, char};
eval_type(Node = {{_, arrelem}, Name, _Index}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {arrelem, get_type(FoundNode)};
eval_type(Node = {{_, funcall}, Name, _Actuals}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {funcall, get_type(FoundNode)};
eval_type(_Node = {{_, fundec}, Type, _Name, _Formals}, _Env) ->
    {fundec, Type};
eval_type(_Node = {{_, fundef}, Type, _Name, _Formals, _Locals, _Stmts}, _Env) ->
    {fundef, Type};
eval_type(_Node = {{_, scalardec}, Type, _Name}, _Env) ->
    {scalardec, Type};
eval_type(_Node = {{_, arraydec}, Type, _Name, _Size}, _Env) ->
    {arraydec, Type};
eval_type(_Node = {{_, formal_arraydec}, Type, _Name}, _Env) ->
    {formal_arraydec, Type}.

widest_type(Type1, Type2, Node) ->
    try widest_type(Type1, Type2)
    catch
        incompatible ->
            throw(exception(Node, 'incompatible types', []))
    end.

widest_type({_,_}, {arraydec,_})        -> throw(incompatible);
widest_type({arraydec,_}, {_,_})        -> throw(incompatible);
widest_type({_,_}, {formal_arraydec,_}) -> throw(incompatible);
widest_type({formal_arraydec,_}, {_,_}) -> throw(incompatible);
widest_type({_,int}, {_,int})           -> int;
widest_type({_,int}, {_,char})          -> int;
widest_type({_,char}, {_,char})         -> char;
widest_type({_,char}, {_,int})          -> int;
widest_type({_,_}, {_,_})               -> throw(incompatible).

convertible_to(ExpectedTuple, ActualTuple, Actual) ->
    try first_accepts_second(ExpectedTuple, ActualTuple)
    catch
        incompatible ->
            throw(exception(Actual, 'inconvertible types', []))
    end.

first_accepts_second({formal_arraydec, Type}, {arraydec, Type}) -> ok;
first_accepts_second({formal_arraydec, Type}, {formal_arraydec, Type}) -> ok;
first_accepts_second(_, {arraydec,_})    -> throw(incompatible);
first_accepts_second({arraydec,_}, _)    -> throw(incompatible);
first_accepts_second(_, {formal_arraydec,_})    -> throw(incompatible);
first_accepts_second({_,void}, {_,void}) -> ok;
first_accepts_second({_,int}, {_,int})   -> ok;
first_accepts_second({_,int}, {_,char})  -> ok;
first_accepts_second({_,char}, {_,char}) -> ok;
first_accepts_second({_,char}, {_,int})  -> ok;
first_accepts_second(_, _)               -> throw(incompatible).
