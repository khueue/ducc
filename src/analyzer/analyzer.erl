-module(analyzer).
-export([analyze/1]).

-define(HELPER, analyzer_helpers).
-define(RULE, analyzer_rules).

analyze(ParseTree) ->
    Env = analyzer_env:new(),
    analyze(ParseTree, Env),
    ParseTree.

analyze(Node, Env0) when erlang:is_tuple(Node) ->
    Tag = ?HELPER:get_tag(Node),
    Env1 = analyze_node(Tag, Node, Env0),
    Env1;
analyze(nil, Env0) -> % Empty statement and empty return.
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
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

analyze_arraydec(Node = {_Meta, _Type, Name, _Size}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    % Parser makes sure that Size is a natural number.
    Env1.

analyze_fundec(Node = {_Meta, _Type, Name, Formals}, Env0) ->
    Redefinition = ?HELPER:exception(Node, 'already defined', []),
    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:add_symbol(Name, Node, Env0);
        FoundNode ->
            case ?HELPER:get_tag(FoundNode) of
                fundec ->
                    ?RULE:same_return_type(Node, FoundNode),
                    ?RULE:same_formals(Node, FoundNode),
                    Env0;
                fundef ->
                    ?RULE:same_return_type(Node, FoundNode),
                    ?RULE:same_formals(Node, FoundNode),
                    Env0;
                _Other ->
                    throw(Redefinition)
            end
    end,
    Env2 = analyzer_env:enter_scope(Name, Env1),
    _Env3 = analyze(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    Redefinition = ?HELPER:exception(Node, 'already defined', []),
    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:add_symbol(Name, Node, Env0);
        FoundNode ->
            case ?HELPER:get_tag(FoundNode) of
                fundec ->
                    ?RULE:same_return_type(Node, FoundNode),
                    ?RULE:same_formals(Node, FoundNode),
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

analyze_formal_arraydec(Node = {_Meta, _Type, Name}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:add_symbol(Name, Node, Env0),
    Env1.

analyze_if(Node = {_Meta, Cond, Then, Else}, Env) ->
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Then, Env1),
    Env3 = analyze(Else, Env2),
    ?HELPER:convertible_to({if_cond, int}, ?HELPER:eval_type(Cond, Env3), Node),
    Env3.

analyze_while(Node = {_Meta, Cond, Stmt}, Env) ->
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Stmt, Env1),
    ?HELPER:convertible_to({while_cond, int}, ?HELPER:eval_type(Cond, Env2),
                           Node),
    Env2.

analyze_return(Node = {_Meta, Expr}, Env) ->
    Env1 = analyze(Expr, Env),
    FunName = analyzer_env:scope_name(Env1),
    FunNode = analyzer_env:lookup(FunName, Node, Env1),
    ?HELPER:convertible_to(?HELPER:eval_type(FunNode, Env1),
                           ?HELPER:eval_type(Expr, Env1), Node),
    Env1.

analyze_funcall(Node = {_Meta, Name, Actuals}, Env0) ->
    Env1 = analyze(Actuals, Env0),
    ?RULE:must_be_tag(Name, Node, Env1, [fundec,fundef]),
    FoundNode = analyzer_env:lookup(Name, Node, Env1),
    ?RULE:check_actuals(FoundNode, Node, Env1),
    Env1.

analyze_arrelem(Node = {_Meta, Name, Index}, Env) ->
    ?RULE:must_be_tag(Name, Node, Env, [arraydec,formal_arraydec]),
    Env1 = analyze(Index, Env),
    ?HELPER:convertible_to({dontcare,int}, ?HELPER:eval_type(Index, Env1), Node),
    Env1.

analyze_binop(Node = {_Meta, Lhs, Op, Rhs}, Env0) ->
    Env1 = analyze(Lhs, Env0),
    Env2 = analyze(Rhs, Env1),
    case Op of
        '=' ->
            ?RULE:must_be_lval(Lhs, Env2),
            ?HELPER:convertible_to(?HELPER:eval_type(Lhs, Env2),
                                   ?HELPER:eval_type(Rhs, Env2), Node);
        _Other ->
            ?HELPER:eval_type(Node, Env2)
    end,
    Env2.

analyze_ident(Node = {_Meta, Name}, Env) ->
    ?RULE:must_be_defined(Name, Node, Env),
    Env.

analyze_intconst({_Meta, _Value}, Env) ->
    Env.

analyze_charconst({_Meta, _Char}, Env) ->
    Env.

analyze_unop(Node = {_Meta, _Op, Rhs}, Env) ->
    Env1 = analyze(Rhs, Env),
    ?HELPER:convertible_to({dontcare,int}, ?HELPER:eval_type(Rhs, Env1), Node),
    Env1.
