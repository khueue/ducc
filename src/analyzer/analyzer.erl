-module(analyzer).
-export([analyze/1]).

-define(HELPER, analyzer_helpers).
-define(RULE, analyzer_rules).

analyze(ParseTree) ->
    Env = analyzer_env:new(),
    analyze(ParseTree, Env),
    ParseTree.

analyze(ParseTree, Env0) ->
    analyze_program(ParseTree, Env0).

analyze_program({_Meta, _File, Topdecs}, Env0) ->
    Env1 = analyze_topdecs(Topdecs, Env0),
    Env1.

analyze_topdecs([], Env) -> Env;
analyze_topdecs([Topdec|Topdecs], Env0) ->
    Env1 = analyze_topdec(Topdec, Env0),
    analyze_topdecs(Topdecs, Env1).

analyze_topdec(Topdec, Env0) ->
    Tag = ?HELPER:get_tag(Topdec),
    case Tag of
        scalardec -> analyze_scalardec(Topdec, Env0);
        arraydec  -> analyze_arraydec(Topdec, Env0);
        fundec    -> analyze_fundec(Topdec, Env0);
        fundef    -> analyze_fundef(Topdec, Env0)
    end.

analyze_scalardec(Node = {_Meta, _Type, Name}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:set_symbol(Name, Node, Env0),
    Env1.

analyze_arraydec(Node = {_Meta, _Type, Name, _Size}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:set_symbol(Name, Node, Env0),
    % Parser makes sure that Size is a natural number.
    Env1.

analyze_fundec(Node = {_Meta, _Type, Name, Formals}, Env0) ->
    ElseRedef = ?HELPER:exception(Node, "function '~s' already defined", [Name]),
    Env1 = case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            analyzer_env:set_symbol(Name, Node, Env0);
        FoundNode ->
            ?RULE:must_be_tag_member(FoundNode, [fundec,fundef], ElseRedef),
            ?RULE:same_return_type(Node, FoundNode),
            ?RULE:same_formals(Node, FoundNode),
            Env0
    end,
    Env2 = analyzer_env:enter_scope(Name, Env1),
    _Env3 = analyze_formals(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    ElseRedef = ?HELPER:exception(Node, "function '~s' already defined", [Name]),
    case analyzer_env:lookup(Name, Node, Env0) of
        not_found ->
            ok;
        FoundNode ->
            ?RULE:must_be_tag_member(FoundNode, [fundec], ElseRedef),
            ?RULE:same_return_type(Node, FoundNode),
            ?RULE:same_formals(Node, FoundNode)
    end,
    Env1 = analyzer_env:set_symbol(Name, Node, Env0),
    Env2 = analyzer_env:enter_scope(Name, Env1),
    Env3 = analyze_formals(Formals, Env2),
    Env4 = analyze_locals(Locals, Env3),
    _Env5 = analyze_stmts(Stmts, Env4),
    Env1. % Updates to the environment are local to the function!

analyze_formals([], Env) -> Env;
analyze_formals([Formal|Formals], Env0) ->
    Env1 = analyze_formal(Formal, Env0),
    analyze_formals(Formals, Env1).

analyze_formal(Formal, Env0) ->
    Tag = ?HELPER:get_tag(Formal),
    case Tag of
        scalardec       -> analyze_scalardec(Formal, Env0);
        formal_arraydec -> analyze_formal_arraydec(Formal, Env0)
    end.

analyze_formal_arraydec(Node = {_Meta, _Type, Name}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = analyzer_env:set_symbol(Name, Node, Env0),
    Env1.

analyze_locals([], Env) -> Env;
analyze_locals([Local|Locals], Env0) ->
    Env1 = analyze_local(Local, Env0),
    analyze_locals(Locals, Env1).

analyze_local(Local, Env0) ->
    Tag = ?HELPER:get_tag(Local),
    case Tag of
        scalardec -> analyze_scalardec(Local, Env0);
        arraydec  -> analyze_arraydec(Local, Env0)
    end.

analyze_stmts([], Env) -> Env;
analyze_stmts([Stmt|Stmts], Env0) ->
    Env1 = analyze_stmt(Stmt, Env0),
    analyze_stmts(Stmts, Env1).

analyze_stmt(nil, Env0) -> %% xxx remove
    Env0;
analyze_stmt(Stmts, Env0) when erlang:is_list(Stmts) ->
    analyze_stmts(Stmts, Env0);
analyze_stmt(Stmt, Env0) ->
    Tag = ?HELPER:get_tag(Stmt),
    case Tag of
        return -> analyze_return(Stmt, Env0);
        while  -> analyze_while(Stmt, Env0);
        'if'   -> analyze_if(Stmt, Env0);
        _Expr  -> analyze_expr(Stmt, Env0)
    end.

analyze_if(Node = {_Meta, Cond, Then, Else}, Env0) ->
    Env1 = analyze_expr(Cond, Env0),
    Env2 = analyze_stmt(Then, Env1),
    Env3 = analyze_stmt(Else, Env2),
    CondType = ?HELPER:eval_type(Cond, Env3),
    ?HELPER:convertible_to(int, CondType, Node),
    Env3.

analyze_while(Node = {_Meta, Cond, Stmt}, Env0) ->
    Env1 = analyze_expr(Cond, Env0),
    Env2 = analyze_stmt(Stmt, Env1),
    CondType = ?HELPER:eval_type(Cond, Env2),
    ?HELPER:convertible_to(int, CondType, Node),
    Env2.

analyze_return(Node = {_Meta, Expr}, Env0) ->
    Env1 = analyze_return_expr(Expr, Env0),
    FunName = analyzer_env:scope_name(Env1),
    FunNode = analyzer_env:lookup(FunName, Node, Env1),
    ReturnType = ?HELPER:eval_type(FunNode, Env1),
    ExprType = ?HELPER:eval_type(Expr, Env1),
    ?HELPER:convertible_to(ReturnType, ExprType, Node),
    Env1.

analyze_return_expr(nil, Env0) ->
    Env0;
analyze_return_expr(Expr, Env0) ->
    analyze_expr(Expr, Env0).

analyze_expr(Expr, Env0) ->
    Tag = ?HELPER:get_tag(Expr),
    case Tag of
        binop     -> analyze_binop(Expr, Env0);
        unop      -> analyze_unop(Expr, Env0);
        ident     -> analyze_ident(Expr, Env0);
        intconst  -> analyze_intconst(Expr, Env0);
        charconst -> analyze_charconst(Expr, Env0);
        funcall   -> analyze_funcall(Expr, Env0);
        arrelem   -> analyze_arrelem(Expr, Env0)
    end.

analyze_funcall(Node = {_Meta, Name, Actuals}, Env0) ->
    ElseUndec  = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    ElseNotFun = ?HELPER:exception(Node, "'~s' is not a function", [Name]),
    Env1 = analyze_actuals(Actuals, Env0),
    FoundNode = analyzer_env:lookup_or_throw(Name, Node, Env1, ElseUndec),
    ?RULE:must_be_tag_member(FoundNode, [fundec,fundef], ElseNotFun),
    ?RULE:check_actuals(FoundNode, Node, Env1),
    Env1.

analyze_actuals([], Env) -> Env;
analyze_actuals([Actual|Actuals], Env0) ->
    Env1 = analyze_actual(Actual, Env0),
    analyze_actuals(Actuals, Env1).

analyze_actual(Actual, Env0) ->
    analyze_expr(Actual, Env0).

analyze_binop(Node = {_Meta, Lhs, Op, Rhs}, Env0) ->
    Env1 = analyze_expr(Lhs, Env0),
    Env2 = analyze_expr(Rhs, Env1),
    case Op of
        '=' ->
            ?RULE:must_be_lval(Lhs, Env2),
            LhsType = ?HELPER:eval_type(Lhs, Env2),
            RhsType = ?HELPER:eval_type(Rhs, Env2),
            ?HELPER:convertible_to(LhsType, RhsType, Node);
        _OtherBinop ->
            ?HELPER:eval_type(Node, Env2)
    end,
    Env2.

analyze_ident(Node = {_Meta, Name}, Env0) ->
    ElseUndec = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    _FoundNode = analyzer_env:lookup_or_throw(Name, Node, Env0, ElseUndec),
    Env0.

analyze_intconst({_Meta, _Value}, Env0) ->
    Env0.

analyze_charconst({_Meta, _Value}, Env0) ->
    Env0.

analyze_unop(Node = {_Meta, _Op, Rhs}, Env0) ->
    Env1 = analyze_expr(Rhs, Env0),
    RhsType = ?HELPER:eval_type(Rhs, Env1),
    ?HELPER:convertible_to(int, RhsType, Node),
    Env1.

analyze_arrelem(Node = {_Meta, Name, Index}, Env0) ->
    ElseUndec   = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    ElseBadType = ?HELPER:exception(Node, "'~s' is not an array", [Name]),
    FoundNode = analyzer_env:lookup_or_throw(Name, Node, Env0, ElseUndec),
    ?RULE:must_be_tag_member(FoundNode, [arraydec,formal_arraydec], ElseBadType),
    Env1 = analyze_expr(Index, Env0),
    IndexType = ?HELPER:eval_type(Index, Env1),
    ?HELPER:convertible_to(int, IndexType, Node),
    Env1.
