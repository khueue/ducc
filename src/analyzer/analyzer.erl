-module(analyzer).
-export([analyze/1]).

-define(HELPER, analyzer_helpers).
-define(RULE, analyzer_rules).
-define(ENV, analyzer_env).

analyze(ParseTree) ->
    Env = ?ENV:new(),
    analyze(ParseTree, Env),
    ParseTree.

analyze(ParseTree, Env0) ->
    analyze_program(ParseTree, Env0).

analyze_program({_, _File, Topdecs}, Env0) ->
    analyze_topdecs(Topdecs, Env0).

analyze_topdecs([], Env0) ->
    Env0;
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

analyze_scalardec(Node = {_, _Type, Name}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = ?ENV:set_symbol(Name, Node, Env0),
    Env1.

analyze_arraydec(Node = {_, _Type, Name, _Size}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = ?ENV:set_symbol(Name, Node, Env0),
    % Parser makes sure that Size is a natural number.
    Env1.

analyze_fundec(Node = {_, _Type, Name, Formals}, Env0) ->
    ElseRedef = ?HELPER:exception(Node, "function '~s' already defined", [Name]),
    Env1 = case ?ENV:lookup(Name, Node, Env0) of
        not_found ->
            ?ENV:set_symbol(Name, Node, Env0);
        FoundNode ->
            ?RULE:must_be_tag_member(FoundNode, [fundec,fundef], ElseRedef),
            ?RULE:same_return_type(Node, FoundNode),
            ?RULE:same_formals(Node, FoundNode),
            Env0
    end,
    Env2 = ?ENV:enter_scope(Name, Env1),
    _Env3 = analyze_formals(Formals, Env2),
    Env1. % Updates to the environment are local to the function!

analyze_fundef(Node = {_, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    ElseRedef = ?HELPER:exception(Node, "function '~s' already defined", [Name]),
    case ?ENV:lookup(Name, Node, Env0) of
        not_found ->
            ok;
        FoundNode ->
            ?RULE:must_be_tag_member(FoundNode, [fundec], ElseRedef),
            ?RULE:same_return_type(Node, FoundNode),
            ?RULE:same_formals(Node, FoundNode)
    end,
    Env1 = ?ENV:set_symbol(Name, Node, Env0),
    Env2 = ?ENV:enter_scope(Name, Env1),
    Env3 = analyze_formals(Formals, Env2),
    Env4 = analyze_locals(Locals, Env3),
    _Env5 = analyze_stmts(Stmts, Env4),
    Env1. % Updates to the environment are local to the function!

analyze_formals([], Env0) ->
    Env0;
analyze_formals([Formal|Formals], Env0) ->
    Env1 = analyze_formal(Formal, Env0),
    analyze_formals(Formals, Env1).

analyze_formal(Formal, Env0) ->
    Tag = ?HELPER:get_tag(Formal),
    case Tag of
        scalardec -> analyze_scalardec(Formal, Env0);
        farraydec -> analyze_farraydec(Formal, Env0)
    end.

analyze_farraydec(Node = {_, _Type, Name}, Env0) ->
    ?RULE:must_not_exist_in_same_scope(Name, Node, Env0),
    Env1 = ?ENV:set_symbol(Name, Node, Env0),
    Env1.

analyze_locals([], Env0) ->
    Env0;
analyze_locals([Local|Locals], Env0) ->
    Env1 = analyze_local(Local, Env0),
    analyze_locals(Locals, Env1).

analyze_local(Local, Env0) ->
    Tag = ?HELPER:get_tag(Local),
    case Tag of
        scalardec -> analyze_scalardec(Local, Env0);
        arraydec  -> analyze_arraydec(Local, Env0)
    end.

analyze_stmts([], Env0) ->
    Env0;
analyze_stmts([Stmt|Stmts], Env0) ->
    Env1 = analyze_stmt(Stmt, Env0),
    analyze_stmts(Stmts, Env1).

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

analyze_return(Node = {_, Expr}, Env0) ->
    Env1 = analyze_return_expr(Expr, Env0),
    FunName = ?ENV:scope_name(Env1),
    FunNode = ?ENV:lookup(FunName, Node, Env1),
    ReturnType = ?HELPER:eval_type(FunNode, Env1),
    ExprType = ?HELPER:eval_type(Expr, Env1),
    ?HELPER:convertible_to(ReturnType, ExprType, Node),
    Env1.

analyze_return_expr(nil, Env0) ->
    Env0;
analyze_return_expr(Expr, Env0) ->
    analyze_expr(Expr, Env0).

analyze_while(Node = {_, Cond, Stmt}, Env0) ->
    Env1 = analyze_expr(Cond, Env0),
    Env2 = analyze_stmt(Stmt, Env1),
    CondType = ?HELPER:eval_type(Cond, Env2),
    ?HELPER:convertible_to(int, CondType, Node),
    Env2.

analyze_if(Node = {_, Cond, Then, Else}, Env0) ->
    Env1 = analyze_expr(Cond, Env0),
    Env2 = analyze_stmt(Then, Env1),
    Env3 = analyze_else_stmt(Else, Env2),
    CondType = ?HELPER:eval_type(Cond, Env3),
    ?HELPER:convertible_to(int, CondType, Node),
    Env3.

analyze_else_stmt(nil, Env0) ->
    Env0;
analyze_else_stmt(Stmt, Env0) ->
    analyze_stmt(Stmt, Env0).

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

analyze_funcall(Node = {_, Name, Actuals}, Env0) ->
    ElseUndec  = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    ElseNotFun = ?HELPER:exception(Node, "'~s' is not a function", [Name]),
    Env1 = analyze_actuals(Actuals, Env0),
    FoundNode = ?ENV:lookup_or_throw(Name, Node, Env1, ElseUndec),
    ?RULE:must_be_tag_member(FoundNode, [fundec,fundef], ElseNotFun),
    ?RULE:check_actuals(FoundNode, Node, Env1),
    Env1.

analyze_actuals([], Env0) ->
    Env0;
analyze_actuals([Actual|Actuals], Env0) ->
    Env1 = analyze_actual(Actual, Env0),
    analyze_actuals(Actuals, Env1).

analyze_actual(Actual, Env0) ->
    analyze_expr(Actual, Env0).

analyze_binop(Node = {_, Lhs, Op, Rhs}, Env0) ->
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

analyze_ident(Node = {_, Name}, Env0) ->
    ElseUndec = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    _FoundNode = ?ENV:lookup_or_throw(Name, Node, Env0, ElseUndec),
    Env0.

analyze_intconst({_, _Value}, Env0) ->
    Env0.

analyze_charconst({_, _Value}, Env0) ->
    Env0.

analyze_unop(Node = {_, _Op, Rhs}, Env0) ->
    Env1 = analyze_expr(Rhs, Env0),
    RhsType = ?HELPER:eval_type(Rhs, Env1),
    ?HELPER:convertible_to(int, RhsType, Node),
    Env1.

analyze_arrelem(Node = {_, Name, Index}, Env0) ->
    ElseUndec   = ?HELPER:exception(Node, "'~s' is undeclared", [Name]),
    ElseBadType = ?HELPER:exception(Node, "'~s' is not an array", [Name]),
    FoundNode = ?ENV:lookup_or_throw(Name, Node, Env0, ElseUndec),
    ?RULE:must_be_tag_member(FoundNode, [arraydec,farraydec], ElseBadType),
    Env1 = analyze_expr(Index, Env0),
    IndexType = ?HELPER:eval_type(Index, Env1),
    ?HELPER:convertible_to(int, IndexType, Node),
    Env1.
