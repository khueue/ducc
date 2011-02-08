-module(translator).
-export([translate/1]).

translate(ParseTree) ->
    Env = analyzer_env:new(),
    translate(ParseTree, Env),
    ParseTree.

translate(ParseTree, Env0) ->
    translate_program(ParseTree, Env0).

translate_program({_Meta, _File, Topdecs}, Env0) ->
    Env1 = translate_topdecs(Topdecs, Env0),
    Env1.

translate_topdecs([], Env0) -> [];
translate_topdecs([Topdec|Topdecs], Env0) ->
    translate_topdec(Topdec, Env0),
    translate_topdecs(Topdecs, Env0).

translate_topdec(Topdec, Env0) ->
    Tag = analyzer_helper:get_tag(Topdec),
    case Tag of
        scalardec -> translate_scalardec(Topdec, Env0);
        arraydec  -> translate_arraydec(Topdec, Env0);
        fundec    -> translate_fundec(Topdec, Env0);
        fundef    -> translate_fundef(Topdec, Env0)
    end.

translate_scalardec({_Meta, Type, Name}, Env0) ->
    Env0.

translate_arraydec({_Meta, Type, Name, Size}, Env0) ->
    Env0.

translate_fundec({_Meta, Type, Name, Formals}, Env0) ->
    translate_formals(Formals, Env0),
    Env0.

translate_fundef({_Meta, Type, Name, Formals, Locals, Stmts}, Env0) ->
    translate_formals(Formals, Env0),
    translate_locals(Locals, Env0),
    translate_stmts(Stmts, Env0),
    Env0.

translate_formals([], Env0) -> [];
translate_formals([Formal|Formals], Env0) ->
    translate_formal(Formal, Env0),
    translate_formals(Formals, Env0).

translate_formal(Formal, Env0) ->
    Tag = analyzer_helper:get_tag(Formal),
    case Tag of
        scalardec       -> translate_scalardec(Formal, Env0);
        formal_arraydec -> translate_formal_arraydec(Formal, Env0)
    end.

translate_formal_arraydec(FormalArraydec, Env0) ->
    Env0.

translate_locals([], Env0) -> [];
translate_locals([Local|Locals], Env0) ->
    translate_local(Local, Env0),
    translate_locals(Locals, Env0).

translate_local(Local, Env0) ->
    Tag = analyzer_helper:get_tag(Local),
    case Tag of
        scalardec -> translate_scalardec(Local, Env0);
        arraydec  -> translate_arraydec(Local, Env0)
    end.

translate_stmts([], Env0) -> [];
translate_stmts([Stmt|Stmts], Env0) ->
    translate_stmt(Stmt, Env0),
    translate_stmts(Stmts, Env0).

translate_stmt(nil, Env0) ->
    Env0;
translate_stmt(Stmts, Env0) when erlang:is_list(Stmts) ->
    translate_stmts(Stmts, Env0),
    Env0;
translate_stmt(Stmt, Env0) ->
    Tag = analyzer_helper:get_tag(Stmt),
    case Tag of
        return -> translate_return(Stmt, Env0);
        while  -> translate_while(Stmt, Env0);
        'if'   -> translate_if(Stmt, Env0);
        _Expr  -> translate_expr(Stmt, Env0)
    end.

translate_return({_Meta, Expr}, Env0) ->
    translate_expr(Expr, Env0),
    Env0.

translate_while({_Meta, Cond, Stmt}, Env0) ->
    translate_expr(Cond, Env0),
    translate_stmt(Stmt, Env0),
    Env0.

translate_if({_Meta, Cond, Then, Else}, Env0) ->
    translate_expr(Cond, Env0),
    translate_stmt(Then, Env0),
    translate_stmt(Else, Env0),
    Env0.

translate_expr(Expr, Env0) ->
    Tag = analyzer_helper:get_tag(Expr),
    case Tag of
        binop     -> translate_binop(Expr, Env0);
        unop      -> translate_unop(Expr, Env0);
        ident     -> translate_ident(Expr, Env0);
        intconst  -> translate_intconst(Expr, Env0);
        charconst -> translate_charconst(Expr, Env0);
        funcall   -> translate_funcall(Expr, Env0);
        arrelem   -> translate_arrelem(Expr, Env0)
    end.

translate_binop({_Meta, Lhs, Op, Rhs}, Env0) ->
    translate_expr(Lhs, Env0),
    translate_expr(Rhs, Env0),
    % eval binop with returns from above translations
    Env0.

translate_unop({_Meta, Op, Rhs}, Env0) ->
    translate_expr(Rhs, Env0),
    % eval
    Env0.

translate_ident({_Meta, Name}, Env0) ->
    Env0.

translate_intconst({_Meta, Value}, Env0) ->
    Env0.

translate_charconst({_Meta, Value}, Env0) ->
    Env0.

translate_funcall({_Meta, Name, Actuals}, Env0) ->
    translate_actuals(Actuals, Env0),
    Env0.

translate_actuals([], Env0) -> [];
translate_actuals([Actual|Actuals], Env0) ->
    translate_actual(Actual, Env0),
    translate_actuals(Actuals, Env0).

translate_actual(Actual, Env0) ->
    translate_expr(Actual, Env0).

translate_arrelem({_Meta, Name, Index}, Env0) ->
    translate_expr(Index, Env0),
    Env0.
