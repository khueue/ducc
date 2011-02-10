-module(translator).
-export([translate/1]).
-compile(export_all).

-define(HELPER, analyzer_helpers).
-define(RULE, analyzer_rules).
-define(ENV, analyzer_env).
-define(RTL, rtl_constructors).

% {local,{temp,123}},    array, {Type, Count, Offset}
% {local,{temp,123}},   scalar, {Type}
% {global,{label,321}},  array, {Type, Count}
% {global,{label,321}}, scalar, {Type}

translate(ParseTree) ->
    Env = ?ENV:new(),
    % pass last used, or first free temp?
    {Env1, Instrs, Ret1} = translate(ParseTree, Env, fp()),
    Instrs.

translate(ParseTree, Env0, Ret0) ->
    translate_program(ParseTree, Env0, Ret0).

translate_program({_Meta, _File, Topdecs}, Env0, Ret0) ->
    translate_topdecs(Topdecs, Env0, Ret0).

translate_topdecs([], Env0, Ret0) ->
    {Env0, [], Ret0};
translate_topdecs([Topdec|Topdecs], Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_topdec(Topdec, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_topdecs(Topdecs, Env1, Ret1),
    {Env2, Instrs1++Instrs2, Ret2}.

translate_topdec(Topdec, Env0, Ret0) ->
    Tag = ?HELPER:get_tag(Topdec),
    case Tag of
        scalardec -> translate_scalardec(Topdec, Env0, Ret0);
        arraydec  -> translate_arraydec(Topdec, Env0, Ret0);
        fundec    -> translate_fundec(Topdec, Env0, Ret0);
        fundef    -> translate_fundef(Topdec, Env0, Ret0)
    end.

translate_scalardec({_Meta, Type, Name}, Env0, Ret0) ->
    Instrs =
    [
        emit(scalar)
    ],
    {Env0, Instrs, Ret0}.

translate_arraydec({_Meta, Type, Name, Size}, Env0, Ret0) ->
    Instrs =
    [
        emit(arraydec)
    ],
    {Env0, Instrs, Ret0}.

translate_fundec({_Meta, Type, Name, Formals}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_formals(Formals, Env0, Ret0),
    Instrs2 =
    [
        emit(fundec)
    ],
    {Env1, Instrs1++Instrs2, Ret1}.

translate_fundef({_Meta, Type, Name, Formals, Locals, Stmts}, Env0, Ret0) ->
    translate_formals(Formals, Env0, Ret0),
    {Env1, Instrs1, Ret1} = translate_formals(Formals, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_locals(Locals, Env1, Ret1),
    {Env3, Instrs3, Ret3} = translate_stmts(Stmts, Env2, Ret2),
    Instrs4 =
    [
        emit(fundef)
    ],
    {Env3, Instrs1++Instrs2++Instrs3++Instrs4, Ret3}.

translate_formals([], Env0, Ret0) ->
    {Env0, [], Ret0};
translate_formals([Formal|Formals], Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_formal(Formal, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_formals(Formals, Env1, Ret1),
    {Env2, Instrs1++Instrs2, Ret2}.

translate_formal(Formal, Env0, Ret0) ->
    Tag = ?HELPER:get_tag(Formal),
    case Tag of
        scalardec -> translate_scalardec(Formal, Env0, Ret0);
        farraydec -> translate_farraydec(Formal, Env0, Ret0)
    end.

translate_farraydec({_Meta, Type, Name}, Env0, Ret0) ->
    Instrs =
    [
        emit(farraydec)
    ],
    {Env0, Instrs, Ret0}.

translate_locals([], Env0, Ret0) ->
    {Env0, [], Ret0};
translate_locals([Local|Locals], Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_local(Local, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_locals(Locals, Env1, Ret1),
    {Env2, Instrs1++Instrs2, Ret2}.

translate_local(Local, Env0, Ret0) ->
    Tag = ?HELPER:get_tag(Local),
    case Tag of
        scalardec -> translate_scalardec(Local, Env0, Ret0);
        arraydec  -> translate_arraydec(Local, Env0, Ret0)
    end.

translate_stmts([], Env0, Ret0) ->
    {Env0, [], Ret0};
translate_stmts([Stmt|Stmts], Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_stmt(Stmt, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_stmts(Stmts, Env1, Ret1),
    {Env2, Instrs1++Instrs2, Ret2}.

translate_stmt(Stmts, Env0, Ret0) when erlang:is_list(Stmts) ->
    translate_stmts(Stmts, Env0, Ret0);
translate_stmt(Stmt, Env0, Ret0) ->
    Tag = ?HELPER:get_tag(Stmt),
    case Tag of
        return -> translate_return(Stmt, Env0, Ret0);
        while  -> translate_while(Stmt, Env0, Ret0);
        'if'   -> translate_if(Stmt, Env0, Ret0);
        _Expr  -> translate_expr(Stmt, Env0, Ret0)
    end.

translate_return({_Meta, Expr}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Expr, Env0, Ret0),
    Instrs2 =
    [
        emit(return)
    ],
    {Env1, Instrs1++Instrs2, Ret1}.

translate_while({_Meta, Cond, Stmt}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Cond, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_stmt(Stmt, Env1, Ret1),
    Instrs3 =
    [
        emit(while)
    ],
    {Env2, Instrs1++Instrs2++Instrs3, Ret2}.

translate_if({_Meta, Cond, Then, Else}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Cond, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_stmt(Then, Env1, Ret1),
    {Env3, Instrs3, Ret3} = translate_stmt(Else, Env2, Ret2),
    Instrs4 =
    [
        emit('if')
    ],
    {Env3, Instrs1++Instrs2++Instrs3++Instrs4, Ret3}.

translate_expr(Expr, Env0, Ret0) ->
    Tag = ?HELPER:get_tag(Expr),
    case Tag of
        binop     -> translate_binop(Expr, Env0, Ret0);
        unop      -> translate_unop(Expr, Env0, Ret0);
        ident     -> translate_ident(Expr, Env0, Ret0);
        intconst  -> translate_intconst(Expr, Env0, Ret0);
        charconst -> translate_charconst(Expr, Env0, Ret0);
        funcall   -> translate_funcall(Expr, Env0, Ret0);
        arrelem   -> translate_arrelem(Expr, Env0, Ret0)
    end.

translate_binop({_Meta, Lhs, Op, Rhs}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Lhs, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_expr(Rhs, Env1, Ret1),
    {Env3, Instrs3, Ret3} = translate_eval(Op, Env2, Ret2, Ret1, Ret2),
    {Env3, Instrs1++Instrs2++Instrs3, Ret3}.

translate_intconst({_Meta, Value}, Env0, Ret0) ->
    Ret1 = new_temp(Ret0),
    Instrs =
    [
        emit_intconst(Ret1, Value)
    ],
    {Env0, Instrs, Ret1}.

translate_eval(Op, Env0, Ret0, RetLhs, RetRhs) ->
    Ret1 = new_temp(Ret0),
    Instrs =
    [
        emit_eval(Op, Ret1, RetLhs, RetRhs)
    ],
    {Env0, Instrs, Ret1}.

emit_intconst(TempRet, Value) ->
    {icon, Value}.

emit_eval('+', TempRet, TempLhs, TempRhs) ->
    {eval, TempRet, {add, TempLhs, TempRhs}}.

emit(X) ->
    {X}.

first_label() ->
    new_label({label, 99}).

new_label({label, Prev}) ->
    {label, Prev + 1}.

rv() ->
    {temp, 0}.

fp() ->
    {temp, 1}.

first_temp() ->
    % Skip return and frame temps.
    new_temp({temp, 1}).

new_temp({temp, Prev}) ->
    {temp, Prev + 1}.

translate_unop({_Meta, Op, Rhs}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Rhs, Env0, Ret0),
    Instrs2 =
    [
        emit(unop)
    ],
    {Env1, Instrs1++Instrs2, Ret1}.

translate_ident({_Meta, Name}, Env0, Ret0) ->
    Instrs =
    [
        emit(ident)
    ],
    {Env0, Instrs, Ret0}.

translate_charconst({_Meta, Value}, Env0, Ret0) ->
    Int = ?RTL:char_to_int(Value),
    Ret1 = new_temp(Ret0),
    Instrs =
    [
        emit_intconst(Ret1, Int)
    ],
    {Env0, Instrs, Ret1}.

translate_funcall({_Meta, Name, Actuals}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_actuals(Actuals, Env0, Ret0),
    Instrs2 =
    [
        emit(funcall)
    ],
    {Env1, Instrs1++Instrs2, Ret1}.

translate_actuals([], Env0, Ret0) ->
    {Env0, [], Ret0};
translate_actuals([Actual|Actuals], Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_actual(Actual, Env0, Ret0),
    {Env2, Instrs2, Ret2} = translate_actuals(Actuals, Env1, Ret1),
    {Env2, Instrs1++Instrs2, Ret2}.

translate_actual(Actual, Env0, Ret0) ->
    translate_expr(Actual, Env0, Ret0).

translate_arrelem({_Meta, Name, Index}, Env0, Ret0) ->
    {Env1, Instrs1, Ret1} = translate_expr(Index, Env0, Ret0),
    Instrs2 =
    [
        emit(arrelem)
    ],
    {Env1, Instrs1++Instrs2, Ret1}.
