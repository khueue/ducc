-module(translator).
-export([translate/1]).
-compile(export_all).

-define(HELPER, analyzer_helpers).
-define(RULE, analyzer_rules).
-define(ENV, translator_env).
-define(RTL, rtl_constructors).

% {local,{temp,123}},    array, {Type, Count, Offset}
% {local,{temp,123}},   scalar, {Type}
% {global,{label,321}},  array, {Type, Count}
% {global,{label,321}}, scalar, {Type}

translate(ParseTree) ->
    Env = ?ENV:new(),
    % Pass last used temporary and let functions increment.
    % LastUsedTemp = fp(), % in env xxxxx
    {Env1, Instrs} = translate(ParseTree, Env),
    Instrs.

translate(ParseTree, Env0) ->
    translate_program(ParseTree, Env0).

translate_program({_Meta, _File, Topdecs}, Env0) ->
    translate_topdecs(Topdecs, Env0).

translate_topdecs(Topdecs, Env0) ->
    Translator = fun(Node, Env) -> translate_topdec(Node, Env) end,
    translate_list(Topdecs, Translator, Env0).

translate_list([], _Translator, Env0) ->
    {Env0, []};
translate_list([Node|Nodes], Translator, Env0) ->
    {Env1, Instrs1} = Translator(Node, Env0),
    {Env2, Instrs2} = translate_list(Nodes, Translator, Env1),
    {Env2, Instrs1++Instrs2}.

translate_topdec(Topdec, Env0) ->
    Tag = ?HELPER:get_tag(Topdec),
    case Tag of
        scalardec -> translate_scalardec(Topdec, Env0);
        arraydec  -> translate_arraydec(Topdec, Env0);
        fundec    -> translate_fundec(Topdec, Env0);
        fundef    -> translate_fundef(Topdec, Env0)
    end.

translate_scalardec({_Meta, Type, Name}, Env0) ->
    Location = case ?ENV:scope(Env0) of
        global ->
            {Env1, Label} = ?ENV:get_new_label(Env0),
            {global, Label};
        local ->
            {Env1, Temp} = ?ENV:get_new_temp(Env0),
            {local, Temp}
    end,
    Size = type_size(Type),
    Env2 = ?ENV:set_symbol(Name, {Location, scalar, Size}, Env1),
    Instrs =
    [
        emit({Location, scalar, Size})
    ],
    {Env2, Instrs}.

type_size(int)  -> long;
type_size(char) -> byte.

translate_arraydec({_Meta, Type, Name, Count}, Env0) ->
    Location = case ?ENV:scope(Env0) of
        global ->
            {Env1, Label} = ?ENV:get_new_label(Env0),
            {global, Label};
        local ->
            {Env1, Temp} = ?ENV:get_new_temp(Env0),
            {local, Temp}
    end,
    Size = type_size(Type),

    Instrs =
    [
        emit(arraydec)
    ],
    {Env0, Instrs}.

translate_fundec({_Meta, Type, Name, Formals}, Env0) ->
    {Env1, Instrs1} = translate_formals(Formals, Env0),
    Instrs2 =
    [
        emit(fundec)
    ],
    {Env1, Instrs1++Instrs2}.

translate_fundef({_Meta, Type, Name, Formals, Locals, Stmts}, Env0) ->
    {Env1, Instrs1} = translate_formals(Formals, Env0),
    {Env2, Instrs2} = translate_locals(Locals, Env1),
    {Env3, Instrs3} = translate_stmts(Stmts, Env2),
    Instrs4 =
    [
        emit(fundef)
    ],
    {Env3, Instrs1++Instrs2++Instrs3++Instrs4}.

translate_formals(Formals, Env0) ->
    Translator = fun(Node, Env) -> translate_formal(Node, Env) end,
    translate_list(Formals, Translator, Env0).

translate_formal(Formal, Env0) ->
    Tag = ?HELPER:get_tag(Formal),
    case Tag of
        scalardec -> translate_scalardec(Formal, Env0);
        farraydec -> translate_farraydec(Formal, Env0)
    end.

translate_farraydec({_Meta, Type, Name}, Env0) ->
    Instrs =
    [
        emit(farraydec)
    ],
    {Env0, Instrs}.

translate_locals(Locals, Env0) ->
    Translator = fun(Node, Env) -> translate_local(Node, Env) end,
    translate_list(Locals, Translator, Env0).

translate_local(Local, Env0) ->
    Tag = ?HELPER:get_tag(Local),
    case Tag of
        scalardec -> translate_scalardec(Local, Env0);
        arraydec  -> translate_arraydec(Local, Env0)
    end.

translate_stmts(Stmts, Env0) ->
    Translator = fun(Node, Env) -> translate_stmt(Node, Env) end,
    translate_list(Stmts, Translator, Env0).

translate_stmt(Stmts, Env0) when erlang:is_list(Stmts) ->
    translate_stmts(Stmts, Env0);
translate_stmt(Stmt, Env0) ->
    Tag = ?HELPER:get_tag(Stmt),
    case Tag of
        return -> translate_return(Stmt, Env0);
        while  -> translate_while(Stmt, Env0);
        'if'   -> translate_if(Stmt, Env0);
        _Expr  -> translate_expr(Stmt, Env0)
    end.

translate_return({_Meta, Expr}, Env0) ->
    {Env1, Instrs1} = translate_expr(Expr, Env0),
    Instrs2 =
    [
        emit(return)
    ],
    {Env1, Instrs1++Instrs2}.

translate_while({_Meta, Cond, Stmt}, Env0) ->
    {Env1, Instrs1} = translate_expr(Cond, Env0),
    {Env2, Instrs2} = translate_stmt(Stmt, Env1),
    Instrs3 =
    [
        emit(while)
    ],
    {Env2, Instrs1++Instrs2++Instrs3}.

translate_if({_Meta, Cond, Then, Else}, Env0) ->
    {Env1, Instrs1} = translate_expr(Cond, Env0),
    {Env2, Instrs2} = translate_stmt(Then, Env1),
    {Env3, Instrs3} = translate_stmt(Else, Env2),
    Instrs4 =
    [
        emit('if')
    ],
    {Env3, Instrs1++Instrs2++Instrs3++Instrs4}.

translate_expr(Expr, Env0) ->
    Tag = ?HELPER:get_tag(Expr),
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
    {Env1, Instrs1} = translate_expr(Lhs, Env0),
    LhsTemp = ?ENV:get_current_temp(Env1),
    {Env2, Instrs2} = translate_expr(Rhs, Env1),
    RhsTemp = ?ENV:get_current_temp(Env2),
    {Env3, Instrs3} = translate_eval(Op, Env2, LhsTemp, RhsTemp),
    {Env3, Instrs1++Instrs2++Instrs3}.

translate_intconst({_Meta, Value}, Env0) ->
    {Env1, ReturnTemp} = ?ENV:get_new_temp(Env0),
    Instrs =
    [
        emit_intconst(ReturnTemp, Value)
    ],
    {Env1, Instrs}.

translate_eval(Op, Env0, RetLhs, RetRhs) ->
    {Env1, ReturnTemp} = ?ENV:get_new_temp(Env0),
    Instrs =
    [
        emit_eval(Op, ReturnTemp, RetLhs, RetRhs)
    ],
    {Env1, Instrs}.

translate_unop({_Meta, Op, Rhs}, Env0) ->
    {Env1, Instrs1} = translate_expr(Rhs, Env0),
    Instrs2 =
    [
        emit(unop)
    ],
    {Env1, Instrs1++Instrs2}.

translate_ident({_Meta, Name}, Env0) ->
    Instrs =
    [
        emit(ident)
    ],
    {Env0, Instrs}.

translate_charconst(Node = {_Meta, _Value}, Env0) ->
    translate_intconst(Node, Env0).

translate_funcall({_Meta, Name, Actuals}, Env0) ->
    {Env1, Instrs1} = translate_actuals(Actuals, Env0),
    Instrs2 =
    [
        emit(funcall)
    ],
    {Env1, Instrs1++Instrs2}.

translate_actuals(Actuals, Env0) ->
    Translator = fun(Node, Env) -> translate_actual(Node, Env) end,
    translate_list(Actuals, Translator, Env0).

translate_actual(Actual, Env0) ->
    translate_expr(Actual, Env0).

translate_arrelem({_Meta, Name, Index}, Env0) ->
    {Env1, Instrs1} = translate_expr(Index, Env0),
    Instrs2 =
    [
        emit(arrelem)
    ],
    {Env1, Instrs1++Instrs2}.

emit_intconst(TempRet, Value) ->
    {icon, Value}.

emit_eval(Op, TempRet, TempLhs, TempRhs) ->
    {eval, TempRet, {Op, TempLhs, TempRhs}}.

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
