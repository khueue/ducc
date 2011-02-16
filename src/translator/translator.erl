-module(translator).
-export([translate/1]).

-define(HELPER, translator_helpers).
-define(ENV, translator_env).

% - Top-level structs:
% {data, {label,"minarray"}, Size}
% {proc, {label,"minfunc"}, FormalTemps, LocalTemps, FrameSize, Instructions}

% - SymTab stuff:
% {local,{temp,123}},    array, {Type, Count, Offset}
% {local,{temp,123}},   scalar, {Type}
% {global,{label,321}},  array, {Type, Count}
% {global,{label,321}}, scalar, {Type}

translate(ParseTree) ->
    Env = ?ENV:new(),
    translate(ParseTree, Env).

translate(ParseTree, Env0) ->
    translate_program(ParseTree, Env0).

translate_program({_Meta, _File, Topdecs}, Env0) ->
    translate_topdecs(Topdecs, Env0).

translate_topdecs([], _Env0) -> [];
translate_topdecs([T|Ts], Env0) ->
    Tag = ?HELPER:get_tag(T),
    case Tag of
        fundec ->
            translate_topdecs(Ts, Env0);
        _Other ->
            {Env1, Topdec, _Temps} = translate_topdec(T, Env0),
            [Topdec|translate_topdecs(Ts, Env1)]
    end.

translate_list([], _Translator, Env0) ->
    {Env0, [], []};
translate_list([Node|Nodes], Translator, Env0) ->
    {Env1, Instrs1, Temps1} = Translator(Node, Env0),
    {Env2, Instrs2, Temps2} = translate_list(Nodes, Translator, Env1),
    {Env2, Instrs1++Instrs2, Temps1++Temps2}.

translate_topdec(Topdec, Env0) ->
    Tag = ?HELPER:get_tag(Topdec),
    case Tag of
        scalardec -> translate_scalardec(Topdec, Env0);
        arraydec  -> translate_arraydec(Topdec, Env0);
        fundec    -> translate_fundec(Topdec, Env0);
        fundef    -> translate_fundef(Topdec, Env0)
    end.

translate_scalardec({_Meta, Type, Name}, Env0) ->
    {Env1, Location = {Scope, TempOrLabel}} = ?HELPER:assign_scalar_location(Env0),
    Data = {Size} = ?HELPER:create_scalar_data(Location, Type),
    Env2 = ?ENV:set_symbol(Name, {Location, scalar, Data}, Env1),
    case Scope of
        global ->
            {Env2, {data, TempOrLabel, Size}, [TempOrLabel]};
        local ->
            {Env2, [], [TempOrLabel]}
    end.

translate_arraydec({_Meta, Type, Name, Count}, Env0) ->
    {Env1, Location = {Scope, LabelOrStack}} = ?HELPER:assign_array_location(Env0),
    Data = ?HELPER:create_array_data(Env1, Location, Type, Count),
    Env2 = case Data of
        {Size1, Count1, _Offset} ->
            Bytes1 = ?HELPER:ducc_byte_size(Size1),
            ?ENV:increment_frame_size(Env1, Bytes1*Count1);
        _ ->
            Env1
    end,
    Env3 = ?ENV:set_symbol(Name, {Location, array, Data}, Env2),
    case Scope of
        global ->
            {Size2, Count2} = Data,
            Bytes2 = ?HELPER:ducc_byte_size(Size2),
            {Env3, {data, LabelOrStack, Bytes2*Count2}, []};
        local ->
            %% No temp for arrays. Use virtual stack frame.
            {Env3, [], []}
    end.

translate_fundec({_Meta, _Type, _Name, _Formals}, Env0) ->
    {Env0, [], []}.

translate_fundef({_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    Env1 = ?ENV:enter_scope(Name, Env0),
    {Env2, EndLabel}  = ?ENV:get_new_label(Env1),
    StartLabel = {label, Name},
    Env3 = ?ENV:set_labels(Env2, StartLabel, EndLabel),
    {Env4, Instrs4, FormalTemps} = translate_formals(Formals, Env3),
    {Env5, Instrs5, LocalTemps} = translate_locals(Locals, Env4),
    {Env6, Instrs6, StmtTemps} = translate_stmts(Stmts, Env5),
    FrameSize = ?ENV:get_frame_size(Env6),
    {Env6, {proc, StartLabel, FormalTemps, LocalTemps++StmtTemps, FrameSize,
            Instrs4++Instrs5++Instrs6, {labdef, EndLabel}}, []}.

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
    {Env1, Location = {_Scope, Temp}} = ?HELPER:assign_scalar_location(Env0),
    Data = ?HELPER:create_scalar_data(Location, Type),
    Env2 = ?ENV:set_symbol(Name, {Location, farray, Data}, Env1),
    {Env2, [], [Temp]}.

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

translate_return({_Meta, nil}, Env0) ->
    {_StartLabel, EndLabel} = ?ENV:get_labels(Env0),
    Instrs = [{jump, EndLabel}],
    {Env0, Instrs, []};
translate_return({_Meta, Expr}, Env0) ->
    {Env1, Instrs1, Temps1} = translate_expr(Expr, Env0),
    {_StartLabel, EndLabel} = ?ENV:get_labels(Env1),
    Instrs2 = [{jump, EndLabel}],
    {Env1, Instrs1++Instrs2, Temps1}.

translate_while({_Meta, Cond, Stmt}, Env0) ->
    {Env1, LabelTest} = ?ENV:get_new_label(Env0),
    {Env2, LabelBody} = ?ENV:get_new_label(Env1),
    {Env3, LabelEnd} = ?ENV:get_new_label(Env2),
    {Env4, InstrsCond, TempsCond} = translate_expr(Cond, Env3),
    {Env5, InstrsStmt, TempsStmt} = translate_stmt(Stmt, Env4),
    RetCond = ?ENV:get_current_temp(Env4),
    Instrs =
        [{jump, LabelTest}] ++
        [{labdef, LabelBody}] ++
        InstrsStmt ++
        [{labdef, LabelTest}] ++
        InstrsCond ++
        [{cjump, neq, RetCond, 0, LabelBody}] ++
        [{labdef, LabelEnd}],
    {Env5, Instrs, TempsCond++TempsStmt}.

translate_if({_Meta, Cond, Then, Else}, Env0) ->
    {Env1, LabelElse} = ?ENV:get_new_label(Env0),
    {Env2, LabelEnd} = ?ENV:get_new_label(Env1),
    {EnvCond, InstrsCond, TempsCond} = translate_expr(Cond, Env2),
    {EnvThen, InstrsThen, TempsThen} = translate_stmt(Then, EnvCond),
    {EnvElse, InstrsElse, TempsElse} = translate_stmt(Else, EnvThen),
    RetCond = ?ENV:get_current_temp(EnvCond),
    Instrs =
        InstrsCond ++
        [{cjump, eq, RetCond, 0, LabelElse}] ++
        InstrsThen ++
        [{jump, LabelEnd}] ++
        [{labdef, LabelElse}] ++
        InstrsElse ++
        [{labdef, LabelEnd}],
    {EnvElse, Instrs, TempsCond++TempsThen++TempsElse}.

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
    {Env1, Instrs1, Temps1} = translate_expr(Lhs, Env0),
    LhsTemp = ?ENV:get_current_temp(Env1),
    {Env2, Instrs2, Temps2} = translate_expr(Rhs, Env1),
    RhsTemp = ?ENV:get_current_temp(Env2),
    {Env3, Instrs3, Temps3} = translate_eval(Op, Env2, LhsTemp, RhsTemp),
    {Env3, Instrs1++Instrs2++Instrs3, Temps1++Temps2++Temps3}.

translate_intconst({_Meta, Value}, Env0) ->
    {Env1, ReturnTemp} = ?ENV:get_new_temp(Env0),
    Instrs =
    [
        emit_intconst(ReturnTemp, Value)
    ],
    {Env1, Instrs, [ReturnTemp]}.

translate_eval(Op, Env0, RetLhs, RetRhs) ->
    {Env1, ReturnTemp} = ?ENV:get_new_temp(Env0),
    Instrs =
    [
        emit_eval(Op, ReturnTemp, RetLhs, RetRhs)
    ],
    {Env1, Instrs, [ReturnTemp]}.

translate_unop({_Meta, _Op, Rhs}, Env0) ->
    {Env1, Instrs1, TempsRhs} = translate_expr(Rhs, Env0),
    Instrs2 =
    [
        emit(unop)
    ],
    {Env1, Instrs1++Instrs2, TempsRhs}.

translate_ident({_Meta, Name}, Env0) ->
    {Location, Type, Data} = ?ENV:lookup(Name, Env0),
    
    Instrs = case Location of
        {global, Label} -> 
            case Type of
                array -> ;
                scalar ->
            end;
        {local, Temp} -> 
    end,

    Instrs =
    [
        emit(ident)
    ],
    {Env0, Instrs, []}.

translate_charconst(Node = {_Meta, _Value}, Env0) ->
    translate_intconst(Node, Env0).

translate_funcall({_Meta, Name, Actuals}, Env0) ->
    ActualInstrs = translate_actuals(Actuals, Env0),
    ArgTemps = ?HELPER:arg_list(ActualInstrs),
    ArgInstrs = ?HELPER:conc_instrs(ActualInstrs),
    Env1 = case ActualInstrs of
        [] -> Env0;
        _  -> {EnvX, _Instrs, _Temps} = lists:last(ActualInstrs),
              EnvX
    end,
    {Env2, RetTemp} = ?ENV:get_new_temp(Env1),
    Instrs =
        ArgInstrs ++
        [{call, RetTemp, {label, Name}, ArgTemps}],
    {Env2, Instrs, get_temps(ActualInstrs)++[RetTemp]}.

get_temps([]) -> [];
get_temps([{_E,_I,T}|Xs]) ->
    T++get_temps(Xs).

translate_actuals([], _Env0) -> [];
translate_actuals([A|As], Env0) ->
    {Env1, Instrs1, Temps1} = translate_actual(A, Env0),
    [{Env1, Instrs1, Temps1}|translate_actuals(As, Env1)].

translate_actual(Actual, Env0) ->
    translate_expr(Actual, Env0).

translate_arrelem({_Meta, _Name, Index}, Env0) ->
    {Env1, Instrs1, TempsIndex} = translate_expr(Index, Env0),
    Instrs2 =
    [
        emit(arrelem)
    ],
    {Env1, Instrs1++Instrs2, TempsIndex}.

emit_intconst(_TempRet, Value) ->
    {icon, Value}.

emit_eval(Op, TempRet, TempLhs, TempRhs) ->
    {eval, TempRet, {Op, TempLhs, TempRhs}}.

emit(X) ->
    {X}.
