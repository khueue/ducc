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
    {Env1, [LabelTest,LabelBody,LabelEnd]} = ?ENV:get_new_labels(3, Env0),
    {Env2, InsCond, TempsCond} = translate_expr(Cond, Env1),
    {Env3, InsStmt, TempsStmt} = translate_stmt(Stmt, Env2),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        [{jump, LabelTest}] ++
        [{labdef, LabelBody}] ++
        InsStmt ++
        [{labdef, LabelTest}] ++
        InsCond ++
        [{cjump, neq, RetCond, 0, LabelBody}] ++
        [{labdef, LabelEnd}],
    Temps =
        TempsCond ++
        TempsStmt,
    {Env3, Instructions, Temps}.

translate_if({_Meta, Cond, Then, Else}, Env0) ->
    {Env1, [LabelElse,LabelEnd]} = ?ENV:get_new_labels(2, Env0),
    {Env2, InsCond, TempsCond} = translate_expr(Cond, Env1),
    {Env3, InsThen, TempsThen} = translate_stmt(Then, Env2),
    {Env4, InsElse, TempsElse} = translate_stmt(Else, Env3),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        InsCond ++
        [{cjump, eq, RetCond, 0, LabelElse}] ++
        InsThen ++
        [{jump, LabelEnd}] ++
        [{labdef, LabelElse}] ++
        InsElse ++
        [{labdef, LabelEnd}],
    Temps =
        TempsCond ++
        TempsThen ++
        TempsElse,
    {Env4, Instructions, Temps}.

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
    {Env1, InsLhs, TempsLhs} = translate_expr(Lhs, Env0),
    {Env2, InsRhs, TempsRhs} = translate_expr(Rhs, Env1),
    LhsTemp = ?HELPER:get_return_temp(TempsLhs),
    RhsTemp = ?HELPER:get_return_temp(TempsRhs),
    {Env3, InsOp, TempsOp} = translate_eval(Op, Env2, LhsTemp, RhsTemp),
    Instructions =
        InsLhs ++
        InsRhs ++
        InsOp,
    Temps =
        TempsLhs ++
        TempsRhs ++
        TempsOp,
    {Env3, Instructions, Temps}.

translate_intconst({_Meta, Value}, Env0) ->
    {Env1, Temps=[ReturnTemp]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_intconst(ReturnTemp, Value)],
    {Env1, Instructions, Temps}.

translate_eval(Op, Env0, RetLhs, RetRhs) ->
    {Env1, Temps=[ReturnTemp]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_eval(Op, ReturnTemp, RetLhs, RetRhs)],
    {Env1, Instructions, Temps}.

translate_unop({_Meta, _Op, Rhs}, Env0) ->
    {Env1, InsRhs, TempsRhs} = translate_expr(Rhs, Env0),
    Instructions =
        InsRhs ++
        [emit(unop)],
    {Env1, Instructions, TempsRhs}.

translate_ident(Node={_Meta, Name}, Env0) ->
    SymTabNode = {{Scope,_}, Type, _Data} = ?ENV:lookup(Name, Node, Env0),
    {Env1, Instructions, Temps} =
    case Scope of
        global ->
            case Type of
                array  -> translate_global_array(SymTabNode, Env0);
                scalar -> translate_global_scalar(SymTabNode, Env0)
            end;
        local ->
            case Type of
                array  -> translate_local_array(SymTabNode, Env0);
                scalar -> translate_local_scalar(SymTabNode, Env0)
            end
    end,
    {Env1, Instructions, Temps}.

translate_local_array({{local, stack}, array, {_Type,_Count,Offset}}, Env0) ->
    TempFP = ?ENV:get_fp(),
    {Env1, Temps=[TempOffset,TempAddress]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
    [
        {eval, TempOffset, {icon, Offset}},
        {eval, TempAddress, {'+', TempFP, TempOffset}}
    ],
    {Env1, Instructions, Temps}.

translate_local_scalar({{local, Temp}, scalar, {_Type}}, Env0) ->
    Instructions =
    [
    ],
    {Env0, Instructions, [Temp]}.

translate_global_scalar({{global, Label}, scalar, {Type}}, Env0) ->
    {Env1, Temps=[TempAddress,TempValue]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
    [
        {eval, TempAddress, {labref, Label}},
        {eval, TempValue, {load, Type, TempAddress}}
    ],
    {Env1, Instructions, Temps}.

translate_global_array({{global, Label}, array, {_Type,_Count}}, Env0) ->
    {Env1, Temps=[TempAddress]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
    [
        {eval, TempAddress, {labref, Label}}
    ],
    {Env1, Instructions, Temps}.

translate_charconst(Node = {_Meta, _Value}, Env0) ->
    translate_intconst(Node, Env0).

% xxx Needs serious fixing.
translate_funcall({_Meta, Name, Actuals}, Env0) ->
    TranslatedActuals = translate_actuals(Actuals, Env0),
    ArgTemps = ?HELPER:arg_list(TranslatedActuals),
    ArgInstrs = ?HELPER:conc_instrs(TranslatedActuals),
    Env1 = case TranslatedActuals of
        [] ->
            Env0;
        _  ->
            {EnvX, _Instrs, _Temps} = lists:last(TranslatedActuals),
            EnvX
    end,
    {Env2, [RetTemp]} = ?ENV:get_new_temps(1, Env1),
    Instructions =
        ArgInstrs ++
        [{call, RetTemp, {label, Name}, ArgTemps}],
    {Env2, Instructions, get_temps(TranslatedActuals)++[RetTemp]}.

get_temps([]) -> [];
get_temps([{_E,_I,T}|Xs]) ->
    T++get_temps(Xs).

translate_actuals([], _Env0) -> [];
translate_actuals([Actual|Actuals], Env0) ->
    Translated = {Env1, _Instrs1, _Temps1} = translate_actual(Actual, Env0),
    [Translated|translate_actuals(Actuals, Env1)].

translate_actual(Actual, Env0) ->
    translate_expr(Actual, Env0).

translate_arrelem({_Meta, _Name, Index}, Env0) ->
    {Env1, InsIndex, TempsIndex} = translate_expr(Index, Env0),
    Instructions =
        InsIndex ++
        [emit(arrelem)],
    {Env1, Instructions, TempsIndex}.

emit_intconst(_TempRet, Value) ->
    {icon, Value}.

emit_eval(Op, TempRet, TempLhs, TempRhs) ->
    {eval, TempRet, {Op, TempLhs, TempRhs}}.

emit(X) ->
    {X}.
