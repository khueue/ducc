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
translate_topdecs([Topdec|Topdecs], Env0) ->
    Tag = ?HELPER:get_tag(Topdec),
    case Tag of
        fundec ->
            translate_topdecs(Topdecs, Env0);
        _Other ->
            {Env1, ToplevelStruct} = translate_topdec(Topdec, Env0),
            [ToplevelStruct|translate_topdecs(Topdecs, Env1)]
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
        scalardec -> translate_global_scalardec(Topdec, Env0);
        arraydec  -> translate_global_arraydec(Topdec, Env0);
        fundef    -> translate_fundef(Topdec, Env0)
    end.

translate_global_scalardec({_Meta, Type, Name}, Env0) ->
    {Env1, Label} = ?ENV:get_new_label(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = {global, Label, scalar, {Size}},
    Env2 = ?ENV:set_symbol(Name, SymbolInfo, Env1),
    {Env2, toplevel_data(Label, ?HELPER:ducc_byte_size(Size))}.

translate_global_arraydec({_Meta, Type, Name, Count}, Env0) ->
    {Env1, Label} = ?ENV:get_new_label(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = {global, Label, array, {Size, Count}},
    Env2 = ?ENV:set_symbol(Name, SymbolInfo, Env1),
    {Env2, toplevel_data(Label, Count*?HELPER:ducc_byte_size(Size))}.

translate_local_scalardec({_Meta, Type, Name}, Env0) ->
    {Env1, Temp} = ?ENV:get_new_temp(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = {local, Temp, scalar, {Size}},
    Env2 = ?ENV:set_symbol(Name, SymbolInfo, Env1),
    {Env2, [], [Temp]}.

translate_local_arraydec({_Meta, Type, Name, Count}, Env0) ->
    Offset = ?ENV:get_frame_size(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = {local, stack, array, {Size, Count, Offset}},
    Env1 = ?ENV:set_symbol(Name, SymbolInfo, Env0),
    Bytes = ?HELPER:ducc_byte_size(Size),
    Env2 = ?ENV:increment_frame_size(Env1, Bytes*Count),
    {Env2, [], []}.

translate_fundef({_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    {Env1, LabelEnd} = ?ENV:get_new_label(Env0),
    Env2 = ?ENV:enter_scope(Name, Env1),
    LabelStart = {label, Name},
    Env3 = ?ENV:set_function_labels(Env2, LabelStart, LabelEnd),
    {Env4, InsFormals, TempsFormals} = translate_formals(Formals, Env3),
    {Env5, InsLocals, TempsLocals} = translate_locals(Locals, Env4),
    {Env6, InsStmts, TempsStmts} = translate_stmts(Stmts, Env5),
    FrameSize = ?ENV:get_frame_size(Env6),
    Instructions =
        InsFormals ++
        InsLocals ++
        InsStmts,
    TempsUsed =
        TempsLocals ++
        TempsStmts,
    Proc =
        toplevel_proc(
            LabelStart,
            TempsFormals,
            lists:usort(TempsUsed),
            FrameSize,
            Instructions,
            LabelEnd),
    {Env6, Proc}. % xxxx wrong scope?!?! but what about labels?

translate_formals(Formals, Env0) ->
    Translator = fun(Node, Env) -> translate_formal(Node, Env) end,
    translate_list(Formals, Translator, Env0).

translate_formal(Formal, Env0) ->
    Tag = ?HELPER:get_tag(Formal),
    case Tag of
        scalardec -> translate_local_scalardec(Formal, Env0);
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
        scalardec -> translate_local_scalardec(Local, Env0);
        arraydec  -> translate_local_arraydec(Local, Env0)
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
    {_LabelStart, LabelEnd} = ?ENV:get_function_labels(Env0),
    Instructions =
        [emit_jump(LabelEnd)],
    Temps =
        [],
    {Env0, Instructions, Temps};
translate_return({_Meta, Expr}, Env0) ->
    {Env1, InsExpr, TempsExpr} = translate_expr(Expr, Env0),
    {_LabelStart, LabelEnd} = ?ENV:get_function_labels(Env1),
    Instructions =
        InsExpr ++
        [emit_jump(LabelEnd)],
    Temps =
        TempsExpr,
    {Env1, Instructions, Temps}.

translate_while({_Meta, Cond, Stmt}, Env0) ->
    {Env1, [LabelTest,LabelBody,LabelEnd]} = ?ENV:get_new_labels(3, Env0),
    {Env2, InsCond, TempsCond} = translate_expr(Cond, Env1),
    {Env3, InsStmt, TempsStmt} = translate_stmt(Stmt, Env2),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        [emit_jump(LabelTest)] ++
        [emit_labdef(LabelBody)] ++
        InsStmt ++
        [emit_labdef(LabelTest)] ++
        InsCond ++
        [emit_cjump(neq, RetCond, 0, LabelBody)] ++
        [emit_labdef(LabelEnd)],
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
        [emit_cjump(eq, RetCond, 0, LabelElse)] ++
        InsThen ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelElse)] ++
        InsElse ++
        [emit_labdef(LabelEnd)],
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
    TempResultLhs = ?HELPER:get_return_temp(TempsLhs),
    TempResultRhs = ?HELPER:get_return_temp(TempsRhs),
    {Env3, [TempResult]} = ?ENV:get_new_temps(1, Env2),
    Instructions =
        InsLhs ++
        InsRhs ++
        [emit_eval(TempResult, rtl_binop(Op, TempResultLhs, TempResultRhs))],
    Temps =
        TempsLhs ++
        TempsRhs ++
        [TempResult],
    {Env3, Instructions, Temps}.

translate_unop({_Meta, _Op, Rhs}, Env0) ->
    {Env1, InsRhs, TempsRhs} = translate_expr(Rhs, Env0),
    Instructions =
        InsRhs ++
        [emit(unop)],
    {Env1, Instructions, TempsRhs}.

translate_intconst({_Meta, Value}, Env0) ->
    {Env1, Temps=[ReturnTemp]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_eval(ReturnTemp, rtl_icon(Value))],
    {Env1, Instructions, Temps}.

translate_ident(Node={_Meta, Name}, Env0) ->
    SymTabNode = {Scope, _, Type, _Data} = ?ENV:lookup(Name, Node, Env0),
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

translate_local_array({local, stack, array, {_Type,_Count,Offset}}, Env0) ->
    TempFP = ?ENV:get_fp(),
    {Env1, Temps=[TempOffset,TempAddress]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
    [
        emit_eval(TempOffset, rtl_icon(Offset)),
        emit_eval(TempAddress, rtl_binop('+', TempFP, TempOffset))
    ],
    {Env1, Instructions, Temps}.

translate_local_scalar({local, Temp, scalar, {_Type}}, Env0) ->
    Instructions =
    [
    ],
    {Env0, Instructions, [Temp]}.

translate_global_scalar({global, Label, scalar, {Type}}, Env0) ->
    {Env1, Temps=[TempAddress,TempValue]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
    [
        emit_eval(TempAddress, rtl_labref(Label)),
        emit_load(Type, TempValue, TempAddress)
    ],
    {Env1, Instructions, Temps}.

translate_global_array({global, Label, array, {_Type,_Count}}, Env0) ->
    {Env1, Temps=[TempAddress]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
    [
        emit_eval(TempAddress, rtl_labref(Label))
    ],
    {Env1, Instructions, Temps}.

translate_charconst(Node = {_Meta, _Value}, Env0) ->
    translate_intconst(Node, Env0).

% xxx Needs serious fixing.
translate_funcall({_Meta, Name, Actuals}, Env0) ->
    TranslatedActuals = translate_actuals(Actuals, Env0),
    ResultsActuals = ?HELPER:arg_list(TranslatedActuals),
    InsActuals = ?HELPER:combine_instrs(TranslatedActuals),
    TempsActuals = ?HELPER:combine_temps(TranslatedActuals),
    Env1 = case TranslatedActuals of
        [] ->
            Env0;
        _  ->
            {EnvX, _Instrs, _Temps} = lists:last(TranslatedActuals),
            EnvX
    end,
    {Env2, [RetTemp]} = ?ENV:get_new_temps(1, Env1),
    Instructions =
        InsActuals ++
        [emit_call(RetTemp, {label,Name}, ResultsActuals)],
    Temps =
        TempsActuals ++
        [RetTemp],
    {Env2, Instructions, Temps}.

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

    % exprs:
    %TEMP temp
    %ICON i
    %LABREF label
    %UNARY unop src %%% removed
    %BINARY binop src1 src2

rtl_temp(Temp) ->
    Temp. % ??? xxx

rtl_icon(Int) ->
    {icon, Int}.

rtl_labref(Label) ->
    {labref, Label}.

rtl_binop(Op, TempLhs, TempRhs) ->
    {binop, Op, TempLhs, TempRhs}.

    % ins:
    %LABDEF label
    %JUMP label
    %CJUMP relop src1 src2 label
    %STORE ty dst src
    %LOAD ty dst src %%% added
    %EVAL dst exp
    %CALL dst label (temp list)

emit_labdef(Label) ->
    {labdef, Label}.

emit_jump(Label) ->
    {jump, Label}.

emit_cjump(Relop, TempLhs, TempRhs, Label) ->
    {cjump, Relop, TempLhs, TempRhs, Label}.

emit_store(Type, TempDestAddress, TempValue) ->
    {store, Type, TempDestAddress, TempValue}.

emit_load(Type, TempDest, TempSourceAddress) ->
    {load, Type, TempDest, TempSourceAddress}.

emit_eval(TempResult, RtlExpr) ->
    {eval, TempResult, RtlExpr}.

emit_call(TempResult, Label, TempsActuals) ->
    {call, TempResult, Label, TempsActuals}.

emit(X) ->
    {X}.

toplevel_data(Label, Bytes) ->
    {data, Label, Bytes}.

toplevel_proc(LabelStart, Formals, Temps, FS, Ins, LabelEnd) ->
    {proc, LabelStart, Formals, Temps, FS, Ins, {labdef, LabelEnd}}. % xxx ???
