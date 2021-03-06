-module(translator).
-export([translate/2]).

-define(HELPER, translator_helpers).
-define(ENV, translator_env).

translate(ParseTree, Lines) ->
    Env = ?ENV:new(Lines),
    translate_program(ParseTree, Env).

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
    Label = {label,Name},
    Size = ?HELPER:type_size(Type),
    SymbolInfo = symbol_global_scalar(Label, Size),
    Env1 = ?ENV:set_symbol(Name, SymbolInfo, Env0),
    {Env1, toplevel_data(Label, ?HELPER:size_of(Size))}.

translate_global_arraydec({_Meta, Type, Name, Count}, Env0) ->
    Label = {label,Name},
    Size = ?HELPER:type_size(Type),
    SymbolInfo = symbol_global_array(Label, Size),
    Env1 = ?ENV:set_symbol(Name, SymbolInfo, Env0),
    {Env1, toplevel_data(Label, Count*?HELPER:size_of(Size))}.

translate_local_scalardec({_Meta, Type, Name}, Env0) ->
    {Env1, Temp} = ?ENV:get_new_temp(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = symbol_local_scalar(Temp, Size),
    Env2 = ?ENV:set_symbol(Name, SymbolInfo, Env1),
    {Env2, [], [Temp]}.

translate_local_arraydec({_Meta, Type, Name, Count}, Env0) ->
    FS = ?ENV:get_frame_size(Env0),
    Offset = FS,
    Size = ?HELPER:type_size(Type),
    SymbolInfo = symbol_local_array(Size, Offset),
    Env1 = ?ENV:set_symbol(Name, SymbolInfo, Env0),
    Bytes = ?HELPER:size_of(Size),
    Env2 = ?ENV:increment_frame_size(Env1, ?HELPER:round4(Bytes*Count)),
    {Env2, [], []}.

translate_fundef({_Meta, _Type, Name, Formals, Locals, Stmts}, Env0) ->
    LabelStart = {label,Name},
    {Env1, LabelEnd} = ?ENV:get_new_label(Env0, Name++"_end"),
    Env2 = ?ENV:enter_scope(Name, Env1),
    % XXX Maybe we should force the fundef to set more of its env (fp)?
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
            lists:usort(TempsUsed) -- TempsFormals,
            FrameSize,
            Instructions,
            LabelEnd),
    Env7 = ?ENV:leave_scope(Env6),
    {Env7, Proc}.

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
    {Env1, Temp} = ?ENV:get_new_temp(Env0),
    Size = ?HELPER:type_size(Type),
    SymbolInfo = symbol_farray(Temp, Size),
    Env2 = ?ENV:set_symbol(Name, SymbolInfo, Env1),
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
    case lists:member(Tag, [return,while,'if']) of
        true  -> translate_control_flow_stmt(Stmt, Env0);
        false -> translate_expr(Stmt, Env0)
    end.

translate_control_flow_stmt(Stmt, Env0) ->
    Tag = ?HELPER:get_tag(Stmt),
    {Env1, Instructions, Temps} =
    case Tag of
        return -> translate_return(Stmt, Env0);
        while  -> translate_while(Stmt, Env0);
        'if'   -> translate_if(Stmt, Env0)
    end,
    SourceLineHeader = ?HELPER:source_line_header(Stmt, Env1),
    {Env1, [SourceLineHeader|Instructions], Temps}.

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
    ReturnTemp = ?HELPER:get_return_temp(TempsExpr),
    Instructions =
        InsExpr ++
        [emit_eval(?ENV:get_rv(), rtl_temp(ReturnTemp))] ++
        [emit_jump(LabelEnd)],
    Temps =
        TempsExpr,
    {Env1, Instructions, Temps}.

translate_while({_Meta, Cond, Stmt}, Env0) ->
    {Env1, LabelTest} = ?ENV:get_new_label(Env0, "while_test"),
    {Env2, LabelBody} = ?ENV:get_new_label(Env1, "while_body"),
    {Env3, LabelEnd}  = ?ENV:get_new_label(Env2, "while_end"),
    {Env4, InsCond, TempsCond} = translate_expr(Cond, Env3),
    {Env5, InsStmt, TempsStmt} = translate_stmt(Stmt, Env4),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        [emit_jump(LabelTest)] ++
        [emit_labdef(LabelBody)] ++
        InsStmt ++
        [emit_labdef(LabelTest)] ++
        InsCond ++
        [emit_cjump(neq, RetCond, rtl_icon(0), LabelBody)] ++
        [emit_labdef(LabelEnd)],
    Temps =
        TempsCond ++
        TempsStmt,
    {Env5, Instructions, Temps}.

translate_if({_Meta, Cond, Then, nil}, Env0) ->
    {Env1, LabelEnd} = ?ENV:get_new_label(Env0, "if_end"),
    {Env2, InsCond, TempsCond} = translate_expr(Cond, Env1),
    {Env3, InsThen, TempsThen} = translate_stmt(Then, Env2),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        InsCond ++
        [emit_cjump(eq, RetCond, rtl_icon(0), LabelEnd)] ++
        InsThen ++
        [emit_labdef(LabelEnd)],
    Temps =
        TempsCond ++
        TempsThen,
    {Env3, Instructions, Temps};
translate_if({_Meta, Cond, Then, Else}, Env0) ->
    {Env1, LabelElse} = ?ENV:get_new_label(Env0, "if_else"),
    {Env2, LabelEnd}  = ?ENV:get_new_label(Env1, "if_end"),
    {Env3, InsCond, TempsCond} = translate_expr(Cond, Env2),
    {Env4, InsThen, TempsThen} = translate_stmt(Then, Env3),
    {Env5, InsElse, TempsElse} = translate_stmt(Else, Env4),
    RetCond = ?HELPER:get_return_temp(TempsCond),
    Instructions =
        InsCond ++
        [emit_cjump(eq, RetCond, rtl_icon(0), LabelElse)] ++
        InsThen ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelElse)] ++
        InsElse ++
        [emit_labdef(LabelEnd)],
    Temps =
        TempsCond ++
        TempsThen ++
        TempsElse,
    {Env5, Instructions, Temps}.

translate_expr(Expr, Env0) ->
    Tag = ?HELPER:get_tag(Expr),
    {Env1, Instructions, Temps} =
    case Tag of
        binop     -> translate_binop(Expr, Env0);
        unop      -> translate_unop(Expr, Env0);
        ident     -> translate_ident(Expr, Env0);
        intconst  -> translate_intconst(Expr, Env0);
        charconst -> translate_charconst(Expr, Env0);
        funcall   -> translate_funcall(Expr, Env0);
        arrelem   -> translate_arrelem(Expr, Env0)
    end,
    SourceLineHeader = ?HELPER:source_line_header(Expr, Env1),
    {Env1, [SourceLineHeader|Instructions], Temps}.

translate_binop(Expr = {_Meta, _Lhs, Op, _Rhs}, Env0) ->
    case Op of
        '='  -> translate_assignment(Expr, Env0);
        '+'  -> translate_arithmetic(Expr, Env0);
        '-'  -> translate_arithmetic(Expr, Env0);
        '*'  -> translate_arithmetic(Expr, Env0);
        '/'  -> translate_arithmetic(Expr, Env0);
        '&&' -> translate_logical_and(Expr, Env0);
        '||' -> translate_logical_or(Expr, Env0);
        '<'  -> translate_arithmetic(Expr, Env0);
        '>'  -> translate_arithmetic(Expr, Env0);
        '<=' -> translate_arithmetic(Expr, Env0);
        '>=' -> translate_arithmetic(Expr, Env0);
        '==' -> translate_arithmetic(Expr, Env0);
        '!=' -> translate_arithmetic(Expr, Env0)
    end.

translate_logical_and({_Meta, Lhs, '&&', Rhs}, Env0) ->
    {Env1, InsLhs, TempsLhs} = translate_expr(Lhs, Env0),
    {Env2, InsRhs, TempsRhs} = translate_expr(Rhs, Env1),
    TempLhs = ?HELPER:get_return_temp(TempsLhs),
    TempRhs = ?HELPER:get_return_temp(TempsRhs),
    {Env3, LabelFalse} = ?ENV:get_new_label(Env2, "and_false"),
    {Env4, LabelEnd}   = ?ENV:get_new_label(Env3, "and_end"),
    {Env5, [TempResult]} = ?ENV:get_new_temps(1, Env4),
    ValueTrue  = rtl_icon(1),
    ValueFalse = rtl_icon(0),
    Instructions =
        InsLhs ++
        [emit_cjump(eq, TempLhs, rtl_icon(0), LabelFalse)] ++
        InsRhs ++
        [emit_cjump(eq, TempRhs, rtl_icon(0), LabelFalse)] ++
        [emit_eval(TempResult, ValueTrue)] ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelFalse)] ++
        [emit_eval(TempResult, ValueFalse)] ++
        [emit_labdef(LabelEnd)],
    Temps =
        TempsLhs ++
        TempsRhs ++
        [TempResult],
    {Env5, Instructions, Temps}.

translate_logical_or({_Meta, Lhs, '||', Rhs}, Env0) ->
    {Env1, InsLhs, TempsLhs} = translate_expr(Lhs, Env0),
    {Env2, InsRhs, TempsRhs} = translate_expr(Rhs, Env1),
    TempLhs = ?HELPER:get_return_temp(TempsLhs),
    TempRhs = ?HELPER:get_return_temp(TempsRhs),
    {Env3, LabelTrue} = ?ENV:get_new_label(Env2, "or_true"),
    {Env4, LabelEnd}  = ?ENV:get_new_label(Env3, "or_end"),
    {Env5, [TempResult]} = ?ENV:get_new_temps(1, Env4),
    ValueTrue = rtl_icon(1),
    ValueFalse = rtl_icon(0),
    Instructions =
        InsLhs ++
        [emit_cjump(neq, TempLhs, rtl_icon(0), LabelTrue)] ++
        InsRhs ++
        [emit_cjump(neq, TempRhs, rtl_icon(0), LabelTrue)] ++
        [emit_eval(TempResult, ValueFalse)] ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelTrue)] ++
        [emit_eval(TempResult, ValueTrue)] ++
        [emit_labdef(LabelEnd)],
    Temps =
        TempsLhs ++
        TempsRhs ++
        [TempResult],
    {Env5, Instructions, Temps}.

translate_assignment({_Meta, Lhs, '=', Rhs}, Env0) ->
    {Env1, InsRval, TempsRval} = translate_rval(Rhs, Env0),
    TempRhs = ?HELPER:get_return_temp(TempsRval),
    {Env2, InsLval, TempsLval} = translate_lval(Lhs, Env1, TempRhs),
    Instructions =
        InsRval ++
        InsLval,
    Temps =
        TempsRval ++
        TempsLval,
    {Env2, Instructions, Temps}.

translate_rval(Rhs, Env0) ->
    translate_expr(Rhs, Env0).

translate_lval(Lhs, Env0, TempRhs) ->
    Tag = ?HELPER:get_tag(Lhs),
    {Env1, Instructions, Temps} =
    case Tag of
        ident   -> translate_lval_ident(Lhs, Env0, TempRhs);
        arrelem -> translate_lval_arrelem(Lhs, Env0, TempRhs)
    end,
    SourceLineHeader = ?HELPER:source_line_header(Lhs, Env1),
    {Env1, [SourceLineHeader|Instructions], Temps}.

translate_lval_ident(Node = {_Meta, Name}, Env0, TempRhs) ->
    SymbolInfo = {Scope, _, _, _} = ?ENV:lookup(Name, Node, Env0),
    case Scope of
        global -> translate_lval_global_scalar(SymbolInfo, Env0, TempRhs);
        local  -> translate_lval_local_scalar(SymbolInfo, Env0, TempRhs)
    end.

translate_lval_arrelem(Node = {_Meta, Name, Index}, Env0, TempRhs) ->
    {Env1, InsIndex, TempsIndex} = translate_expr(Index, Env0),
    TempIndex = ?HELPER:get_return_temp(TempsIndex),
    SymbolInfo = {Scope, _, _, _} = ?ENV:lookup(Name, Node, Env1),
    {Env2, InsLval, TempsLval} =
    case Scope of
        global -> translate_lval_global_arrelem(SymbolInfo, Env1, TempIndex, TempRhs);
        local  -> translate_lval_local_arrelem(SymbolInfo, Env1, TempIndex, TempRhs)
    end,
    Instructions =
        InsIndex ++
        InsLval,
    Temps =
        TempsIndex ++
        TempsLval,
    {Env2, Instructions, Temps}.

translate_lval_global_arrelem({global, Label, array, {Size}}, Env0, TempIndex, TempRhs) ->
    {Env1, Temps=[TempSizeof,TempOffset,TempBaseAddr,TempAddress]} = ?ENV:get_new_temps(4, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempOffset, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempBaseAddr, rtl_labref(Label))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempBaseAddr, TempOffset))] ++
        [emit_store(Size, TempAddress, TempRhs)],
    {Env1, Instructions, Temps}.

translate_lval_local_arrelem({local, stack, array, {Size,Offset}}, Env0, TempIndex, TempRhs) ->
    {Env1, Temps=[TempSizeof,TempMult,TempFrameOffset,TempFrameAndMultOffset,TempElementAddress]} = ?ENV:get_new_temps(5, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempFrameOffset, rtl_icon(Offset))] ++
        [emit_eval(TempFrameAndMultOffset, rtl_binop('+', TempFrameOffset, TempMult))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', ?ENV:get_fp(), TempFrameAndMultOffset))] ++
        [emit_store(Size, TempElementAddress, TempRhs)],
    {Env1, Instructions, Temps};
translate_lval_local_arrelem({local, Temp, farray, {Size}}, Env0, TempIndex, TempRhs) ->
    {Env1, Temps=[TempSizeof,TempMult,TempElementAddress]} = ?ENV:get_new_temps(3, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', Temp, TempMult))] ++
        [emit_store(Size, TempElementAddress, TempRhs)],
    {Env1, Instructions, Temps}.

translate_lval_global_scalar({global, Label, scalar, {Size}}, Env0, TempRhs) ->
    {Env1, Temps=[TempAddress]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))] ++
        [emit_store(Size, TempAddress, TempRhs)],
    {Env1, Instructions, Temps}.

translate_lval_local_scalar({local, Temp, scalar, {_Size}}, Env0, TempRhs) ->
    Instructions =
        [emit_eval(Temp, rtl_temp(TempRhs))],
    Temps =
        [],
    {Env0, Instructions, Temps}.

translate_arithmetic({_Meta, Lhs, Op, Rhs}, Env0) ->
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

translate_unop(Unop = {_Meta, Op, Rhs}, Env0) ->
    Line = ?HELPER:get_line(Unop),
    Meta = {Line,binop},
    Zero = {{0,intconst}, 0}, % We fake line number 0.
    case Op of
        '-' -> translate_binop({Meta, Zero, '-', Rhs}, Env0);
        '!' -> translate_binop({Meta, Zero, '==', Rhs}, Env0)
    end.

translate_intconst({_Meta, Value}, Env0) ->
    {Env1, Temps=[ReturnTemp]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_eval(ReturnTemp, rtl_icon(Value))],
    {Env1, Instructions, Temps}.

translate_ident(Node = {_Meta, Name}, Env0) ->
    SymTabNode = {Scope, _, Tag, _} = ?ENV:lookup(Name, Node, Env0),
    case Scope of
        global ->
            case Tag of
                array  -> translate_global_array(SymTabNode, Env0);
                scalar -> translate_global_scalar(SymTabNode, Env0)
            end;
        local ->
            case Tag of
                array  -> translate_local_array(SymTabNode, Env0);
                scalar -> translate_local_scalar(SymTabNode, Env0);
                farray -> translate_farray(SymTabNode, Env0)
            end
    end.

translate_farray({local, Temp, farray, {_Size}}, Env0) ->
    Instructions = [],
    Temps = [Temp],
    {Env0, Instructions, Temps}.

translate_local_array({local, stack, array, {_Size,Offset}}, Env0) ->
    TempFP = ?ENV:get_fp(),
    {Env1, Temps=[TempOffset,TempAddress]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
        [emit_eval(TempOffset, rtl_icon(Offset))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempFP, TempOffset))],
    {Env1, Instructions, Temps}.

translate_local_scalar({local, Temp, scalar, {_Size}}, Env0) ->
    Instructions = [],
    Temps = [Temp],
    {Env0, Instructions, Temps}.

translate_global_scalar({global, Label, scalar, {Size}}, Env0) ->
    {Env1, Temps=[TempAddress,TempValue]} = ?ENV:get_new_temps(2, Env0),
    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))] ++
        [emit_load(Size, TempValue, TempAddress)],
    {Env1, Instructions, Temps}.

translate_global_array({global, Label, array, {_Size}}, Env0) ->
    {Env1, Temps=[TempAddress]} = ?ENV:get_new_temps(1, Env0),
    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))],
    {Env1, Instructions, Temps}.

translate_charconst(Node = {_Meta, _Value}, Env0) ->
    translate_intconst(Node, Env0).

% XXX Needs serious cleaning.
translate_funcall({_Meta, Name, Actuals}, Env0) ->
    TranslatedActuals = translate_actuals(Actuals, Env0),
    ResultsActuals = ?HELPER:arg_list(TranslatedActuals),
    InsActuals = ?HELPER:combine_instrs(TranslatedActuals),
    TempsActuals = ?HELPER:combine_temps(TranslatedActuals),
    % Looks like crap, but works. We only update the env if we have any actuals.
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

translate_arrelem(Node = {_Meta, Name, Index}, Env0) ->
    {Env1, InsIndex, TempsIndex} = translate_expr(Index, Env0),
    TempIndex = ?HELPER:get_return_temp(TempsIndex),
    SymbolInfo = ?ENV:lookup(Name, Node, Env1),
    {Env2, InsArrelem, TempsArrelem} = translate_rval_arrelem(SymbolInfo, Env1, TempIndex),
    Instructions =
        InsIndex ++
        InsArrelem,
    Temps =
        TempsIndex ++
        TempsArrelem,
    {Env2, Instructions, Temps}.

translate_rval_arrelem(Symbol = {Scope, _, Tag, _}, Env0, TempIndex) ->
    case Scope of
        global -> translate_rval_global_arrelem(Symbol, Env0, TempIndex);
        local ->
            case Tag of
                array  -> translate_rval_local_arrelem(Symbol, Env0, TempIndex);
                farray -> translate_rval_farrelem(Symbol, Env0, TempIndex)
            end
    end.

translate_rval_local_arrelem({local, stack, array, {Size,Offset}}, Env0, TempIndex) ->
    {Env1, Temps=[TempSizeof,TempMult,TempFrameOffset,TempFrameAndMultOffset,TempElementAddress,TempResult]} = ?ENV:get_new_temps(6, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempFrameOffset, rtl_icon(Offset))] ++
        [emit_eval(TempFrameAndMultOffset, rtl_binop('+', TempFrameOffset, TempMult))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', ?ENV:get_fp(), TempFrameAndMultOffset))] ++
        [emit_load(Size, TempResult, TempElementAddress)],
    {Env1, Instructions, Temps}.

translate_rval_global_arrelem({global, Label, array, {Size}}, Env0, TempIndex) ->
    {Env1, Temps=[TempSizeof,TempOffset,TempBaseAddr,TempAddress,TempResult]} = ?ENV:get_new_temps(5, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempOffset, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempBaseAddr, rtl_labref(Label))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempBaseAddr, TempOffset))] ++
        [emit_load(Size, TempResult, TempAddress)],
    {Env1, Instructions, Temps}.

translate_rval_farrelem({local, TempBase, farray, {Size}}, Env0, TempIndex) ->
    {Env1, Temps=[TempSizeof,TempMult,TempElementAddress,TempResult]} = ?ENV:get_new_temps(4, Env0),
    Sizeof = ?HELPER:size_of(Size),
    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', TempBase, TempMult))] ++
        [emit_load(Size, TempResult, TempElementAddress)],
    {Env1, Instructions, Temps}.

rtl_temp(Temp) ->
    Temp. % Nothing new, since we use this tuple everywhere anyway.

rtl_icon(Int) ->
    {icon, Int}.

rtl_labref(Label) ->
    {labref, Label}.

rtl_binop(Op, TempLhs, TempRhs) ->
    {binop, Op, TempLhs, TempRhs}.

emit_labdef(Label) ->
    {labdef, Label}.

emit_jump(Label) ->
    {jump, Label}.

emit_cjump(Relop, TempLhs, TempRhs, Label) ->
    {cjump, Relop, TempLhs, TempRhs, Label}.

emit_store(Size, TempDestAddress, TempValue) ->
    {store, Size, TempDestAddress, TempValue}.

emit_load(Size, TempDest, TempSourceAddress) ->
    {load, Size, TempDest, TempSourceAddress}.

emit_eval(TempResult, RtlExpr) ->
    {eval, TempResult, RtlExpr}.

emit_call(TempResult, Label, TempsActuals) ->
    {call, TempResult, Label, TempsActuals}.

toplevel_data(Label, Bytes) ->
    {data, Label, Bytes}.

toplevel_proc(LabelStart, Formals, Temps, FS, Ins, LabelEnd) ->
    {proc, LabelStart, Formals, Temps, FS, Ins, LabelEnd}.

symbol_global_scalar(Label, Size) ->
    {global, Label, scalar, {Size}}.

symbol_global_array(Label, Size) ->
    {global, Label, array, {Size}}.

symbol_local_scalar(Temp, Size) ->
    {local, Temp, scalar, {Size}}.

symbol_local_array(Size, Offset) ->
    {local, stack, array, {Size, Offset}}.

symbol_farray(Temp, Size) ->
    {local, Temp, farray, {Size}}.
