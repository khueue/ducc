-module(codegen).
-export([generate_code/2]).

-define(HELPER, codegen_helpers).
-define(ENV, codegen_env).
-define(ASM, codegen_asm).

generate_code(RtlCode, _Lines) ->
    AsmCode = translate_toplevels(RtlCode),
    AsmCode.

% XXX: stdio included even if not needed.
translate_toplevels([]) -> ?HELPER:setup_stdio();
translate_toplevels([Toplevel|Toplevels]) ->
    ToplevelAsm = translate_toplevel(Toplevel),
    [ToplevelAsm|translate_toplevels(Toplevels)].

translate_toplevel(Toplevel) ->
    Type = erlang:element(1, Toplevel),
    case Type of
        data -> translate_data(Toplevel);
        proc -> translate_proc(Toplevel)
    end.

translate_data({data,Label,Bytes}) ->
    Instructions =
    [
        ?ASM:asm_segment_data(),
        ?ASM:asm_align(4),
        ?ASM:asm_labdef(Label),
        ?ASM:asm_space(Bytes)
    ],
    Instructions.

translate_proc({proc,LabelStart,Formals,Locals,ArraysSize,Ins,LabelEnd}) ->
    Header = ?HELPER:setup_function_header(LabelStart),
    FS = ?HELPER:calculate_frame_size(Locals, ArraysSize),
    Prologue = ?HELPER:setup_function_prologue(FS, ArraysSize),
    Epilogue = ?HELPER:setup_function_epilogue(FS, ArraysSize, LabelEnd),
    Env = ?ENV:new(Formals, Locals, ArraysSize),
    Body = translate_instructions(Ins, Env),
    Instructions =
        Header ++
        Prologue ++
        Body ++
        Epilogue,
    Instructions.

translate_instructions([], _Env) -> [];
translate_instructions([{'- SOURCE -',_,_,_}|Ins], Env) ->
    translate_instructions(Ins, Env);
translate_instructions([In|Ins], Env0) ->
    {Env1,In1} = translate_instruction(In, Env0),
    In1 ++ translate_instructions(Ins, Env1).

translate_instruction(Instr, Env) ->
    Tag = erlang:element(1, Instr),
    case Tag of
        eval   -> translate_eval(Instr, Env);
        load   -> translate_load(Instr, Env);
        store  -> translate_store(Instr, Env);
        labdef -> translate_labdef(Instr, Env);
        jump   -> translate_jump(Instr, Env);
        cjump  -> translate_cjump(Instr, Env);
        call   -> translate_call(Instr, Env)
    end.

translate_call({call,TempRetVal,Label,TempsActuals}, Env0) ->
    NumActuals = erlang:length(TempsActuals),
    BytesForArgs = NumActuals * 4,
    InsStart =
    [
        ?ASM:asm_subu(sp, sp, BytesForArgs)
    ],
    {Env1, InsPushArgs} = push_args_on_stack(TempsActuals, BytesForArgs, Env0),
    {{BaseRetVal,OffsetRetVal},Env2} = ?ENV:lookup(TempRetVal, Env1),
    ProperOffset = compensate_offset(BaseRetVal, OffsetRetVal, BytesForArgs),
    InsStop =
    [
        ?ASM:asm_jal(Label),
        ?ASM:asm_sw(v0, ProperOffset, BaseRetVal),
        ?ASM:asm_addu(sp, sp, BytesForArgs)
    ],
    Instructions =
        InsStart ++
        InsPushArgs ++
        InsStop,
    {Env2, Instructions}.

push_args_on_stack(TempsActuals, BytesForArgs, Env) ->
    push_args_on_stack(TempsActuals, 0, BytesForArgs, Env).

push_args_on_stack([], _Index, _BytesForArgs, Env) -> {Env, []};
push_args_on_stack([TempActual|TempsActuals], Index, BytesForArgs, Env0) ->
    {{BaseActual,OffsetActual},Env1} = ?ENV:lookup(TempActual, Env0),
    ProperOffset = compensate_offset(BaseActual, OffsetActual, BytesForArgs),
    InsActual =
    [
        ?ASM:asm_lw(t0, ProperOffset, BaseActual),
        ?ASM:asm_sw(t0, Index*4, sp)
    ],
    {Env2, InsActuals} = push_args_on_stack(TempsActuals, Index+1, BytesForArgs, Env1),
    {Env2, InsActual++InsActuals}.

% Stack pointer is changed, so we need to compensate when using sp.
compensate_offset(BaseReg, Offset, BytesForArgs) ->
    case BaseReg of
        sp -> Offset + BytesForArgs;
        _  -> Offset
    end.

translate_jump({jump,Label}, Env) ->
    Instructions =
    [
        ?ASM:asm_j(Label)
    ],
    {Env, Instructions}.

% XXX: We only use {icon,0} for RHS.
translate_cjump({cjump,Comp,TempLhs,{icon,0},Label}, Env0) ->
    {{BaseLhs,OffsetLhs},Env1} = ?ENV:lookup(TempLhs, Env0),
    CjumpFun = ?HELPER:asm_cjump_fun(Comp),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetLhs, BaseLhs),
        CjumpFun(t0, Label)
    ],
    {Env1, Instructions}.

translate_labdef({labdef,Label}, Env) ->
    Instructions =
    [
        ?ASM:asm_labdef(Label)
    ],
    {Env, Instructions}.

translate_load({load,Size,TempDst,TempSrcAddr}, Env) ->
    case Size of
        long -> translate_load_long(TempDst, TempSrcAddr, Env);
        byte -> translate_load_byte(TempDst, TempSrcAddr, Env)
    end.

translate_load_long(TempDst, TempSrcAddr, Env0) ->
    {{BaseSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrcAddr, Env0),
    {{BaseDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetSrc, BaseSrc), % t0 = addr
        ?ASM:asm_lw(t1, 0, t0),              % t1 = addr[0]
        ?ASM:asm_sw(t1, OffsetDst, BaseDst)  % dst = t1
    ],
    {Env2, Instructions}.

translate_load_byte(TempDst, TempSrcAddr, Env0) ->
    {{BaseSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrcAddr, Env0),
    {{BaseDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetSrc, BaseSrc), % t0 = addr
        ?ASM:asm_lb(t1, 0, t0),              % t1 = addr[0]
        ?ASM:asm_sb(t1, OffsetDst, BaseDst)  % dst = t1
    ],
    {Env2, Instructions}.

translate_store({store,Size,TempDstAddr,TempValue}, Env) ->
    case Size of
        long -> translate_store_long(TempDstAddr, TempValue, Env);
        byte -> translate_store_byte(TempDstAddr, TempValue, Env)
    end.

translate_store_long(TempDstAddr, TempValue, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDstAddr, Env0),
    {{BaseVal,OffsetVal},Env2} = ?ENV:lookup(TempValue, Env1),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetDst, BaseDst),
        ?ASM:asm_lw(t1, OffsetVal, BaseVal),
        ?ASM:asm_sw(t1, 0, t0)
    ],
    {Env2, Instructions}.

translate_store_byte(TempDstAddr, TempValue, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDstAddr, Env0),
    {{BaseVal,OffsetVal},Env2} = ?ENV:lookup(TempValue, Env1),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetDst, BaseDst),
        ?ASM:asm_lb(t1, OffsetVal, BaseVal),
        ?ASM:asm_sb(t1, 0, t0)
    ],
    {Env2, Instructions}.

translate_eval({eval,TempDst,RtlExpr}, Env) ->
    Type = erlang:element(1, RtlExpr),
    case Type of
        icon   -> translate_eval_icon(TempDst, RtlExpr, Env);
        temp   -> translate_eval_temp(TempDst, RtlExpr, Env);
        labref -> translate_eval_labref(TempDst, RtlExpr, Env);
        binop  -> translate_eval_binop(TempDst, RtlExpr, Env)
    end.

translate_eval_icon(TempDst, {icon,Value}, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
    [
        ?ASM:asm_li(t0, Value),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env) ->
    case TempDst of
        {temp,0} -> translate_eval_temp_return(TempSrc, Env);
        _Other   -> translate_eval_temp_other(TempDst, TempSrc, Env)
    end.

translate_eval_temp_return(TempSrc, Env0) ->
    {{BaseSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetSrc, BaseSrc),
        ?ASM:asm_move(v0, t0)
    ],
    {Env1, Instructions}.

translate_eval_temp_other(TempDst, TempSrc, Env0) ->
    {{BaseSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetSrc, BaseSrc),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env2, Instructions}.

translate_eval_labref(TempDst, {labref,Label}, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
    [
        ?ASM:asm_la(t0, Label),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env1, Instructions}.

translate_eval_binop(TempDst, {binop,Op,Lhs,Rhs}, Env) ->
    case Lhs of
        {temp,1} -> translate_eval_binop_fp_offset(TempDst, Rhs, Env);
        _Other   -> translate_eval_binop_other(TempDst, Op, Lhs, Rhs, Env)
    end.

translate_eval_binop_fp_offset(TempDst, Rhs, Env0) ->
    {{BaseRhs,OffsetRhs},Env1} = ?ENV:lookup(Rhs, Env0),
    {{BaseDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    ArraysSize = ?ENV:get_arrays_size(Env2),
    Instructions =
    [
        ?ASM:asm_subu(t0, fp, ArraysSize),
        ?ASM:asm_lw(t1, OffsetRhs, BaseRhs),
        ?ASM:asm_add(t0, t0, t1),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env2, Instructions}.

translate_eval_binop_other(TempDst, Op, Lhs, Rhs, Env0) ->
    {{BaseLhs,OffsetLhs},Env1} = ?ENV:lookup(Lhs, Env0),
    {{BaseRhs,OffsetRhs},Env2} = ?ENV:lookup(Rhs, Env1),
    {{BaseDst,OffsetDst},Env3} = ?ENV:lookup(TempDst, Env2),
    BinopFun = ?HELPER:asm_binop_fun(Op),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetLhs, BaseLhs),
        ?ASM:asm_lw(t1, OffsetRhs, BaseRhs),
        BinopFun(t2, t0, t1),
        ?ASM:asm_sw(t2, OffsetDst, BaseDst)
    ],
    {Env3, Instructions}.
