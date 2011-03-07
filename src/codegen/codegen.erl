-module(codegen).
-export([generate_code/2]).

-define(HELPER, codegen_helpers).
-define(ENV, codegen_env).
-define(ASM, codegen_asm).

generate_code(RtlCode, _Lines) ->
    AsmCode = translate_toplevels(RtlCode),
    AsmCode.

translate_toplevels([]) -> [];
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

translate_proc({proc,Label,Formals,Locals,ArraysSize,Ins,LabelEnd}) ->
    FS = ?HELPER:calculate_frame_size(Locals, ArraysSize),
    Env = ?ENV:new(Formals, Locals),
    Instructions =
        [
            ?ASM:asm_segment_text(),
            ?ASM:asm_globl(Label),
            ?ASM:asm_labdef(Label)
        ] ++
        ?HELPER:function_prologue(FS, ArraysSize) ++
        translate_instructions(Ins, Env) ++
        ?HELPER:function_epilogue(FS, ArraysSize, LabelEnd),
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
        _ -> {Env,[{xxx,"--- XXX UNHANDLED: " ++ atom_to_list(Tag)}]} % xxxxxxxxxx
    end.

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
        ?ASM:asm_lw(t0, OffsetSrc, BaseSrc),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env2, Instructions}.

% xxxxxx.
translate_load_byte(_TempDst, _TempSrcAddr, Env0) ->
    {Env0, [{xxx,"--- XXX UNHANDLED: load byte"}]}.

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

% xxxxxx.
translate_store_byte(_TempDstAddr, _TempValue, Env0) ->
    {Env0, [{xxx,"--- XXX UNHANDLED: store byte"}]}.

translate_eval({eval,Temp,RtlExpr}, Env) ->
    Type = erlang:element(1, RtlExpr),
    case Type of
        icon   -> translate_eval_icon(Temp, RtlExpr, Env);
        temp   -> translate_eval_temp(Temp, RtlExpr, Env);
        labref -> translate_eval_labref(Temp, RtlExpr, Env);
        binop  -> translate_eval_binop(Temp, RtlExpr, Env)
    end.

translate_eval_icon(TempDst, {icon,Value}, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
    [
        ?ASM:asm_li(t0, Value),
        ?ASM:asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env0) ->
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

translate_eval_binop(TempDst, {binop,Op,Lhs,Rhs}, Env0) ->
    {{BaseLhs,OffsetLhs},Env1} = ?ENV:lookup(Lhs, Env0),
    {{BaseRhs,OffsetRhs},Env2} = ?ENV:lookup(Rhs, Env1),
    {{BaseDst,OffsetDst},Env3} = ?ENV:lookup(TempDst, Env2),
    Fun = ?HELPER:asm_fun(Op),
    Instructions =
    [
        ?ASM:asm_lw(t0, OffsetLhs, BaseLhs),
        ?ASM:asm_lw(t1, OffsetRhs, BaseRhs),
        Fun(t2, t0, t1),
        ?ASM:asm_sw(t2, OffsetDst, BaseDst)
    ],
    {Env3, Instructions}.
