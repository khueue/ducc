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

translate_data({data, Label, Bytes}) ->
    Instructions =
        ?ASM:asm_segment_data() ++
        ?ASM:asm_align(4) ++
        ?ASM:asm_labdef(Label) ++
        ?ASM:asm_space(Bytes),
    Instructions.

translate_proc(Proc = {proc, Label, Formals, Locals, ArraysSize, Ins, LabelEnd}) ->
    FS = ?HELPER:calc_frame_size(Proc),
    Env = ?ENV:new(nil, Formals, Locals), % xxxxxxxxx
    Instructions =
        ?ASM:asm_segment_text() ++
        ?ASM:asm_globl(Label) ++
        ?ASM:asm_labdef(Label) ++
        ?ASM:asm_prologue(FS, ArraysSize) ++
        translate_instructions(Ins, Env) ++
        ?ASM:asm_epilogue(FS, ArraysSize, LabelEnd),
    Instructions.

translate_instructions([], _Env) -> [];
translate_instructions([{'- SOURCE -',_,_,_}|Ins], Env) ->
    translate_instructions(Ins, Env);
translate_instructions([In|Ins], Env0) ->
    {Env1,Ins1} = translate_instruction(In, Env0),
    Ins1 ++ translate_instructions(Ins, Env1).

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
        ?ASM:asm_labdef(Label),
    {Env, Instructions}.

translate_load(Instr, Env) ->
    Size = erlang:element(2, Instr),
    case Size of
        long -> translate_load_long(Instr, Env);
        byte -> translate_load_byte(Instr, Env)
    end.

translate_store(Instr, Env) ->
    Size = erlang:element(2, Instr),
    case Size of
        long -> translate_store_long(Instr, Env);
        byte -> translate_store_byte(Instr, Env)
    end.

translate_load_long({load, long, TempDst, TempSrcAddress}, Env0) ->
    {{Reg1,Offset1},Env1} = ?ENV:lookup(TempSrcAddress, Env0),
    {{Reg2,Offset2},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
        ?ASM:asm_lw(t0, Offset1, Reg1) ++
        ?ASM:asm_sw(t0, Offset2, Reg2),
    {Env2, Instructions}.

translate_load_byte(_,_) -> % xxxxxxxxxxxx
    {nil,[]}.

translate_store_long({store, long, TempDstAddress, TempValue}, Env0) ->
    {{Reg1,Offset1},Env1} = ?ENV:lookup(TempDstAddress, Env0),
    {{Reg2,Offset2},Env2} = ?ENV:lookup(TempValue, Env1),
    Instructions =
        ?ASM:asm_lw(t0, Offset1, Reg1) ++
        ?ASM:asm_lw(t1, Offset2, Reg2) ++
        ?ASM:asm_sw(t1, 0, t0),
    {Env2, Instructions}.

translate_store_byte(_,_) -> % xxxxxxxxxxxx
    {nil,[]}.

translate_eval({eval, Temp, RtlExpr}, Env) ->
    Type = erlang:element(1, RtlExpr),
    case Type of
        icon   -> translate_eval_icon(Temp, RtlExpr, Env);
        temp   -> translate_eval_temp(Temp, RtlExpr, Env);
        labref -> translate_eval_labref(Temp, RtlExpr, Env);
        binop  -> translate_eval_binop(Temp, RtlExpr, Env)
    end.

translate_eval_icon(Temp, {icon,Value}, Env0) ->
    {{sp,Offset},Env1} = ?ENV:lookup(Temp, Env0),
    Instructions =
        ?ASM:asm_li(t0, Value) ++
        ?ASM:asm_sw(t0, Offset, sp),
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env0) ->
    {{BaseRegSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseRegDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
        ?ASM:asm_lw(t0, OffsetSrc, BaseRegSrc) ++
        ?ASM:asm_sw(t0, OffsetDst, BaseRegDst),
    {Env2, Instructions}.

translate_eval_labref(TempDst, {labref,Label}, Env0) ->
    {{Reg,Offset},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
        ?ASM:asm_la(t0, Label) ++
        ?ASM:asm_sw(t0, Offset, Reg),
    {Env1, Instructions}.

translate_eval_binop(TempDst, {binop,Op,Lhs,Rhs}, Env) ->
    % XXX Only arithmetics in binop?
    translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env).

translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env0) ->
    {{RegLhs,OffsetLhs},Env1} = ?ENV:lookup(Lhs, Env0),
    {{RegRhs,OffsetRhs},Env2} = ?ENV:lookup(Rhs, Env1),
    {{RegDst,OffsetDst},Env3} = ?ENV:lookup(TempDst, Env2),
    Fun = ?HELPER:asm_fun(Op),
    Instructions =
        ?ASM:asm_lw(t0, OffsetLhs, RegLhs) ++
        ?ASM:asm_lw(t1, OffsetRhs, RegRhs) ++
        Fun(t2, t0, t1) ++
        ?ASM:asm_sw(t2, OffsetDst, RegDst),
    {Env3, Instructions}.
