-module(codegen).
-export([generate_code/2]).

% -define(HELPER, codegen_helpers).
-define(ENV, codegen_env).

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
        asm_segment_data() ++
        asm_align(4) ++
        asm_labdef(Label) ++
        asm_space(Bytes),
    Instructions.

translate_proc(Proc = {proc, Label, Formals, Locals, ArraysSize, Ins, LabelEnd}) ->
    FS = calc_frame_size(Proc),
    Env = ?ENV:new(nil, Formals, Locals), % xxxxxxxxx
    Instructions =
        asm_segment_text() ++
        asm_globl(Label) ++
        asm_labdef(Label) ++
        asm_prologue(FS, ArraysSize) ++
        translate_instructions(Ins, Env) ++
        asm_epilogue(FS, ArraysSize, LabelEnd),
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
        asm_labdef(Label),
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
        asm_lw(t0, Offset1, Reg1) ++
        asm_sw(t0, Offset2, Reg2),
    {Env2, Instructions}.

translate_load_byte(_,_) -> % xxxxxxxxxxxx
    {nil,[]}.

translate_store_long({store, long, TempDstAddress, TempValue}, Env0) ->
    {{Reg1,Offset1},Env1} = ?ENV:lookup(TempDstAddress, Env0),
    {{Reg2,Offset2},Env2} = ?ENV:lookup(TempValue, Env1),
    Instructions =
        asm_lw(t0, Offset1, Reg1) ++
        asm_lw(t1, Offset2, Reg2) ++
        asm_sw(t1, 0, t0),
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
        asm_li(t0, Value) ++
        asm_sw(t0, Offset, sp),
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env0) ->
    {{BaseRegSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseRegDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
        asm_lw(t0, OffsetSrc, BaseRegSrc) ++
        asm_sw(t0, OffsetDst, BaseRegDst),
    {Env2, Instructions}.

translate_eval_labref(TempDst, {labref,Label}, Env0) ->
    {{Reg,Offset},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
        asm_la(t0, Label) ++
        asm_sw(t0, Offset, Reg),
    {Env1, Instructions}.

translate_eval_binop(TempDst, {binop,Op,Lhs,Rhs}, Env) ->
    % XXX Only arithmetics in binop?
    translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env).

translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env0) ->
    {{RegLhs,OffsetLhs},Env1} = ?ENV:lookup(Lhs, Env0),
    {{RegRhs,OffsetRhs},Env2} = ?ENV:lookup(Rhs, Env1),
    {{RegDst,OffsetDst},Env3} = ?ENV:lookup(TempDst, Env2),
    Fun = asm_fun(Op),
    Instructions =
        asm_lw(t0, OffsetLhs, RegLhs) ++
        asm_lw(t1, OffsetRhs, RegRhs) ++
        Fun(t2, t0, t1) ++
        asm_sw(t2, OffsetDst, RegDst),
    {Env3, Instructions}.

asm_fun('+')  -> fun asm_add/3;
asm_fun('-')  -> fun asm_sub/3;
asm_fun('*')  -> fun asm_mul/3;
asm_fun('/')  -> fun asm_div/3;
asm_fun('<')  -> fun asm_slt/3;
asm_fun('<=') -> fun asm_sle/3;
asm_fun('>')  -> fun asm_sgt/3;
asm_fun('>=') -> fun asm_sge/3;
asm_fun('==') -> fun asm_seq/3;
asm_fun('!=') -> fun asm_sne/3.

calc_frame_size({proc, _Label, _Formals, Temps, ArraysSize, _Ins, _LabelEnd}) ->
    4 + 4 + ArraysSize + erlang:length(Temps)*4.

asm_prologue(FS, ArraysSize) ->
    asm_subu(sp, sp, FS) ++
    asm_sw(fp, FS-ArraysSize-4, sp) ++
    asm_sw(ra, FS-ArraysSize-8, sp) ++
    asm_addu(fp, sp, FS).

asm_epilogue(FS, ArraysSize, LabelEnd) ->
    asm_labdef(LabelEnd) ++
    asm_lw(ra, FS-ArraysSize-8, sp) ++
    asm_lw(fp, FS-ArraysSize-4, sp) ++
    asm_addu(sp, sp, FS) ++
    asm_jr(ra).

asm_segment_data() ->
    [{segment, data}].

asm_segment_text() ->
    [{segment, text}].

asm_align(Bytes) ->
    [{align, Bytes}].

asm_globl(Label) ->
    [{globl, Label}].

asm_labdef(Label) ->
    [{labdef, Label}].

asm_space(Bytes) ->
    [{space, Bytes}].

asm_subu(Dst, Src1, Src2) ->
    [{subu, Dst, Src1, Src2}].

asm_slt(Dst, Src1, Src2) ->
    [{slt, Dst, Src1, Src2}].

asm_sle(Dst, Src1, Src2) ->
    [{sle, Dst, Src1, Src2}].

asm_sgt(Dst, Src1, Src2) ->
    [{sgt, Dst, Src1, Src2}].

asm_sge(Dst, Src1, Src2) ->
    [{sge, Dst, Src1, Src2}].

asm_seq(Dst, Src1, Src2) ->
    [{seq, Dst, Src1, Src2}].

asm_sne(Dst, Src1, Src2) ->
    [{sne, Dst, Src1, Src2}].

asm_sub(Dst, Src1, Src2) ->
    [{sub, Dst, Src1, Src2}].

asm_la(Dst, Label) ->
    [{la, Dst, Label}].

asm_sw(Src, Offset, Dst) ->
    [{sw, Src, Offset, Dst}].

asm_lw(Dst, Offset, Src) ->
    [{lw, Dst, Offset, Src}].

%asm_j(Label) ->
%    [{j, Label}].

asm_jr(Dst) ->
    [{jr, Dst}].

%asm_beqz(Rsrc, Label) ->
%    [{beqz, Rsrc, Label}].

asm_li(Dst, Value) ->
    [{li, Dst, Value}].

asm_mul(Dst, Src1, Src2) ->
    [{mul, Dst, Src1, Src2}].

asm_div(Dst, Src1, Src2) ->
    [{'div', Dst, Src1, Src2}].

asm_add(Dst, Src1, Src2) ->
    [{add, Dst, Src1, Src2}].

%asm_addi(Dst, Src1, Value) ->
%    [{addi, Dst, Src1, Value}].

asm_addu(Dst, Src1, Src2) ->
    [{addu, Dst, Src1, Src2}].
