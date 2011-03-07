-module(codegen).
-export([generate_code/2]).

% -define(HELPER, codegen_helpers).
-define(ENV, codegen_env).

generate_code(RtlCode, Lines) ->
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
        segment_data() ++
        align(4) ++
        labdef(Label) ++
        space(Bytes),
    Instructions.

translate_proc(Proc = {proc, Label, Formals, Locals, ArraysSize, Ins, LabelEnd}) ->
    FS = calc_frame_size(Proc),
    Env = ?ENV:new(nil, Formals, Locals), % xxxxxxxxx
    Instructions =
        segment_text() ++
        globl(Label) ++
        labdef(Label) ++
        prologue(FS, ArraysSize) ++
        translate_instructions(Ins, Env) ++
        epilogue(FS, ArraysSize, LabelEnd),
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
        eval  -> translate_eval(Instr, Env);
        load  -> translate_load(Instr, Env);
        store -> translate_store(Instr, Env);
        _ -> {Env,["--- XXX UNHANDLED: " ++ atom_to_list(Tag)]} % xxxxxxxxxx
    end.

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
        lw(t0, Offset1, Reg1) ++
        sw(t0, Offset2, Reg2),
    {Env2, Instructions}.

translate_load_byte(_,_) -> % xxxxxxxxxxxx
    {nil,[]}.

translate_store_long({store, long, TempDstAddress, TempValue}, Env0) ->
    {{Reg1,Offset1},Env1} = ?ENV:lookup(TempDstAddress, Env0),
    {{Reg2,Offset2},Env2} = ?ENV:lookup(TempValue, Env1),
    Instructions =
        lw(t0, Offset1, Reg1) ++
        lw(t1, Offset2, Reg2) ++
        sw(t1, 0, t0),
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
        li(t0, Value) ++
        sw(t0, Offset, sp),
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env0) ->
    {{BaseRegSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseRegDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
        lw(t0, OffsetSrc, BaseRegSrc) ++
        sw(t0, OffsetDst, BaseRegDst),
    {Env2, Instructions}.

translate_eval_labref(TempDst, {labref,Label}, Env0) ->
    {{Reg,Offset},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
        la(t0, Label) ++
        sw(t0, Offset, Reg),
    {Env1, Instructions}.

translate_eval_binop(_,_,_) ->
    {nil,[]}.

move(Dst, Src) ->
    [{move, Dst, Src}].

calc_frame_size({proc, _Label, _Formals, Temps, ArraysSize, _Ins, LabelEnd}) ->
    4 + 4 + ArraysSize + erlang:length(Temps)*4.

prologue(FS, ArraysSize) ->
    subu(sp, sp, FS) ++
    sw(fp, FS-ArraysSize-4, sp) ++
    sw(ra, FS-ArraysSize-8, sp) ++
    addu(fp, sp, FS).

epilogue(FS, ArraysSize, LabelEnd) ->
    labdef(LabelEnd) ++
    lw(ra, FS-ArraysSize-8, sp) ++
    lw(fp, FS-ArraysSize-4, sp) ++
    addu(sp, sp, FS) ++
    jr(ra).

segment_data() ->
    [{segment, data}].

segment_text() ->
    [{segment, text}].

align(Bytes) ->
    [{align, Bytes}].

globl(Label) ->
    [{globl, Label}].

labdef(Label) ->
    [{labdef, Label}].

space(Bytes) ->
    [{space, Bytes}].

subu(Dst, Src1, Src2) ->
    [{subu, Dst, Src1, Src2}].

sub(Dst, Src1, Src2) ->
    [{sub, Dst, Src1, Src2}].

la(Dst, Label) ->
    [{la, Dst, Label}].

sw(Src, Offset, Dst) ->
    [{sw, Src, Offset, Dst}].

lw(Dst, Offset, Src) ->
    [{lw, Dst, Offset, Src}].

j(Label) ->
    [{j, Label}].

jr(Dst) ->
    [{jr, Dst}].

beqz(Rsrc, Label) ->
    [{beqz, Rsrc, Label}].

li(Dst, Value) ->
    [{li, Dst, Value}].

addi(Dst, Src1, Value) ->
    [{addi, Dst, Src1, Value}].

addu(Dst, Src1, Src2) ->
    [{addu, Dst, Src1, Src2}].
