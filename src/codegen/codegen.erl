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
        eval -> translate_eval(Instr, Env)
    end.

translate_eval({eval, Temp, {icon, Value}}, Env0) ->
    {{sp,Offset},Env1} = ?ENV:lookup(Temp, Env0),
    Instructions =
        li(t0, Value) ++
        sw(t0, Offset, sp),
    {Env1, Instructions};
translate_eval({eval, TempDst, TempSrc={temp,_}}, Env0) ->
    {{BaseRegSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseRegDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
        lw(t0, OffsetSrc, BaseRegSrc) ++
        sw(t0, OffsetDst, BaseRegDst),
    {Env2, Instructions}.

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
