-module(codegen).
-export([generate_code/2]).

% -define(HELPER, codegen_helpers).
-define(ENV, codegen_env).

generate_code(RtlCode, Lines) ->
    Env = ?ENV:new(Lines),
    AsmCode = translate_toplevels(RtlCode, Env),
    AsmCode.

translate_toplevels([], _Env) -> [];
translate_toplevels([Toplevel|Toplevels], Env) ->
    ToplevelAsm = translate_toplevel(Toplevel, Env),
    [ToplevelAsm|translate_toplevels(Toplevels, Env)].

translate_toplevel(Toplevel, Env) ->
    Type = erlang:element(1, Toplevel),
    case Type of
        data -> translate_data(Toplevel, Env);
        proc -> translate_proc(Toplevel, Env)
    end.

translate_data({data, Label, Bytes}, _Env) ->
    Instructions =
        segment_data() ++
        align(4) ++
        labdef(Label) ++
        space(Bytes),
    Instructions.

translate_proc(Proc = {proc, Label, Formals, Temps, ArraysSize, Ins, LabelEnd}, _Env) ->
    FS = calc_frame_size(Proc),
    Instructions =
        segment_text() ++
        globl(Label) ++
        labdef(Label) ++
        prologue(FS, ArraysSize) ++
        % Ins ++
        epilogue(FS, ArraysSize, LabelEnd),
    Instructions.

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
    j(ra).

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

j(Dst) ->
    [{j, Dst}].

addu(Dst, Src1, Src2) ->
    [{addu, Dst, Src1, Src2}].
