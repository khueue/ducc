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

translate_data({data,Label,Bytes}) ->
    Instructions =
    [
        asm_segment_data(),
        asm_align(4),
        asm_labdef(Label),
        asm_space(Bytes)
    ],
    Instructions.

translate_proc({proc,Label,Formals,Locals,ArraysSize,Ins,LabelEnd}) ->
    FS = calculate_frame_size(Locals, ArraysSize),
    Env = ?ENV:new(nil, Formals, Locals), % xxxxxxxxx Lines?
    BeforeFunction =
    [
        asm_segment_text(),
        asm_globl(Label),
        asm_labdef(Label)
    ],
    Instructions =
        BeforeFunction ++
        function_prologue(FS, ArraysSize) ++
        translate_instructions(Ins, Env) ++
        function_epilogue(FS, ArraysSize, LabelEnd),
    Instructions.

calculate_frame_size(Temps, ArraysSize) ->
    4 + 4 + ArraysSize + 4*erlang:length(Temps).

function_prologue(FS, ArraysSize) ->
    [
        asm_subu(sp, sp, FS),
        asm_sw(fp, FS-ArraysSize-4, sp),
        asm_sw(ra, FS-ArraysSize-8, sp),
        asm_addu(fp, sp, FS)
    ].

function_epilogue(FS, ArraysSize, LabelEnd) ->
    [
        asm_labdef(LabelEnd),
        asm_lw(ra, FS-ArraysSize-8, sp),
        asm_lw(fp, FS-ArraysSize-4, sp),
        asm_addu(sp, sp, FS),
        asm_jr(ra)
    ].

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
        asm_labdef(Label)
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
        asm_lw(t0, OffsetSrc, BaseSrc),
        asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env2, Instructions}.

% xxxxxx.
translate_load_byte(_TempDst, _TempSrcAddr, _Env0) ->
    {nil, []}.

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
        asm_lw(t0, OffsetDst, BaseDst),
        asm_lw(t1, OffsetVal, BaseVal),
        asm_sw(t1, 0, t0)
    ],
    {Env2, Instructions}.

% xxxxxx.
translate_store_byte(_TempDstAddr, _TempValue, _Env0) ->
    {nil, []}.

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
        asm_li(t0, Value),
        asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env1, Instructions}.

translate_eval_temp(TempDst, TempSrc, Env0) ->
    {{BaseSrc,OffsetSrc},Env1} = ?ENV:lookup(TempSrc, Env0),
    {{BaseDst,OffsetDst},Env2} = ?ENV:lookup(TempDst, Env1),
    Instructions =
    [
        asm_lw(t0, OffsetSrc, BaseSrc),
        asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env2, Instructions}.

translate_eval_labref(TempDst, {labref,Label}, Env0) ->
    {{BaseDst,OffsetDst},Env1} = ?ENV:lookup(TempDst, Env0),
    Instructions =
    [
        asm_la(t0, Label),
        asm_sw(t0, OffsetDst, BaseDst)
    ],
    {Env1, Instructions}.

translate_eval_binop(TempDst, {binop,Op,Lhs,Rhs}, Env) ->
    % XXX Only arithmetics in binop?
    translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env).

translate_eval_arithmetic(TempDst, Op, Lhs, Rhs, Env0) ->
    {{BaseLhs,OffsetLhs},Env1} = ?ENV:lookup(Lhs, Env0),
    {{BaseRhs,OffsetRhs},Env2} = ?ENV:lookup(Rhs, Env1),
    {{BaseDst,OffsetDst},Env3} = ?ENV:lookup(TempDst, Env2),
    Fun = asm_fun(Op),
    Instructions =
    [
        asm_lw(t0, OffsetLhs, BaseLhs),
        asm_lw(t1, OffsetRhs, BaseRhs),
        Fun(t2, t0, t1),
        asm_sw(t2, OffsetDst, BaseDst)
    ],
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

% Various.
asm_segment_data() -> {segment, data}.
asm_segment_text() -> {segment, text}.
asm_align(Bytes)   -> {align, Bytes}.
asm_globl(Label)   -> {globl, Label}.
asm_labdef(Label)  -> {labdef, Label}.
asm_space(Bytes)   -> {space, Bytes}.

% Comparisons.
asm_slt(Dst, Src1, Src2) -> {slt, Dst, Src1, Src2}.
asm_sle(Dst, Src1, Src2) -> {sle, Dst, Src1, Src2}.
asm_sgt(Dst, Src1, Src2) -> {sgt, Dst, Src1, Src2}.
asm_sge(Dst, Src1, Src2) -> {sge, Dst, Src1, Src2}.
asm_seq(Dst, Src1, Src2) -> {seq, Dst, Src1, Src2}.
asm_sne(Dst, Src1, Src2) -> {sne, Dst, Src1, Src2}.

% Simple loads.
asm_la(Dst, Label) -> {la, Dst, Label}.
asm_li(Dst, Value) -> {li, Dst, Value}.

% Load and store.
asm_sw(Src, Offset, Dst) -> {sw, Src, Offset, Dst}.
asm_lw(Dst, Offset, Src) -> {lw, Dst, Offset, Src}.

% Jumps.
%asm_j(Label) -> {j, Label}.
asm_jr(Dst)   -> {jr, Dst}.

% Branches.
%asm_beqz(Rsrc, Label) -> {beqz, Rsrc, Label}.

% Arithmetic.
asm_add(Dst, Src1, Src2)    -> {add, Dst, Src1, Src2}.
%asm_addi(Dst, Src1, Value) -> {addi, Dst, Src1, Value}.
asm_addu(Dst, Src1, Src2)   -> {addu, Dst, Src1, Src2}.
asm_sub(Dst, Src1, Src2)    -> {sub, Dst, Src1, Src2}.
asm_subu(Dst, Src1, Src2)   -> {subu, Dst, Src1, Src2}.
asm_mul(Dst, Src1, Src2)    -> {mul, Dst, Src1, Src2}.
asm_div(Dst, Src1, Src2)    -> {'div', Dst, Src1, Src2}.
