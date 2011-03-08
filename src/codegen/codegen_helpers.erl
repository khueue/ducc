-module(codegen_helpers).
-export([
    asm_binop_fun/1,
    asm_cjump_fun/1,
    calculate_frame_size/2,
    setup_function_prologue/2,
    setup_function_epilogue/3]).

-define(ASM, codegen_asm).

asm_binop_fun('+')  -> fun ?ASM:asm_add/3;
asm_binop_fun('-')  -> fun ?ASM:asm_sub/3;
asm_binop_fun('*')  -> fun ?ASM:asm_mul/3;
asm_binop_fun('/')  -> fun ?ASM:asm_div/3;
asm_binop_fun('<')  -> fun ?ASM:asm_slt/3;
asm_binop_fun('<=') -> fun ?ASM:asm_sle/3;
asm_binop_fun('>')  -> fun ?ASM:asm_sgt/3;
asm_binop_fun('>=') -> fun ?ASM:asm_sge/3;
asm_binop_fun('==') -> fun ?ASM:asm_seq/3;
asm_binop_fun('!=') -> fun ?ASM:asm_sne/3.

asm_cjump_fun(eq)  -> fun ?ASM:asm_beqz/2;
asm_cjump_fun(neq) -> fun ?ASM:asm_bnez/2.

calculate_frame_size(Locals, ArraysSize) ->
    4 + 4 + % FP and RA.
    ArraysSize +
    4 * erlang:length(Locals).

setup_function_prologue(FS, ArraysSize) ->
    [
        ?ASM:asm_subu(sp, sp, FS),
        ?ASM:asm_sw(fp, FS-ArraysSize-4, sp),
        ?ASM:asm_sw(ra, FS-ArraysSize-8, sp),
        ?ASM:asm_addu(fp, sp, FS)
    ].

setup_function_epilogue(FS, ArraysSize, LabelEnd) ->
    [
        ?ASM:asm_labdef(LabelEnd),
        ?ASM:asm_lw(ra, FS-ArraysSize-8, sp),
        ?ASM:asm_lw(fp, FS-ArraysSize-4, sp),
        ?ASM:asm_addu(sp, sp, FS),
        ?ASM:asm_jr(ra)
    ].
