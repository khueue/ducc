-module(codegen_helpers).
-export([asm_fun/1,
        calc_frame_size/1]).

-define(ASM, codegen_asm).

asm_fun('+')  -> fun ?ASM:asm_add/3;
asm_fun('-')  -> fun ?ASM:asm_sub/3;
asm_fun('*')  -> fun ?ASM:asm_mul/3;
asm_fun('/')  -> fun ?ASM:asm_div/3;
asm_fun('<')  -> fun ?ASM:asm_slt/3;
asm_fun('<=') -> fun ?ASM:asm_sle/3;
asm_fun('>')  -> fun ?ASM:asm_sgt/3;
asm_fun('>=') -> fun ?ASM:asm_sge/3;
asm_fun('==') -> fun ?ASM:asm_seq/3;
asm_fun('!=') -> fun ?ASM:asm_sne/3.

calc_frame_size({proc, _Label, _Formals, Temps, ArraysSize, _Ins, _LabelEnd}) ->
    4 + 4 + ArraysSize + erlang:length(Temps)*4.
