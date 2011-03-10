-module(codegen_helpers).
-export([
    asm_binop_fun/1,
    asm_cjump_fun/1,
    calculate_frame_size/2,
    setup_function_header/1,
    setup_function_prologue/2,
    setup_function_epilogue/3,
    setup_stdio/0]).

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

setup_function_header(Label) ->
    Instructions =
    [
        ?ASM:asm_segment_text(),
        ?ASM:asm_globl(Label),
        ?ASM:asm_labdef(Label)
    ],
    Instructions.

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

setup_stdio() ->
    Instructions =
    [
        setup_function_putint(),
        setup_function_getint(),
        setup_function_putstring(),
        setup_function_getstring()
    ],
    Instructions.

setup_function_putint() ->
    Label = {label, "putint"},
    Header = setup_function_header(Label),
    Prologue = setup_stdio_prologue(),
    Body =
    [
        ?ASM:asm_lw(a0, 0, fp),
        ?ASM:asm_li(v0, 1),
        ?ASM:asm_syscall()
    ],
    Epilogue = setup_stdio_epilogue(),
    Instructions =
        Header ++
        Prologue ++
        Body ++
        Epilogue,
    Instructions.

setup_function_getint() ->
    Label = {label, "getint"},
    Header = setup_function_header(Label),
    Prologue = setup_stdio_prologue(),
    Body =
    [
        ?ASM:asm_li(v0, 5),
        ?ASM:asm_syscall()
    ],
    Epilogue = setup_stdio_epilogue(),
    Instructions =
        Header ++
        Prologue ++
        Body ++
        Epilogue,
    Instructions.

setup_function_putstring() ->
    Label = {label, "putstring"},
    Header = setup_function_header(Label),
    Prologue = setup_stdio_prologue(),
    Body =
    [
        ?ASM:asm_lw(a0, 0, fp),
        ?ASM:asm_li(v0, 4),
        ?ASM:asm_syscall()
    ],
    Epilogue = setup_stdio_epilogue(),
    Instructions =
        Header ++
        Prologue ++
        Body ++
        Epilogue,
    Instructions.

setup_function_getstring() ->
    Label = {label, "getstring"},
    Header = setup_function_header(Label),
    Prologue = setup_stdio_prologue(),
    Body =
    [
        ?ASM:asm_lw(a0, 0, fp),
        ?ASM:asm_li(a1, 1024), % XXX: Arbitrary constant length.
        ?ASM:asm_li(v0, 8),
        ?ASM:asm_syscall()
    ],
    Epilogue = setup_stdio_epilogue(),
    Instructions =
        Header ++
        Prologue ++
        Body ++
        Epilogue,
    Instructions.

setup_stdio_prologue() ->
    Instructions =
    [
        ?ASM:asm_subu(sp, sp, 8),
        ?ASM:asm_sw(fp, 4, sp),
        ?ASM:asm_sw(ra, 0, sp),
        ?ASM:asm_addu(fp, sp, 8)
    ],
    Instructions.

setup_stdio_epilogue() ->
    Instructions =
    [
        ?ASM:asm_lw(ra, 0, sp),
        ?ASM:asm_lw(fp, 4, sp),
        ?ASM:asm_addu(sp, sp, 8),
        ?ASM:asm_jr(ra)
    ],
    Instructions.
