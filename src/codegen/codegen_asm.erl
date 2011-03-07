-module(codegen_asm).
-export([
    % Various.
    asm_segment_data/0,
    asm_segment_text/0,
    asm_align/1,
    asm_globl/1,
    asm_labdef/1,
    asm_space/1,
    % Arithmetic.
    asm_add/3,
    asm_addu/3,
    asm_addi/3,
    asm_sub/3,
    asm_subu/3,
    asm_mul/3,
    asm_div/3,
    % Comparisons.
    asm_slt/3,
    asm_sle/3,
    asm_sgt/3,
    asm_sge/3,
    asm_seq/3,
    asm_sne/3,
    % Simple loads.
    asm_la/2,
    asm_li/2,
    % Loads and stores.
    asm_sw/3,
    asm_lw/3,
    % Jumps.
    asm_j/1,
    asm_jr/1,
    % Branches.
    asm_beqz/2]).

% Various.
asm_segment_data() -> {segment, data}.
asm_segment_text() -> {segment, text}.
asm_align(Bytes)   -> {align, Bytes}.
asm_globl(Label)   -> {globl, Label}.
asm_labdef(Label)  -> {labdef, Label}.
asm_space(Bytes)   -> {space, Bytes}.

% Arithmetic.
asm_add(Dst, Src1, Src2)   -> {add, Dst, Src1, Src2}.
asm_addi(Dst, Src1, Value) -> {addi, Dst, Src1, Value}.
asm_addu(Dst, Src1, Src2)  -> {addu, Dst, Src1, Src2}.
asm_sub(Dst, Src1, Src2)   -> {sub, Dst, Src1, Src2}.
asm_subu(Dst, Src1, Src2)  -> {subu, Dst, Src1, Src2}.
asm_mul(Dst, Src1, Src2)   -> {mul, Dst, Src1, Src2}.
asm_div(Dst, Src1, Src2)   -> {'div', Dst, Src1, Src2}.

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

% Loads and stores.
asm_sw(Src, Offset, Dst) -> {sw, Src, Offset, Dst}.
asm_lw(Dst, Offset, Src) -> {lw, Dst, Offset, Src}.

% Jumps.
asm_j(Label) -> {j, Label}.
asm_jr(Dst)  -> {jr, Dst}.

% Branches.
asm_beqz(Rsrc, Label) -> {beqz, Rsrc, Label}.
