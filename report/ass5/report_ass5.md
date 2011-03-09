# Assignment 5 - MIPS Assembly Code

Compiler Project, VT11

2011-03-XXXXX

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT department server at:
/home/emhe9781/src/XXXXXXXXXXXXXXXX

Previous reports can be found in the project folder under `report`.

(Note that the source won't compile and the executables won't run on the IT
department servers. The servers have an old release of Erlang installed as
default which doesn't include Leex nor escript).

## Introduction

XXX mention that codegen generates a "structure", emitter is used as the final
phase which actually emits the asm instructions.

## Control Flow Statements

Control flow statements are translated by `translate_cjump/2` and
`translate_jump/2` located in `src/codegen/codegen.erl`.

### Conditional Jump

    translate_cjump({cjump,Comp,TempDst,{icon,0},Label}, Env0) ->
        {{BaseTemp,OffsetTemp},Env1} = ?ENV:lookup(TempDst, Env0),
        CjumpFun = ?HELPER:asm_cjump_fun(Comp),
        Instructions =
        [
            ?ASM:asm_lw(t0, OffsetTemp, BaseTemp),
            CjumpFun(t0, Label)
        ],
        {Env1, Instructions}.

where `CjumpFun` is `asm_beqz/2` for `Comp` = `eq`, or `asm_bnez/2` for
`Comp` = `neq`.

### Unconditional Jump

    translate_jump({jump,Label}, Env) ->
        Instructions =
        [
            ?ASM:asm_j(Label)
        ],
        {Env, Instructions}.

## Activation Record

The layout of the activation records:

        Stack

    |   ...          |
    |----------------|
    |   Argument 2   |   8(fp)
    |----------------|
    |   Argument 1   |   4(fp)
    |----------------|
    |   Argument 0   |   0(fp)      ^- Callers activation record
    |================|  <-- fp
    |   ...          |
    |----------------|
    |   arr1[0]      |   (0*sizeof(arr1[0]))(fp - ArraysSize + offset_arr1)
    |----------------|
    |   arr0[N]      |   (N*sizeof(arr0[N]))(fp - ArraysSize + offset_arr0)
    |----------------|
    |   ...          |
    |----------------|
    |   arr0[1]      |   (1*sizeof(arr0[1]))(fp - ArraysSize + offset_arr0)
    |----------------|
    |   arr0[0]      |   (0*sizeof(arr0[0]))(fp - ArraysSize + offset_arr0)
    |----------------|  <-- fp - ArraysSize
    |   Callers fp   |
    |----------------|
    |   Callers ra   |
    |----------------|
    |   Temp N       |
    |----------------|
    |   ...          |
    |----------------|
    |   Temp 2       |   8(sp)
    |----------------|
    |   Temp 1       |   4(sp)
    |----------------|
    |   Temp 0       |   0(sp)
    |================|  <-- sp

Actual parameters are accessed at the bottom of the callers activation record,
fp + offset.

Local arrays are accessed at the top of the activation record using 
`(Index*sizeof(Element))` + `(fp - ArraysSize + array_offset)`, where
`ArraysSize` is the combined size of all local arrays.

Local temporaries are accessed at the bottom of the activation record, 
sp + offset.

## Calling Convention

The calling convention looks like:

  * 1 Caller: Set up call
  * 2 Callee: Prologue
  * 3 Callee: Epilogue
  * 4 Caller: Clean

### 1 Caller: Set up call

The caller pushes all arguments to the stack and calls the callee. 
As such, all parameters are located at the bottom in the callers activation
record, as indicated by the illustration in the section _Activation Record_.
See `translate_call/2` in `src/codegen/codegen.erl`.

### 2 Callee: Prologue

The callee sets up its activation record and pushes the callers `ra` and `fp`
registers on the stack. See `translate_proc/1` in `src/codegen/codegen.erl`
and `setup_function_prologue/2` in `src/codegen/codegen_helpers.erl`.

### 3 Callee: Epilogue

The callee reinstates the callers `ra` and `fp` registers, deallocates its
activation record and returns to the caller. See `translate_proc/1` in 
`src/codegen/codegen.erl` and `setup_function_epilogue/3` in 
`src/codegen/codegen_helpers.erl`.

The return value is placed in register `v0`, see `translate_eval_temp/3` in
`src/codegen/codegen.erl` which calls `translate_eval_temp_return/2` in 
`src/codegen/codegen.erl` if the temporary which is being evaluated is 
`{temp,0}`, which was used in the RTL to indicate the return value.

### 4 Caller: Clean

The caller pushes the return value onto the stack and restores the old
activation record by deallocating the arguments which was pushed on the stack
in step 1. See `translate_call/2` in `src/codegen/codegen.erl`.

