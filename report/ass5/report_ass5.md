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

We have implemented the last phase in two different steps:

  1. Code Generator
  2. Emitter

The code generator, located in `src/codegen/`, generates a structure which
includes the .c source file and the assembler instructions.
The emitter, located in `src/emitter/` takes the output from the code
generator and actually outputs the assembler instructions in its raw form
(which is executable in spim).

## Code Generator

The result from the code generator has the form:

    {
        {source, SourceList},
        {asm, TopLevelsAsm}
    }

where `SourceList` is a list consisting of the content if the .c source file,
`TopLevelsAsm` is a list of generated assembler instructions.

`TopLevelsAsm` has the form:

    [
        TopLevelAsm,
        TopLevelAsm,
        ...,
        TopLevelAsm
    ]

Each `TopLevelAsm` is a list of assembler instructions, either for global data
or procedures.

See `suite/noisy/advanced/8queens.c.codegen` for an extensive example of what
the code generators output looks like.

## Emitter

The emitter takes the code generators output and produces a string for the
assembler instructions in `TopLevelsAsm`.

See `suite/noisy/advanced/8queens.c.emitter` for an extensive example of what
the the emitters output looks like.

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

Steps in the calling convention:

  1. Caller - Set Up Call
  2. Callee - Prologue
  3. Callee - Epilogue
  4. Caller - Clean

### 1. Caller - Set Up Call

The caller pushes all arguments to the stack and executes `jal`.  See
`translate_call/2` in `src/codegen/codegen.erl`.

### 2. Callee - Prologue

The callee creates its own activation record and saves the callers `fp` and
`ra` on the stack.
See `translate_proc/1` in `src/codegen/codegen.erl` and
`setup_function_prologue/2` in `src/codegen/codegen_helpers.erl`.

### 3. Callee - Epilogue

The callee restores `ra` and the callers `fp`. The callee then deallocates its
activation record and returns to the caller. See `translate_proc/1` in
`src/codegen/codegen.erl` and `setup_function_epilogue/3` in
`src/codegen/codegen_helpers.erl`.

The return value is placed in register `v0`. See `translate_eval_temp/3` in
`src/codegen/codegen.erl`. If the temporary which is being evaluated is
`{temp,0}` (`{temp,0}` was used in the RTL to indicate the return value),
then `translate_eval_temp/3` calls `translate_eval_temp_return/2`
which puts the result in register `v0`.

### 4. Caller - Clean

The caller saves the return value on the stack and restores the old activation
record before the call by deallocating the arguments which was pushed on the
stack in step 1. See `translate_call/2` in `src/codegen/codegen.erl`.

## Running the Code Generator and Emitter

### Multi-Step Compilation

The translator (as all previous steps) has been implemented to read from
standard input and output to standard output.
As such it's possible to, for example, pipe the result from the lexer to the
parser, from the parser to the analyzer, from the analyzer to the
translator, from the translator to the codegen, and finally from the codegen
to the emitter:

    cat suite/quiet/rtl/r01.c | lexer | parser | analyzer | translator | codegen | emitter

### Single-Step Compilation

For a more conventional approach to compilation, a script called `ducc` can
be used. `ducc` takes a single file as argument and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

The script `ducc` can also be flagged to stop after a certain step. To stop
after the codegen, one can issue the command:

    ducc -c file.c

To stop after the emitter, (which is currently the default), one can issue the
command:

    ducc -e file.c

## Testruns

Testruns for assignment 5 are available in `report/ass5/testruns_ass5.md`
or browsable online at
<https://github.com/khueue/ducc/tree/master/report/ass5>. Testruns for the
entire testsuite is available in `suite/` or browsable online at
<https://github.com/khueue/ducc/tree/master/suite>.
