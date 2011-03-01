# Assignment 5 - MIPS Assembly Code

Compiler Project, VT11

2011-XX-XX

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT department server at:
/home/emhe9781/src/XXXXXXXXXXXXXXXX

Previous reports can be found in the project folder under `report`.

(Note that the source won't compile and the executables won't run on the IT
department servers. The servers have an old release of Erlang installed as
default which doesn't include Leex nor escript).

## Introduction

XXX

mention that codegen generates a "structure", emitter is used as the final
phase which actually emits the asm instructions.

## Control Flow Statements

XXX describe how control flow statements are translated

## Activation Record

XXX describe the layout of activation records

The layout of the activation records:

        Stack

    |----------------|
    |   ...          |
    |----------------|
    |   Argument 2   |  8(fp)
    |----------------|
    |   Argument 1   |  4(fp)
    |----------------|
    |   Argument 0   |  0(fp)      ^- Callers activation record
    |================|  <-- fp
    |   a_1[0]       |
    |----------------|
    |   a_1[1]       |
    |----------------|
    |   ...          |
    |----------------|
    |   a_1[n]       |
    |----------------|
    |   a_2[0]       |
    |----------------|
    |   a_2[1]       |
    |----------------|
    |   ...          |
    |----------------|
    |   a_2[n]       |
    |----------------|
    |                |
    |   ...          |
    |                |
    |----------------|
    |   Callers fp   |
    |----------------|
    |   Callers ra   |
    |----------------|
    |   ...          |
    |----------------|
    |   Temp 2       |  8(sp)
    |----------------|
    |   Temp 1       |  4(sp)
    |----------------|
    |   Temp 0       |  0(sp)
    |================|  <-- sp

As indicated by the illustration, the actual parameters are accessed at the
bottom of the callers activation record (fp + offset).
Local arrays are accessed at the top of the activation record (fp - offset).
Local temporaries are accessed at the bottom of the activation record
(sp + offset).

## Calling Convention

XXX describe the calling convention

All parameters are located at the bottom in the callers activation record, as
indicated by the illustration in the section "Activation Record".
Parameters are passed in a right to left, such that argN is put on the stack
before argN-1, ..., arg0. Hence, arg0 is located at the bottom of the
activation record.

XXX caller save
XXX callee save

The return value is placed in register XXX.
