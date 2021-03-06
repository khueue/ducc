# Assignment 4 - Intermediate Code

Compiler Project, VT11

2011-02-18

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT department server at:
/home/emhe9781/src/ducc-2011-02-18.tar.gz

Previous reports can be found in the project folder under `report`.

(Note that the source won't compile and the executables won't run on the IT
department servers. The servers have an old release of Erlang installed as
default which doesn't include Leex nor escript).

## Introduction

In this report, we will briefly discuss our attempt to implement a
translation from the abstract syntax tree (AST) into a flat, RTL-like,
intermediate representation for the uC language.

We started out by implementing a depth-first search algorithm for traversing
the AST. The translator starts at the root node (`program`) and recursively
translates its children in a left-to-right order.

## Translator Environment

The environment is implemented in `src/translator/translator_env.erl` and
has the form:

    {LastUsedTemp, LastUsedLabel, CurrentScopeData, Scopes}

### LastUsedTemp

`LastUsedTemp` is used to denote the last used temporary register, and has
the form:

    {temp, TempId}

The temporary registers are used for actual parameters and local scalar
variables.

`TempId` is an integer value which starts at 0. `{temp, 0}` is reserved for
the return value. `{temp, 1}` is reserved for the virtual frame pointer
(which is used for local array variables). The next free temporary register
is `{temp, 2}`.

Note that `LastUsedTemp` is supposed to act as a global variable, and is
therefore preserved when leaving a scope. Each invocation of a function that
gives a new temporary is guaranteed to get a unique temporary as long as
the latest environment is always passed along.

### LastUsedLabel

`LastUsedLabel` is used to denote the last used label, and has the form:

    {label, LabelId}

The labels are used for global variables.

`LabelId` is an integer value which is set to 99 (different from temps, to
distinguish them easily) in a new environment, denoting that the next free
label is `{label, 100}`.

Note that `LastUsedLabel` is just as "global" as `LastUsedTemp`.

### CurrentScopeData

`CurrentScopeData` contains data about the current scope (only has meaning
when translating a function), and has the form:

    {StartLabel, EndLabel, FrameSize}

The `StartLabel` and `EndLabel` are labels of the same form as indicated by
`LastUsedLabel` above. They are used to mark the start and end of the current
function, respectively. Initially when entering a scope, `StartLabel` and
`EndLabel` are set to `nil` (later to be filled in by the function under
translation).

`FrameSize` is an integer value which denotes the frame size of the current
scope. Initially when entering a scope, `FrameSize` is set to 0.

Note that `CurrentScopeData` is not preserved when leaving a scope with
`leave_scope/1`.

### Scopes

`Scopes` is a stack of scopes, just as in the analyzer. In these scopes,
we use dicts that store information about location, type and size of
encountered symbols.

#### Symbol Tables

As soon as a new variable declaration is encountered, it is entered into
the symbol table of the current scope. We store the following structures,
indexed by the name of the identifier:

 * Global scalar: `{global, {label,321}, scalar, {Size}`
 * Global array : `{global, {label,321}, array,  {Size}`
 * Local scalar:  `{local,  {temp,123},  scalar, {Size}`
 * Local array:   `{local,  stack,       array,  {Size, Offset}`
 * Formal array:  `{local,  {temp,123},  farray, {Size}`

Formal scalars are treated as local scalars.

In the above enumeration, `Size` is the size requirement of the data type, so
the size of char would be `byte` and the size of int would be `long`.
`Offset` is the offset of the array from the enclosing function's FP.

## RTL

We have mostly followed the proposed RTL. We made load and store into
proper instructions. Unary instructions are converted to binary instructions.

### Top-Level Constructs

The result from the translator consists of a list of top-level declarations
of the following two types, either data or procedure:

    toplevel_data(Label, Bytes) ->
        {data, Label, Bytes}.

    toplevel_proc(LabelStart, Formals, Temps, FS, Ins, LabelEnd) ->
        {proc, LabelStart, Formals, Temps, FS, Ins, {labdef, LabelEnd}}.

The `data` construct contains an identifying label and a size in bytes.
The `proc` construct contains start and end labels, a list of temps used
by formal arguments, a list of temps used by the body of the function,
the frame size required by the function (its local arrays), and the
instructions emitted by the function body.

### Expressions

Basic RTL expressions are represented by the following structures:

    rtl_temp(Temp) ->
        Temp.

    rtl_icon(Int) ->
        {icon, Int}.

    rtl_labref(Label) ->
        {labref, Label}.

    rtl_binop(Op, TempLhs, TempRhs) ->
        {binop, Op, TempLhs, TempRhs}.

### Instructions

Instructions emitted by the translator have the following structure:

    emit_labdef(Label) ->
        {labdef, Label}.

    emit_jump(Label) ->
        {jump, Label}.

    emit_cjump(Relop, TempLhs, TempRhs, Label) ->
        {cjump, Relop, TempLhs, TempRhs, Label}.

    emit_store(Size, TempDestAddress, TempValue) ->
        {store, Size, TempDestAddress, TempValue}.

    emit_load(Size, TempDest, TempSourceAddress) ->
        {load, Size, TempDest, TempSourceAddress}.

    emit_eval(TempResult, RtlExpr) ->
        {eval, TempResult, RtlExpr}.

    emit_call(TempResult, Label, TempsActuals) ->
        {call, TempResult, Label, TempsActuals}.

## Translation Process

### Control Flow Statements

#### if

There are two cases for the if statement which we have to consider. The one
where the else part has been omitted, and the one where the else part is
included.

The case without the else part:

    Instructions =
        InsCond ++
        [emit_cjump(eq, RetCond, 0, LabelEnd)] ++
        InsThen ++
        [emit_labdef(LabelEnd)],

The case with the else part:

    Instructions =
        InsCond ++
        [emit_cjump(eq, RetCond, 0, LabelElse)] ++
        InsThen ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelElse)] ++
        InsElse ++
        [emit_labdef(LabelEnd)],

`InsCond` is the instructions for the condition, which has been recursively
translated by translate_expr/2.
`InsThen` and `InsElse` is the instructions for the then and else part,
respectively, which has been recursively translated by translate_stmt/2.

#### while

While statements are translated by recursively translating the condition with
translate_expr/2 and the statement (body) with translate_stmt/2. Yielding the
instructions:

    Instructions =
        [emit_jump(LabelTest)] ++
        [emit_labdef(LabelBody)] ++
        InsStmt ++
        [emit_labdef(LabelTest)] ++
        InsCond ++
        [emit_cjump(neq, RetCond, 0, LabelBody)] ++
        [emit_labdef(LabelEnd)]

`InsStmt` is the instructions for the statement (body) and InsCond is the
instructions for the condition.
`RetCond` contains the result of the translated condition.

#### Logical and

Logical and, `&&`, is translated by translating the expressions in the left
and right hand sides recursively. The resulting instructions are:

    Instructions =
        InsLhs ++
        [emit_cjump(eq, TempLhs, 0, LabelFalse)] ++
        InsRhs ++
        [emit_cjump(eq, TempRhs, 0, LabelFalse)] ++
        [emit_eval(TempResult, ValueTrue)] ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelFalse)] ++
        [emit_eval(TempResult, ValueFalse)] ++
        [emit_labdef(LabelEnd)]

`InsLhs` and `InsRhs` are the instructions for the left hand side and right
right hand side, respectively, which has been recursively translated by
translate_expr/2.
`TempLhs` contains the result of the left hand side. `TempRhs` contains the
result of the right hand side.
`TempResult` will contain the result of the entire logical and statement,
which either will be true (1) or false (0).

#### Logical or

Logical or, `||`, is translated by translating the expressions in the left and
right hand sides recursively. The resulting instructions are:

    Instructions =
        InsLhs ++
        [emit_cjump(neq, TempLhs, 0, LabelTrue)] ++
        InsRhs ++
        [emit_cjump(neq, TempRhs, 0, LabelTrue)] ++
        [emit_eval(TempResult, ValueFalse)] ++
        [emit_jump(LabelEnd)] ++
        [emit_labdef(LabelTrue)] ++
        [emit_eval(TempResult, ValueTrue)] ++
        [emit_labdef(LabelEnd)]

`InsLhs` and `InsRhs` are the instructions for the left hand side and right
right hand side, respectively, which has been recursively translated by
translate_expr/2.
`TempLhs` contains the result of the left hand side. `TempRhs` contains the
result of the right hand side.
`TempResult` will contain the result of the entire logical or statement, which
either will be true (1) or false (0).

### Variable References

Global lval array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempOffset, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempBaseAddr, rtl_labref(Label))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempBaseAddr, TempOffset))] ++
        [emit_store(Size, TempAddress, TempRhs)],

Global rval array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempOffset, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempBaseAddr, rtl_labref(Label))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempBaseAddr, TempOffset))] ++
        [emit_load(Size, TempResult, TempAddress)],

Global lval scalar:

    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))] ++
        [emit_store(Size, TempAddress, TempRhs)],

Global rval scalar:

    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))] ++
        [emit_load(Type, TempValue, TempAddress)],

Global rval array:

    Instructions =
        [emit_eval(TempAddress, rtl_labref(Label))],

Local lval array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempFrameOffset, rtl_icon(Offset))] ++
        [emit_eval(TempFrameAndMultOffset, rtl_binop('+', TempFrameOffset, TempMult))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', ?ENV:get_fp(), TempFrameAndMultOffset))] ++
        [emit_store(Size, TempElementAddress, TempRhs)],

Local lval formal array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', Temp, TempMult))] ++
        [emit_store(Size, TempElementAddress, TempRhs)],

Local lval scalar:

    Instructions =
        [emit_eval(Temp, rtl_temp(TempRhs))],

Local rval array:

    Instructions =
        [emit_eval(TempOffset, rtl_icon(Offset))] ++
        [emit_eval(TempAddress, rtl_binop('+', TempFP, TempOffset))],

Local rval array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempFrameOffset, rtl_icon(Offset))] ++
        [emit_eval(TempFrameAndMultOffset, rtl_binop('+', TempFrameOffset, TempMult))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', ?ENV:get_fp(), TempFrameAndMultOffset))] ++
        [emit_load(Size, TempResult, TempElementAddress)],

Local rval formal array element:

    Instructions =
        [emit_eval(TempSizeof, rtl_icon(Sizeof))] ++
        [emit_eval(TempMult, rtl_binop('*', TempIndex, TempSizeof))] ++
        [emit_eval(TempElementAddress, rtl_binop('+', TempBase, TempMult))] ++
        [emit_load(Size, TempResult, TempElementAddress)],

Local formal arrays and local scalars as rvals does not generate any
instructions since they are assigned to temporary registers, which can
be used ad-hoc.

## Running the Translator

### Multi-Step Compilation

The translator (as all previous steps) has been implemented to read from
standard input and output to standard output.
As such it's possible to, for example, pipe the result from the lexer to the
parser, from the parser to the analyzer, and from the analyzer to the
translator:

    cat suite/quiet/rtl/r01.c | lexer | parser | analyzer | translator

If the translator encounters an error, it will print a error message to
standard output and stop evaluation. Otherwise, if successful, the translator
will output the RTL code to standard output.

### Single-Step Compilation

For a more conventional approach to compilation, a script called `ducc` can
be used. `ducc` takes a single file as argument and runs all (implemented)
successive steps on it, and prints the result to standard output:

    ducc file.c

The script `ducc` can also be flagged to stop after a certain step. To stop
after for example the translator (which is currently the default since we
have not implemented any more steps yet), one can issue the command:

    ducc -t file.c
