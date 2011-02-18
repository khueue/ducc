# Assignment 4 - Intermediate Code

Compiler Project, VT11

2011-02-18

Emil Hessman (emhe9781@student...)

Sebastian Lundström (selu7901@student...)

The source and executables are available on the IT department server at:
/home/emhe9781/src/XXX

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

The environment is implemented in `src/translator/translator_env.erl`.

The environment has the form:

    {LastUsedTemp, LastUsedLabel, CurrentScopeData, Scopes}

#### LastUsedTemp

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

#### LastUsedLabel

`LastUsedLabel` is used to denote the last used label, and has the form:

    {label, LabelId}

The labels are used for global variables.

`LabelId` is an integer value which is set to 99 (different from temps, to
distinguish them easily) in a new environment, denoting that the next free
label is `{label, 100}`.

Note that `LastUsedLabel` is just as "global" as `LastUsedTemp`.

#### CurrentScopeData

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

#### Scopes

`Scopes` is a stack of scopes, just as in the analyzer. In these scopes,
we use dicts that store information about location, type and size of
encountered symbols.

### Symbol Tables

XXX different kind of nodes compared to the analyzer?

Each symbol table is represented by an Erlang dictionary. When we encounter
a new declaration, its identifier is used as the key, and its entire AST node
is used as the associated value. This provides us with all the information we
need (and more), and we do not need to devise new data types. On the
downside, this probably involves more copying than necessary.

## RTL

XXX RTL design, datatypes, ...

## Translation Process

### Control Flow Statements

#### if

XXX

#### while

XXX

#### Logical and (`&&`)

XXX

#### Logical or (`||`)

Logical or, `||`, is translated by translating the expressions in the left and
right hand sides recursively.

If the result of the left hand side is true, it skips the instructions for the
right hand side by jumping to a label further down, where it sets the result
of the entire logical expression to true and reaches the end.

If the result of the left hand side is false (e.g. 0), it continues evaluating
the instructions for the right hand side. If the result of the right hand side
is true, it jumps to the label which sets the result of the entire logical
expression to true and reaches the end.
In the case when the right hand side is false it sets the result of the entire
logical expression to false and jumps to the end.

### Variable References

#### Global and Local Variables

XXX

#### Array and Scalar Variables

XXX

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

### Error Handling

When a script (such as `translator`) receives invalid input, an error message
is printed and execution stops.

An error exhibited by the translator looks like:

    XXX
