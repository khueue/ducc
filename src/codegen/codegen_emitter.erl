-module(codegen_emitter).
-export([asm_to_string/1]).

asm_to_string(AsmCode) ->
    toplevels_to_string(AsmCode).

toplevels_to_string([Toplevel]) ->
    toplevel_to_string(Toplevel);
toplevels_to_string([Toplevel|Toplevels]) ->
    toplevel_to_string(Toplevel) ++ newline() ++
    toplevels_to_string(Toplevels).

toplevel_to_string([]) -> "";
toplevel_to_string([In|Ins]) ->
    instruction_to_string(In) ++ newline() ++
    toplevel_to_string(Ins).

instruction_to_string({segment, Type}) ->
    indent() ++ "." ++ erlang:atom_to_list(Type);
instruction_to_string({align, Bytes}) ->
    indent() ++ ".align " ++ erlang:integer_to_list(Bytes);
instruction_to_string({globl, Label}) ->
    indent() ++ ".globl " ++ label_string(Label);
instruction_to_string({labdef, Label}) ->
    label_string(Label) ++ ":";
instruction_to_string({space, Bytes}) ->
    indent() ++ ".space " ++ erlang:integer_to_list(Bytes);
instruction_to_string({subu, Dst, Src1, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "subu " ++ commalist([reg(Dst), reg(Src1), erlang:integer_to_list(Icon)]);
instruction_to_string({subu, Dst, Src1, Src2}) ->
    indent() ++ "subu " ++ commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({addu, Dst, Src1, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "addu " ++ commalist([reg(Dst), reg(Src1), erlang:integer_to_list(Icon)]);
instruction_to_string({addu, Dst, Src1, Src2}) ->
    indent() ++ "addu " ++ commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sub, Dst, Src1, Src2}) ->
    indent() ++ "sub " ++ commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sw, Src, Offset, Dst}) ->
    indent() ++ "sw " ++ commalist([reg(Src), mem(Offset, Dst)]);
instruction_to_string({lw, Dst, Offset, Src}) ->
    indent() ++ "lw " ++ commalist([reg(Dst), mem(Offset, Src)]);
instruction_to_string({jr, Dst}) ->
    indent() ++ "jr " ++ reg(Dst);
instruction_to_string(X) ->
    indent() ++ "XXX --- " ++ erlang:atom_to_list(erlang:element(1, X)).

label_string({label, "main"}) ->
    "main";
label_string({label, Name}) ->
    "L" ++ Name;
label_string({label, Id, ""}) ->
    "L" ++ erlang:integer_to_list(Id);
label_string({label, Id, Name}) ->
    "L" ++ erlang:integer_to_list(Id) ++ "_" ++ Name.

commalist([X]) ->
    X;
commalist([X|Xs]) ->
    X ++ ", " ++
    commalist(Xs).

mem(Offset, Temp) ->
    erlang:integer_to_list(Offset) ++ "(" ++ reg(Temp) ++ ")".

reg(Temp) ->
    "$" ++ erlang:atom_to_list(Temp).

indent() ->
    "\t".

newline() ->
    "\n".
