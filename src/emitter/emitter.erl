-module(emitter).
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
    indent() ++ ".align" ++ indent() ++ erlang:integer_to_list(Bytes);
instruction_to_string({globl, Label}) ->
    indent() ++ ".globl" ++ indent() ++ label_string(Label);
instruction_to_string({labdef, Label}) ->
    label_string(Label) ++ ":";
instruction_to_string({space, Bytes}) ->
    indent() ++ ".space" ++ indent() ++ erlang:integer_to_list(Bytes);

instruction_to_string({add, Dst, Src1, Src2}) ->
    indent() ++ "add" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({addu, Dst, Src1, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "addu" ++ indent() ++
    commalist([reg(Dst), reg(Src1), erlang:integer_to_list(Icon)]);
instruction_to_string({addu, Dst, Src1, Src2}) ->
    indent() ++ "addu" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({addi, Dst, Src1, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "addi" ++ indent() ++
    commalist([reg(Dst), reg(Src1), erlang:integer_to_list(Icon)]);
instruction_to_string({sub, Dst, Src1, Src2}) ->
    indent() ++ "sub" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({subu, Dst, Src1, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "subu" ++ indent() ++
    commalist([reg(Dst), reg(Src1), erlang:integer_to_list(Icon)]);
instruction_to_string({subu, Dst, Src1, Src2}) ->
    indent() ++ "subu" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({mul, Dst, Src1, Src2}) ->
    indent() ++ "mul" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({'div', Dst, Src1, Src2}) ->
    indent() ++ "div" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);

instruction_to_string({li, Dst, Icon}) when erlang:is_integer(Icon) ->
    indent() ++ "li" ++ indent() ++ % xxxxxxxxx ???
    commalist([reg(Dst), erlang:integer_to_list(Icon)]);
instruction_to_string({li, Dst, Char}) ->
    Icon = char_to_int(Char),
    instruction_to_string({li,Dst,Icon});
instruction_to_string({la, Dst, Label}) ->
    indent() ++ "la" ++ indent() ++
    commalist([reg(Dst), label_string(Label)]);

instruction_to_string({slt, Dst, Src1, Src2}) ->
    indent() ++ "slt" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sle, Dst, Src1, Src2}) ->
    indent() ++ "sle" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sgt, Dst, Src1, Src2}) ->
    indent() ++ "sgt" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sge, Dst, Src1, Src2}) ->
    indent() ++ "sge" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({seq, Dst, Src1, Src2}) ->
    indent() ++ "seq" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);
instruction_to_string({sne, Dst, Src1, Src2}) ->
    indent() ++ "sne" ++ indent() ++
    commalist([reg(Dst), reg(Src1), reg(Src2)]);

instruction_to_string({sw, Src, Offset, Dst}) ->
    indent() ++ "sw" ++ indent() ++
    commalist([reg(Src), mem(Offset, Dst)]);
instruction_to_string({lw, Dst, Offset, Src}) ->
    indent() ++ "lw" ++ indent() ++
    commalist([reg(Dst), mem(Offset, Src)]);

instruction_to_string({j, Label}) ->
    indent() ++ "j" ++ indent() ++
    label_string(Label);
instruction_to_string({jr, Dst}) ->
    indent() ++ "jr" ++ indent() ++
    reg(Dst);
instruction_to_string({xxx,Str}) ->
    indent() ++ Str.

label_string({label, "main"}) ->
    "main";
label_string({label, Name}) ->
    "L" ++ Name;
label_string({label, Id, ""}) ->
    "L" ++ erlang:integer_to_list(Id);
label_string({label, Id, Name}) ->
    "L" ++ erlang:integer_to_list(Id) ++ "_" ++ Name.

char_to_int(Value) ->
    case atom_to_list(Value) of
        [$\\,$n] -> 10; % ASCII newline.
        [Int]    -> Int
    end.

commalist([X]) ->
    X;
commalist([X|Xs]) ->
    X ++ ", " ++
    commalist(Xs).

mem(Offset, Temp) ->
    erlang:integer_to_list(Offset) ++ "(" ++ reg(Temp) ++ ")".

reg(0) ->
    "$0";
reg(Temp) ->
    "$" ++ erlang:atom_to_list(Temp).

indent() ->
    "\t".

newline() ->
    "\n".
